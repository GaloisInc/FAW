"""Implements decide.py in a depth-first form.  In a separate application
from the standard PDF observatory so that we can leverage the
mongo_queue_helper code.

Note that pdf-etl-tools WILL NOT be re-run if they already exist.  This allows
the short-hand of deleting only the tmp-observatory/observatory queue
collection to trigger a parser re-run without bothering with pdf-etl-tools.
"""

import mongo_queue_helper

import click
import collections
import contextlib
from dataclasses import dataclass
import faw_pipelines_util
import functools
import json
import math
import os
import pymongo
import shutil
import subprocess
import tempfile
import time
import urllib.request

# Trickery to import pieces of pdf-etl decision pipeline
import importlib
import sys
sys.path.insert(0, os.path.abspath('../pdf-etl-parse'))
def _import_from_path(mod_name, pth):
    spec = importlib.util.spec_from_file_location(mod_name, pth)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod
pdf_etl_parse = _import_from_path('pdf_etl_parse', '../pdf-etl-parse/main.py')

app_api_info = None
app_config = None

@click.command()
@click.option('--mongo-db', type=str, required=True, help="Path to observatory "
        "database, e.g., localhost:27017/tmp-observatory")
@click.option('--pdf-dir', type=str, required=True, help="Path to folder "
        "containing PDFs; MUST MATCH between primary and worker nodes.")
@click.option('--pdf-fetch-url', type=str, help="A URL endpoint which accepts "
        "file names relative to PDF_DIR on the main host.")
@click.option('--config', type=str, required=True, help="Path to JSON file "
        "specifying parsers.")
@click.option('--retry-errors/--no-retry-errors', default=False,
        help="Forcibly mark documents which have errors for retry.")
@click.option('--clean/--no-clean', default=False,
        help="Trigger re-processing of all documents.")
@click.option('--api-info', default=None, type=str,
        help="JSON-encoded API information")
def main(mongo_db, pdf_dir, pdf_fetch_url, config, clean, retry_errors, api_info):
    assert len(mongo_db.split('/')) == 2, mongo_db

    # Clean out old invoker files first, in case there were any stragglers.
    _invokers_cleanup()

    global app_api_info, app_config
    if api_info is not None:
        app_api_info = json.loads(api_info)

    app_config = json.load(open(config))
    app_config['parsers'] = pdf_etl_parse.config_schema(app_config['parsers'])
    # When loading config, be sure to add on pipeline parsers. Needed for 
    # timeout, etc.
    for pipeline_name, pipeline_cfg in app_config['pipelines'].items():
        pipeline_parsers = pdf_etl_parse.config_schema(pipeline_cfg['parsers'])
        for parser_name, parser_cfg in pipeline_parsers.items():
            parser_cfg['pipeline'] = pipeline_name
            app_config['parsers'][f'{pipeline_name}--{parser_name}'] = parser_cfg

    timeout_default = app_config['parserDefaultTimeout']
    timeout_total = sum([p['timeout'] or timeout_default
            for p in app_config['parsers'].values()])

    # Before running, ensure that pdf-etl-tool is built
    assert os.path.lexists('/usr/local/bin/pdf-etl-tool'), \
            'Running in environment without pdf-etl-tool installed?'
    #call(['stack', '--allow-different-user', 'build', 'pdf-etl-tools:pdf-etl-tool'],
    #        cwd='/home/pdf-etl-tools')

    # Run
    mongo_col = mongo_db + '/observatory'
    init_fn = None
    if pdf_fetch_url is None:
        # A local folder; indicative of main FAW
        live_mode = False
        init_fn = functools.partial(db_init, mongo_db=mongo_db,
                pdf_dir=pdf_dir, retry_errors=retry_errors)
        @contextlib.contextmanager
        def pdf_getter(fpath):
            yield os.path.join(pdf_dir, fpath)
    else:
        # A network path; indicative of worker FAW
        live_mode = True
        try:
            os.makedirs(pdf_dir)
        except FileExistsError:
            pass

        @contextlib.contextmanager
        def pdf_getter(fpath):
            # Importantly, the absolute path MUST match that of the host. So,
            # use the same pdf_dir.
            fpath_full = os.path.join(pdf_dir, fpath)
            try:
                os.makedirs(os.path.dirname(fpath_full))
            except FileExistsError:
                pass

            with open(fpath_full, 'wb') as f:
                with urllib.request.urlopen(pdf_fetch_url + fpath) as response:
                    shutil.copyfileobj(response, f)
            try:
                yield fpath_full
            finally:
                os.unlink(fpath_full)
    mongo_queue_helper.run(live_mode=live_mode, processes=0, threads=2,
            clean=clean, db_src=mongo_col, db_queue=mongo_col,
            processing_timeout=timeout_total*1.5,
            handle_init_callback=init_fn,
            handle_queue_callback=functools.partial(load_document,
                pdf_getter=pdf_getter, mongo_db=mongo_db,
                retry_errors=retry_errors),
            # PDF observatory can show exceptions
            allow_exceptions=True)


def call(args, cwd=None, input=None, timeout=None):
    p = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            stdin=subprocess.PIPE, cwd=cwd)
    if input is not None:
        input = input.encode()
    try:
        stdout, stderr = p.communicate(input=input, timeout=timeout)
    except subprocess.TimeoutExpired:
        p.terminate()
        stdout, stderr = p.communicate()
        stdout = stdout.decode()
        stderr = stderr.decode()
        raise ValueError(f'While running {args}, timed out\n\nstdout: {stdout}\n\nstderr: {stderr}')
    stdout = stdout.decode()
    stderr = stderr.decode()
    if p.wait() != 0:
        raise ValueError(f'Program {args} returned: {p.wait()}\n\nstdout: {stdout}\n\nstderr: {stderr}')
    return stdout


def db_init(coll_resolver, mongo_db, pdf_dir, retry_errors):
    """Creates indices for database.
    """
    coll = coll_resolver(mongo_db + '/rawinvocations')
    coll.create_index([('file', pymongo.ASCENDING)])
    coll.create_index([('result._cons', pymongo.ASCENDING)])

    coll = coll_resolver(mongo_db + '/invocationsparsed')
    coll.create_index([('file', pymongo.ASCENDING)])

    if retry_errors:
        coll = coll_resolver(mongo_db + '/observatory')
        # Retry any queue documents which were marked as erroneous.
        coll.update_many({'queueErr': {'$ne': None}}, {'$set': {
                'queueErr': None, 'queueStart': None, 'queueStop': None}})

        if True:
            # Also find any "Timeout" results, and retry those documents.
            coll_invoc = coll_resolver(mongo_db + '/rawinvocations')
            docs_to_retry = set()
            for doc in coll_invoc.find({'result._cons': 'Timeout'}):
                file_id = os.path.relpath(doc['file'], pdf_dir)
                docs_to_retry.add(file_id)
            if docs_to_retry:
                coll.update_many({'_id': {'$in': list(docs_to_retry)}},
                        {'$set': {
                            'queueErr': None, 'queueStart': None,
                            'queueStop': None}})


def load_document(doc, coll_resolver, pdf_getter, mongo_db, retry_errors):
    """
    """
    print(f'Handling {doc["_id"]}')

    fpath = doc['_id']
    with pdf_getter(fpath) as fpath_access:
        return _load_document_inner(doc, coll_resolver, fpath_access,
                mongo_db, retry_errors)


def _load_document_inner(doc, coll_resolver, fpath_access, mongo_db,
        retry_errors):
    db_coll = coll_resolver(mongo_db + '/rawinvocations')
    
    # Clear previous raw invocations which timed out -- assume those which
    # did not time out were OK.  By assuming the ok-ness of those which did
    # not time out, the parsers may be re-run without re-running the tools.
    delete_or_clause = []
    if retry_errors:
        delete_or_clause.append({'file': fpath_access, 'result._cons': 'Timeout'})
    # Clear previous raw invocations whose versions do not match the expected
    # tool versions.
    invokers_whitelist = []
    invokers_config_files = {}
    for k, v in app_config['parsers'].items():
        if v.get('disabled'):
            continue
        invoker_cfg, invoker_version = _invokers_build_cfg(k, v, 
                db_conn=db_coll.database)
        invokers_config_files[k] = invoker_cfg

        invokers_whitelist.append(k)
        delete_or_clause.append({'file': fpath_access, 'invoker.invName': k,
                'invoker.version': {'$ne': invoker_version}})
    # Used to clear out old parser data. However... since private distributions,
    # e.g. `./workbench.py ../modified-pdf ...`, it doesn't really make sense
    # to clear out the old information on the off chance it will be re-used.
    # Since that's the main situation in which this deletion would be triggered,
    # it doesn't make sense for deleting extraneous parser information to be the
    # default behavior.
    #delete_or_clause.append({'file': fpath_access,
    #        'invoker.invName': {'$nin': invokers_whitelist}})

    # Run delete
    if delete_or_clause:
        if len(delete_or_clause) == 1:
            query = delete_or_clause[0]
        else:
            query = {'$or': delete_or_clause}
        db_coll.delete_many(query)

    invs_seen = [r['invoker']['invName']
            for r in db_coll.find({'file': fpath_access}, {'invoker.invName': True})]

    # Produce raw invocations
    host_port, db = mongo_db.split('/')
    args = ['pdf-etl-tool', '-s', host_port, '-d', db,
            '-c', 'rawinvocations',
    ]

    # This used to rely on `-i ALL` behavior of pdf-etl-tool, but the spin-up
    # was very slow. Faster to do this.
    inv_left = len(set(invokers_whitelist).difference(invs_seen))
    timeout_default = app_config['parserDefaultTimeout']
    for k, v in app_config['parsers'].items():
        if v.get('disabled') or k in invs_seen:
            continue
        aargs = args[:]

        tool_timeout = v['timeout'] or timeout_default

        # Build an invokers file for this parser
        aargs.extend(['--invokersfile', invokers_config_files[k]])
        aargs.extend([
            'add-raw',
            # Deliberately allow existing data which did not time out
            # Removed because we now query `invs_seen` beforehand.
            #'--absentonly',
        ])

        # Use as much time as possible, but allow timeouts to produce detectable
        # errors rather than time out.
        aargs.extend(['--timeout', str(math.ceil(max(1, tool_timeout - 1)))])

        # Rely on pdf-etl-tools' timeout. If using our own, it would show up as
        # a DB error. This is somewhat antiquated code.
        tool_timeout = None

        aargs.extend(['-i', k, fpath_access])
        call(aargs,
                cwd='/home/dist',
                timeout=tool_timeout)

    # For each raw invocation, parse it.
    _load_document_parse(doc['_id'],
            tools_pdf_name=fpath_access,
            coll_resolver=coll_resolver,
            mongo_db=mongo_db)

    # For each parsed invocation, compute stats.
    _load_document_stats(doc['_id'],
            coll_resolver=coll_resolver,
            mongo_db=mongo_db)

    # We used to then pre-group files into sets keyed by error message. However,
    # that index wasn't used, and the current UI requires a full table scan
    # anyway. Furthermore, that collection resulted in mongo documents greater
    # than 16 megabytes, causing errors. Therefore, it has been removed.


_invokers_folder = '/dev/shm/faw-invokers'
def _invokers_cleanup():
    try:
        # Use shared memory -- these are many small files, no need to tax the
        # disk at all.
        shutil.rmtree(_invokers_folder)
    except FileNotFoundError:
        pass
    os.mkdir(_invokers_folder)


_invokers_files = {}
@dataclass
class _InvokerCfg:
    file: tempfile.NamedTemporaryFile
    version: str
def _invokers_build_cfg(inv_name, inv_config, db_conn):
    """Returns (file path, invoker version)
    """
    # Build invokers.cfg based on specified invokers.

    api_info = app_api_info.copy()
    if 'pipeline' in inv_config:
        # Special case -- need to fetch task versions
        api_info['pipeline'] = inv_config['pipeline']
        
        api = faw_pipelines_util.Api(api_info, db_conn=db_conn)

        versions = [inv_config['version']]
        for task_name, task_cfg in app_config['pipelines'][inv_config['pipeline']]['tasks'].items():
            versions.append(str(api.task_get_state(task_name)))
        version = '__'.join(versions)
    else:
        # Standard, hardcoded
        version = inv_config['version']
    key = f'{inv_name}-{version}'
    cfg = _invokers_files.get(key)
    if cfg is not None:
        return cfg.file.name, cfg.version

    # A new config -- TODO clear out old

    inv_file = tempfile.NamedTemporaryFile(dir=_invokers_folder,
            mode='w+')
    lines = []
    lines.append('[')

    assert inv_config.get('exec'), inv_name

    def exec_encode(v):
        if v.startswith('<tempFile'):
            suffix = v[9:-1]
            if suffix:
                assert suffix[0] == ' ', suffix
                assert ' ' not in suffix[1:], suffix
                suffix = suffix[1:]
                assert '"' not in suffix, suffix
            return f'TmpFN "tmpfile{suffix}"'
        elif v.startswith('<'):
            return {
                    '<apiInfo>': lambda: f'Str {json.dumps(json.dumps(api_info))}',
                    '<inputFile>': lambda: 'InputFile',
            }[v]()

        # Haskell requires double quote delimiters, which JSON works with
        vrepr = json.dumps(v)
        return f'Str {vrepr}'

    lines.append(f'Invoker')
    exec_args = ', '.join([exec_encode(a) for a in inv_config['exec']])
    timeout = 20  # We no longer use this feature, so fix it to a large value
    lines.append(f'  {{ exec = [ {exec_args} ]')
    lines.append(f'  , timeoutScale = Just {timeout}')
    lines.append(f'  , version = "{version}"')
    lines.append(f'  , invName = "{inv_name}"')
    lines.append(f'  }}')
    lines.append(']')
    inv_file.write('\n'.join(lines))
    inv_file.flush()

    # Finally, assign our result to keep it in cache
    _invokers_files[key] = _InvokerCfg(file=inv_file, version=version)
    return inv_file.name, version


def _load_document_parse(fname, tools_pdf_name, coll_resolver, mongo_db):
    """Looks at all tool invocation outputs for a given file and produces the
    parsed versions.
    """

    # Since pdf-etl-tools re-creates documents, we can validate this step by
    # first getting document IDs corresponding to the given file, and then
    # checking if those same IDs exist in the parser db.
    col_tools = coll_resolver(mongo_db + '/rawinvocations')
    col_parse_name = mongo_db + '/invocationsparsed'
    col_parse = coll_resolver(col_parse_name)

    # Parser is pretty fast - OK to delete all prior work and try again.
    for parsedoc in col_parse.find({'file': fname}):
        col_parse.delete_one({'_id': parsedoc['_id']})

    tooldocs = set()
    for tooldoc in col_tools.find({'file': tools_pdf_name}):
        tooldocs.add(tooldoc['_id'])

        # How one would write a check which re-uses past work.
        if False and col_parse.find_one({'_id': tooldoc['_id']}) is not None:
            # Already computed
            continue

        pdf_etl_parse.handle_doc(tooldoc, coll_resolver, fname_rewrite=fname,
                db_dst=col_parse_name,
                parsers_config=app_config['parsers'])

    if len(tooldocs) == 0:
        raise ValueError("No successful tool invocations found?")


def _load_document_stats(fname, coll_resolver, mongo_db):
    """Aggregate all stats acording to the decision process."""
    col_parse = coll_resolver(mongo_db + '/invocationsparsed')
    stats_dbs = {'db_file': mongo_db + '/statsbyfile'}

    # Explicitly remove the file-level statistics, since the observatory
    # decision process relies on those.
    coll_resolver(stats_dbs['db_file']).delete_one({'_id': fname})

    for parsedoc in col_parse.find({'file': fname}):
        doc = parsedoc
        dst_file = coll_resolver(stats_dbs['db_file'])

        doc_file_update = {}
        parser = doc['parser'].replace('.', '_')
        if 'unhandled' in doc['result']:
            raise ValueError(f'doc {doc["file"]} needs parsers re-ran: '
                    f'{doc["result"]["unhandled"]}')
        for k, v in doc['result'].items():
            # File-level, just create a doc with all keys keyed by parser name.
            pk = f'{parser}_{k}'
            pk = pk.replace('.', '_')
            doc_file_update.setdefault('$set', {})[pk] = v

        def update_with_retry(db, query, update):
            ntrial = 5
            for trial in range(ntrial):
                try:
                    db.update_one(query, update, upsert=True)
                    return
                except pymongo.errors.DuplicateKeyError:
                    # Broke from update to upsert, but someone else finished the
                    # upsert first.  Retry!
                    if trial == ntrial - 1:
                        raise
                except:
                    raise ValueError(update)

        if doc_file_update:
            update_with_retry(dst_file, {'_id': doc['file']}, doc_file_update)


if __name__ == '__main__':
    main()

