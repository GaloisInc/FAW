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
import functools
import json
import math
import os
import pymongo
import subprocess
import time

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

app_config = None

@click.command()
@click.option('--mongo-db', type=str, required=True, help="Path to observatory "
        "database, e.g., localhost:27017/tmp-observatory")
@click.option('--pdf-dir', type=str, required=True, help="Path to folder "
        "containing PDFs.")
@click.option('--config', type=str, required=True, help="Path to JSON file "
        "specifying parsers.")
@click.option('--timeout', default=60, type=float, help="Increase execution "
        "timeout.")
@click.option('--retry-errors/--no-retry-errors', default=False,
        help="Forcibly mark documents which have errors for retry.")
@click.option('--clean/--no-clean', default=False,
        help="Trigger re-processing of all documents.")
def main(mongo_db, pdf_dir, config, clean, timeout, retry_errors):
    assert len(mongo_db.split('/')) == 2, mongo_db

    global app_config
    app_config = json.load(open(config))
    app_config['parsers'] = pdf_etl_parse.config_schema(app_config['parsers'])

    # Before running, ensure that pdf-etl-tool is built
    assert os.path.lexists('/usr/local/bin/pdf-etl-tool'), \
            'Running in environment without pdf-etl-tool installed?'
    #call(['stack', '--allow-different-user', 'build', 'pdf-etl-tools:pdf-etl-tool'],
    #        cwd='/home/pdf-etl-tools')

    # Run
    mongo_col = mongo_db + '/observatory'
    mongo_queue_helper.run(live_mode=False, processes=0, threads=2,
            clean=clean, db_src=mongo_col, db_queue=mongo_col,
            processing_timeout=timeout*1.5,
            handle_init_callback=functools.partial(db_init, mongo_db=mongo_db,
                pdf_dir=pdf_dir, retry_errors=retry_errors),
            handle_queue_callback=functools.partial(load_document,
                pdf_dir=pdf_dir, mongo_db=mongo_db, timeout=timeout,
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
        p.kill()
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


def load_document(doc, coll_resolver, pdf_dir, mongo_db, timeout,
        retry_errors):
    """
    """
    print(f'Handling {doc["_id"]}')

    # Clear previous raw invocations which timed out -- assume those which
    # did not time out were OK.  By assuming the ok-ness of those which did
    # not time out, the parsers may be re-run without re-running the tools.
    coll_resolver(mongo_db + '/rawinvocations').delete_many(
            {'file': os.path.join(pdf_dir, doc['_id']),
                'result._cons': 'Timeout'})

    # Produce raw invocations
    host_port, db = mongo_db.split('/')
    pdf_file = os.path.join(pdf_dir, doc['_id'])
    args = ['pdf-etl-tool', '-s', host_port, '-d', db,
            '-c', 'rawinvocations', 'add-raw',
            # Deliberately allow existing data which did not time out
            '--absentonly',
            '-i', 'ALL',
    ]
    # If we're retrying errors, use as much time as possible, but allow
    # timeouts to produce detectable errors rather than
    if retry_errors:
        # Note that the --timeout argument is cumulative across all invokers.
        args.extend(['--timeout', str(math.ceil(max(1, timeout - 5)))])
    else:
        # Rather than relying on the internal timeout, which might look like
        # a file didn't error out because of time, use a ridiculous value.
        # This way, timeouts will obviously show up as errors.
        args.extend(['--timeout', str(math.ceil(timeout * 2 + 5))])
    args.append(pdf_file)
    call(args,
            cwd='/home/pdf-etl-tools',
            timeout=timeout)

    # For each raw invocation, parse it.
    _load_document_parse(doc['_id'],
            tools_pdf_name=pdf_file,
            coll_resolver=coll_resolver,
            mongo_db=mongo_db)

    # For each parsed invocation, compute stats.
    _load_document_stats(doc['_id'],
            coll_resolver=coll_resolver,
            mongo_db=mongo_db)

    # Finally, attempt to translate the mechanically-filtered error messages
    # into groups using fuzzy string matches + buckets for locking protection.
    _load_document_groups(doc['_id'],
            coll_resolver=coll_resolver,
            mongo_db=mongo_db)


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
    stats_dbs = {'db_file': mongo_db + '/statsbyfile',
            'db_population': mongo_db + '/statsbypopulation'}

    # Explicitly remove the file-level statistics, since the observatory
    # decision process relies on those.
    coll_resolver(stats_dbs['db_file']).delete_one({'_id': fname})

    for parsedoc in col_parse.find({'file': fname}):
        doc = parsedoc
        dst_file = coll_resolver(stats_dbs['db_file'])
        dst_pop = coll_resolver(stats_dbs['db_population'])

        doc_file_update = {}
        doc_pop_update = {}
        parser = doc['parser'].replace('.', '_')
        doc_pop_update['$inc'] = {f'file_count_{parser}': 1}
        if 'unhandled' in doc['result']:
            raise ValueError(f'doc {doc["file"]} needs parsers re-ran: '
                    f'{doc["result"]["unhandled"]}')
        for k, v in doc['result'].items():
            # File-level, just create a doc with all keys keyed by parser name.
            pk = f'{parser}_{k}'
            pk = pk.replace('.', '_')
            doc_file_update.setdefault('$set', {})[pk] = v

            # Population level...
            # Just track number of documents with this field set, for now
            cmd = doc_pop_update.setdefault('$inc', {})
            cmd[pk] = 1
            #if isinstance(v, str):
            #    cmd = doc_pop_update.setdefault('$addToSet', {})
            #    cmd.setdefault(pk, {'$each': []})
            #    cmd[pk]['$each'].append(v)
            #elif isinstance(v, datetime.datetime):
            #    #cmd = doc_pop_update.setdefault('$addToSet', {})
            #    #cmd.setdefault(pk, {'$each': []})
            #    #cmd[pk]['$each'].append(v.strftime('%Y-%m-%d %H:%M:%S'))
            #    cmd = doc_pop_update.setdefault('$inc', {})
            #    pkv = f'{pk}_date_set'
            #    cmd[pkv] = cmd.get(pkv, 0) + 1
            #elif isinstance(v, bool):
            #    cmd = doc_pop_update.setdefault('$inc', {})
            #    pkv = f'{pk}_{v}'
            #    cmd[pkv] = cmd.get(pkv, 0) + 1
            #elif isinstance(v, (int, float)):
            #    cmd = doc_pop_update.setdefault('$inc', {})
            #    cmd[pk] = cmd.get(pk, 0) + v
            #else:
            #    raise NotImplementedError(f'{type(v)}: {repr(v)}')

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

        if doc_pop_update:
            update_with_retry(dst_pop, {'_id': 'global'}, doc_pop_update)


def _load_document_groups(fname, coll_resolver, mongo_db):
    """Populate obsgroups with fuzzy-matched list of files and
    """
    db_file = coll_resolver(mongo_db + '/statsbyfile')
    db_groups = coll_resolver(mongo_db + '/obsgroups')

    doc = db_file.find_one(fname)
    if doc is None:
        raise ValueError('No stats doc?')

    # NOTE - down the line, may want to map individual PDFs to integers for
    # efficiency on large datasets.
    doc_id = fname

    for parser_message, parser_v in doc.items():
        if parser_message.startswith('_'):
            # Reserved names
            continue

        if parser_v in (0, 0., False, None):
            # Falsey values which indicate a missing attribute.
            continue

        # Break out parser name -- use a radix for sorting / controlling
        # locks.  "Exact" fuzzy matching is not exactly efficient, so the
        # finer grain the lock, the faster it will be.
        pieces = parser_message.split('_', 1)
        assert len(pieces) == 2, pieces
        radix = '_'.join(pieces[:1]) + '_' + pieces[1][:3]
        assert parser_message.startswith(radix)

        while True:
            # Inner, abort-if-lock-changed loop
            query = {'_id': radix}
            update = {'$set': {},
                    '$addToSet': collections.defaultdict(lambda: {'$each': []})}
            rad_doc = db_groups.find_one(query)
            groups = []
            if rad_doc is None:
                # Turns into a DuplicateKeyError if no _obs_version == 0 exists
                # but it tries to insert a new one with matching _id.
                query['_obs_version'] = 0
                update['$set']['_obs_version'] = 1
            else:
                query['_obs_version'] = rad_doc['_obs_version']
                update['$set']['_obs_version'] = rad_doc['_obs_version'] + 1
                groups = rad_doc.keys()

            # Iterate through all messages in our document
            suffix = parser_message[len(radix):]
            if not suffix:
                # Radix only... put it in its own group
                update['$addToSet']['_NULL']['$each'].append(doc_id)
            else:
                # This bit used to contain code which used
                # difflib.SequenceMatcher to collapse together strings which
                # were highly related to one another.  However, this leads to
                # inherently unstable output, as the order of insert always
                # matters.  This can be seen with the idea of a threshold:
                # Assume two dissimilar messages, A and B, are added.  Now
                # assume C gets added, which is similar to both A and B.
                # Any decision, _even collapsing A, B, and C_, depends on
                # either insertion ordering OR the pdfs submitted to the
                # observatory.  That is, if message C did not exist, then A
                # and B never would have been collapsed.
                #
                # Therefore, this code is rather dull now.
                update['$addToSet'][suffix]['$each'].append(doc_id)

            try:
                db_groups.update_one(query, update, upsert=True)
                # Inserted / updated OK, so no contention.
                break
            except pymongo.errors.DuplicateKeyError:
                # Someone else finished upsert first -- we need to re-fetch
                # version.
                continue
            except:
                raise ValueError(f'Handling {radix} / {query} / {update}')


if __name__ == '__main__':
    main()

