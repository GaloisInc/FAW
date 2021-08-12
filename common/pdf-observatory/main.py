
import app_util
import faw_analysis_set
import faw_analysis_set_util
import faw_pipelines_util

import aiohttp.web as web
import asyncio
import bson
import click
import collections
import contextlib
import functools
import importlib.util
import ujson as json
import math
import motor.motor_asyncio
import os
import pickle
import psutil
import pymongo
import pympler.asizeof as asizeof
import re
import shlex
import strictyaml
import sys
import tempfile
import time
import traceback
import vuespa

app_config = None
app_config_loaded = None
app_config_path = None
app_docker = False
app_hostname = None
app_hostport = None

app_init = None
app_mongodb = None
app_mongodb_conn = None
app_pdf_dir = None

etl_path = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..')

@click.command()
@click.argument('pdf-dir')
@click.argument('mongodb')
@click.option('--host', type=str, default=None)
@click.option('--port', type=int, default=None)
@click.option('--hostname', type=str, default=None, help="Used for teaming "
        "deployments; specifies the hostname passed for <apiInfo>")
@click.option('--in-docker/--not-in-docker', default=False,
        help="Must specify if running in docker.")
@click.option('--production/--no-production', default=False,
        help="Specify to use pre-built version of UI, rather than building "
            "on the fly and using Vue's hot reload functionality.")
@click.option('--config', type=str, required=True,
        help="(Required) Path to .json defining this observatory deployment.")
@click.option('--quit-after-config/--no-quit-after-config', default=False)
def main(pdf_dir, mongodb, host, port, hostname, in_docker, production, config,
        quit_after_config):
    """Run the PDF observatory on the given mongodb instance and database,
    providing a UI.

    Args:
        PDF_DIR: Directory containing PDF files which should be observed.
        MONGODB: e.g. localhost:27017/observe or just "observe" - server, port,
                and database in which to store pdf observatory results.
    """

    global app_config, app_config_path, app_docker, app_hostname, app_hostport, \
            app_init, app_mongodb, app_mongodb_conn, app_pdf_dir

    assert in_docker, 'Config specifying parsers must be in docker'

    app_config_path = config
    app_hostname = hostname if hostname is not None else 'localhost'
    app_hostport = port
    assert hostname is None or port is not None, 'Must specify port with hostname'

    app_pdf_dir = os.path.abspath(pdf_dir)
    if '/' in mongodb:
        app_mongodb = mongodb
    else:
        app_mongodb = 'localhost:27017/' + mongodb

    _config_reload()

    if quit_after_config:
        return

    app_docker = in_docker

    mhost_port, db = app_mongodb.split('/')
    mhost, mport = mhost_port.split(':')
    app_mongodb_conn = motor.motor_asyncio.AsyncIOMotorClient(host=mhost,
            port=int(mport))[db]

    loop = asyncio.get_event_loop()
    app_config_refresh = loop.create_task(_config_check_loop())
    app_init = loop.create_task(init_check_pdfs())
    loop.create_task(faw_analysis_set.main_loop(app_mongodb_conn,
            app_config, _get_api_info))
    vuespa.VueSpa('ui', Client, host=host, port=port,
            development=not production,
            config_web_callback=functools.partial(config_web, pdf_dir=pdf_dir)
            ).run()


def config_web(app, pdf_dir):
    """Add an endpoint for direct downloading of files.
    """
    app.router.add_routes([
            web.post('/db_reparse', _config_web_db_reparse_handler),
            web.static('/file_download', pdf_dir),
            web.get('/file_list', _config_web_file_list_handler),
    ])


async def _config_web_db_reparse_handler(req):
    """Reparse, looking at submission packet for tools to reset.
    """
    req_json = await req.json()
    await _db_reparse(req_json['tools_to_reset'])
    return web.Response(text='{}')


async def _config_web_file_list_handler(req):
    file_list = list(_walk_pdf_files())
    return web.Response(text='\n'.join(file_list))


async def _config_check_loop():
    while True:
        try:
            ts = os.path.getmtime(app_config_path)
            if ts != app_config_loaded:
                # Note -- various system/docker configs can result in
                # development overwrites being in the past. So use not equal.
                _config_reload()
        except:
            traceback.print_exc()

        await asyncio.sleep(0.5)



def _config_reload():
    global app_config, app_config_loaded, app_config_path

    ts_loaded = os.path.getmtime(app_config_path)
    app_config = app_util.config_load(app_config_path)
    faw_analysis_set.config_update(app_config)

    # Kick dask...
    for p in psutil.process_iter():
        if p.name() in ['dask-worker', 'dask-scheduler']:
            p.kill()

    # Set at end of method, to get around any async / threading issues
    app_config_loaded = ts_loaded


_app_parser_sizetable = {}
async def _app_parser_stats():
    """Retrieve / update cached parser stat information.

    Fields:
        id: identifier of parser
        size_doc: approximate size (bytes) per document or null
    """

    parser_cfg = faw_analysis_set_util.lookup_all_parsers(
            app_mongodb_conn.delegate, app_config)

    parsers = []
    promises = []
    for k, v in parser_cfg.items():
        if v.get('disabled'):
            continue
        parser = {
            'id': k,
            'size_doc': None,
            'pipeline': True if v.get('pipeline') else False,
        }
        parsers.append(parser)

        r = _app_parser_sizetable.get(k)
        if r is not None and r[1] > time.monotonic():
            parser['size_doc'] = r[0]
        else:
            async def stat_pop(k, parser):
                ndocs = 5
                # Only include successful runs
                docs = await app_mongodb_conn['invocationsparsed'].find({
                        'parser': k,
                        'exitcode': 0}).limit(ndocs).to_list(None)
                size = None
                if len(docs) > 1:
                    # Size is actually a differential. Indicates information
                    # added per additional document due to unique features.
                    size = 0
                    fts_size = set([dr['k'] for dr in docs[0]['result']])
                    for d in docs[1:]:
                        fts_new = set([dr['k'] for dr in d['result']])
                        fts_new.difference_update(fts_size)
                        size += len(pickle.dumps(fts_new))
                        fts_size.update(fts_new)
                    size /= len(docs) - 1

                if len(docs) == ndocs:
                    # Keep result for a long time
                    r = [size, time.monotonic() + 600]
                else:
                    # Let it load, don't hammer the DB
                    r = [size, time.monotonic() + 30]

                _app_parser_sizetable[k] = r
                parser['size_doc'] = r[0]
            promises.append(stat_pop(k, parser))
    if promises:
        await asyncio.wait(promises)
    return parsers

def _get_api_info(extra_info={}):
    """NOTE: Updates here must also affect `common/teaming/pyinfra/deploy.py`,
    where the worker script gets written. look for "--api-info" in that file.
    """
    r = {
        'hostname': app_hostname,
        'hostport': app_hostport,
        'dask': f'{app_hostname}:8786',
        'mongo': app_mongodb,
        'pdfdir': app_pdf_dir,
    }
    r.update(extra_info)
    return r


def _plugin_key_process(config_key, plugin_key):
    """Plugin keys for non-pipeline plugins don't have exclamation points.
    So, use those to separate them. Then, return plugin definition and extra
    api args.
    """
    extra_api_info = {}
    if '!' in plugin_key:
        # Pipeline
        aset_id, pipeline, plugin_key = plugin_key.split('!')
        extra_api_info['aset'] = aset_id
        extra_api_info['pipeline'] = pipeline

        plugin_def = app_config['pipelines'][pipeline][config_key].get(plugin_key)
        if plugin_def is None:
            raise ValueError(f'{config_key}: {aset_id} / {pipeline} / {plugin_key} not found')
    else:
        plugin_def = app_config[config_key].get(plugin_key)
        if plugin_def is None:
            raise ValueError(f'{config_key}: {plugin_key} not found')

    return plugin_def, extra_api_info


def _walk_pdf_files():
    """Walk through all non-hidden files in the app's file folder.
    """
    for path, subfolders, files in os.walk(app_pdf_dir):
        # Filter subsequent subfolders to not go into hidden directories
        for i in range(len(subfolders)-1, -1, -1):
            if subfolders[i].startswith('.'):
                subfolders.pop(i)

        for f in files:
            if f.startswith('.'):
                continue

            ff = os.path.relpath(os.path.join(path, f), app_pdf_dir)
            yield ff


async def _db_reparse(tools_to_reset=[]):
    """Special function to trigger a reset within an async handler.
    """
    _db_abort_process()
    await app_mongodb_conn.drop_collection('observatory')
    if tools_to_reset:
        await app_mongodb_conn['rawinvocations'].delete_many(
                {'invoker.invName': {'$in': tools_to_reset}})
    _db_reprocess()


def _db_reprocess(*args, **kwargs):
    global app_init
    loop = asyncio.get_event_loop()
    app_init = loop.create_task(init_check_pdfs(*args, **kwargs))


class _DbLoaderProc:
    def __init__(self):
        self.proc = None
        self.aborted = False
    def abort(self):
        self.aborted = True
        if self.proc is None:
            return
        try:
            self.proc.kill()
        except ProcessLookupError:
            # Already finished, OK
            pass
_db_loader_proc = _DbLoaderProc()
def _db_abort_process():
    """Returns new `_DbLoaderProc`"""
    global _db_loader_proc

    # Try to abort old `init_check_pdfs` call.
    _db_loader_proc.abort()
    _db_loader_proc = _DbLoaderProc()
    return _db_loader_proc
async def init_check_pdfs(retry_errors=False):
    """Check the database to see what's populated and what's not.

    Additionally, control the flow of the various pdf-etl tools for on-demand
    import of population.

    Ran on program init and on DB reset via UI.
    """

    loader_proc = _db_abort_process()

    db = app_mongodb_conn
    col = db['observatory']

    # Create additional indices... there's already one on queueStart w/
    # queueStop == None
    await asyncio.wait([
        col.create_index([('queueErr', pymongo.ASCENDING)]),
        # Partial index isn't used for count, unfortunately.
        col.create_index([('queueStop', pymongo.ASCENDING)]),
    ])

    # Use a batch to support parallelism on mongodb's side
    batch = set()
    batch_max = 100
    async def insert_or_ignore(fpath):
        try:
            await col.insert_one({'_id': fpath, 'queueStart': None,
                    'queueStop': None, 'queueErr': None})
        except pymongo.errors.DuplicateKeyError:
            pass
    for ff in _walk_pdf_files():
        if loader_proc.aborted:
            # User re-triggered this step, so stop processing.
            return

        batch.add(insert_or_ignore(ff))
        if len(batch) > batch_max:
            _, batch = await asyncio.wait(batch,
                    return_when=asyncio.FIRST_COMPLETED)
    await asyncio.wait(batch)

    # Now that all PDFs are guaranteed queued, run a queue helper which does
    # depth-first processing of all files
    oargs = []
    if retry_errors:
        oargs = ['--retry-errors']
    try:
        proc = await asyncio.create_subprocess_exec(
                'python3', '../pdf-observatory/queue_client.py',
                '--mongo-db', app_mongodb, '--pdf-dir', app_pdf_dir,
                '--config', os.path.abspath(app_config_path),
                '--api-info', json.dumps(_get_api_info()),
                *oargs,
                cwd=os.path.join(etl_path, 'dist'),
        )
        loader_proc.proc = proc
        await proc.communicate()
        if await proc.wait() != 0:
            raise ValueError("non-zero exit")
    except:
        if loader_proc.aborted:
            # OK if aborted; we were expecting some failure.
            return

        traceback.print_exc()
        # No point in continuing, fatal error.  Use os._exit to avoid
        # SystemExit exception, which won't be caught by UI.
        os._exit(1)


class Client(vuespa.Client):
    async def vuespa_on_open(self):
        pass


    async def vuespa_on_close(self):
        pass


    async def api_analysis_set_ft_count(self, ft_def):
        """Returns count of matching documents for AsFeature query.
        """
        regex = re.compile('^' + ft_def.get('ft', ''),
                flags=0 if ft_def.get('ft_case') else re.I)
        c = await app_mongodb_conn['invocationsparsed'].count_documents({
                'parser': ft_def.get('parser', ''),
                'result.k': regex,
        })
        return c


    async def api_analysis_set_data(self):
        """Returns object like AsStatus from ui/src/components/AnalysisSetConfig.vue"""
        parsers = await _app_parser_stats()
        return {
                'asets': await faw_analysis_set.as_list(),
                'parsers': parsers,
        }


    async def api_analysis_set_pipeline_start(self, id, pipeline):
        await faw_analysis_set.as_pipeline_start(id, pipeline)


    async def api_analysis_set_pipeline_delete(self, id, pipeline):
        await faw_analysis_set.as_pipeline_delete(id, pipeline)


    async def api_analysis_set_update(self, cfg):
        """Takes object like AsStatus from ui/src/components/AnalysisSetConfig.vue"""
        await faw_analysis_set.as_update(id=cfg['id'],
                definition=cfg['definition'])


    async def api_analysis_set_delete(self, id):
        """Takes analysis set ID"""
        await faw_analysis_set.as_delete(id)


    async def api_config_get(self):
        """Translate for UI before returning config.
        """
        return app_config


    async def api_config_plugin_run(self, plugin_key, vuespa_url, json_args,
            input_spec):

        plugin_def, extra_api_info = _plugin_key_process('file_detail_views',
                plugin_key)

        if plugin_def['type'] == 'program_to_html':
            assert isinstance(input_spec, str), input_spec
            prefix = '/home/pdf-files/'
            input_spec = prefix + input_spec
            assert os.path.lexists(input_spec), input_spec

            cmd = plugin_def.get('exec')
            assert cmd is not None, cmd

            file_out = None
            def get_output_html():
                nonlocal file_out
                assert file_out is None, 'Cannot use <outputHtml> twice'
                file_out = tempfile.NamedTemporaryFile(delete=False,
                        suffix='.html')
                file_out.close()
                return file_out.name
            cmd = self._cmd_plugin_template_replace(cmd, vuespa_url, {
                    '<inputFile>': lambda: input_spec,
                    '<jsonArguments>': lambda: json.dumps(json_args),
                    '<outputHtml>': get_output_html,
            }, extra_api_info=extra_api_info)

            try:
                proc = await asyncio.create_subprocess_exec(
                        *cmd,
                        cwd=os.path.join(etl_path, 'dist') + '/' + plugin_def['cwd'],
                        stdout=asyncio.subprocess.PIPE,
                        stderr=asyncio.subprocess.PIPE,
                )
                stdout, stderr = await proc.communicate()
                if await proc.wait() != 0:
                    raise ValueError(f"non-zero exit: {stderr.decode('latin1')}")

                r = {'mimetype': plugin_def['outputMimeType']}
                if file_out is None:
                    r['result'] = stdout.decode('latin1')
                else:
                    with open(file_out.name, 'rb') as f:
                        r['result'] = f.read().decode('latin1')

                return r
            finally:
                if file_out is not None:
                    os.unlink(file_out.name)
        else:
            raise NotImplementedError(plugin_def['type'])


    async def api_config_plugin_dec_run(self, plugin_key, api_url, json_args,
            reference_decisions, subset_options):
        """Runs a decision plugin.
        """
        timer = _Timer()

        output_html = None
        def get_output_html():
            nonlocal output_html
            assert output_html is None, 'Cannot use <outputHtml> twice'
            output_html = tempfile.NamedTemporaryFile(delete=False,
                    suffix='.html')
            output_html.close()
            return output_html.name

        plugin_def, extra_api_info = _plugin_key_process('decision_views',
                plugin_key)

        try:
            if plugin_def['type'] == 'program':
                cmd = plugin_def.get('exec')
                assert cmd is not None, 'exec'
                cmd = self._cmd_plugin_template_replace(cmd, api_url, {
                        '<jsonArguments>': lambda: json.dumps(json_args),
                        '<outputHtml>': get_output_html,
                }, extra_api_info=extra_api_info)
                proc = await asyncio.create_subprocess_exec(
                        *cmd,
                        cwd=os.path.join(etl_path, 'dist') + '/' + plugin_def['cwd'],
                        stdin=asyncio.subprocess.PIPE,
                        stdout=asyncio.subprocess.PIPE,
                        stderr=asyncio.subprocess.PIPE,

                        # Important, otherwise the read process will freeze when
                        # a sufficiently large record gets written...
                        limit=1e7,
                )
                result = {'html': None, 'decisions': []}
                d = result['decisions']
                # Track stderr to forward to user interface
                stderr = []
                async def read_out(s):
                    async for line in s:
                        line = line.decode('utf-8')
                        # Ignore non-JSON lines
                        if not line.startswith('{'):
                            continue
                        d.append(json.loads(line))
                async def read_err(s):
                    # FIXME long lines (print statement in pdf/vue_plugin/main.py)
                    # will drop the end of their content. This _may_ affect
                    # stdout, which would be a bigger problem.
                    async for line in s:
                        line = line.decode('utf-8')
                        stderr.append(line)
                        print(f'{plugin_key}: {line}', file=sys.stderr, end='')
                async def write_in(s):
                    try:
                        template = plugin_def.get('execStdin')
                        if template is None:
                            return

                        for part in re.split('(<[^>]+>)', template):
                            if not part.startswith('<'):
                                s.write(part.encode('utf-8'))
                                await s.drain()
                            elif part == '<referenceDecisions>':
                                with timer.save('stream references'):
                                    # Optimize...
                                    bytes_written = 0
                                    for q in reference_decisions:
                                        if bytes_written > 500e6:
                                            await s.drain()
                                            bytes_written = 0
                                        a = json.dumps(q).encode('utf-8')
                                        s.write(a)
                                        s.write(b'\n')
                                        bytes_written += len(a) + 1
                                    await s.drain()
                            elif part == '<statsbyfile>':
                                with timer.save('stream file stats'):
                                    # optimization, saves ~16% on large
                                    # collections
                                    bytes_written = 0
                                    async for dd in self._statsbyfile_cursor(
                                            subset_options):
                                        for d in dd:
                                            if bytes_written > 500e6:
                                                await s.drain()
                                                bytes_written = 0
                                            a = json.dumps(d).encode('utf-8')
                                            s.write(a)
                                            s.write(b'\n')
                                            bytes_written += len(a) + 1
                                    await s.drain()
                            else:
                                raise NotImplementedError(part)
                    finally:
                        s.close()

                with timer.save('subprocess'):
                    r = await asyncio.gather(
                            read_out(proc.stdout),
                            read_err(proc.stderr),
                            write_in(proc.stdin),
                            # Read all of stdout and stderr, even if stdin
                            # crashes.
                            return_exceptions=True,
                    )
                    exit_code = await proc.wait()
                    for exc in r:
                        if isinstance(exc, Exception):
                            traceback.print_exception(type(exc), exc,
                                    exc.__traceback__)

                if exit_code != 0:
                    # Gets forwarded to user, hence desire for duplicating
                    # stderr
                    raise ValueError(f'non-zero exit: {plugin_key}:\n{"".join(stderr)}')

                with timer.save('transcribe output'):
                    if output_html is not None:
                        result['html'] = open(output_html.name, 'rb').read().decode(
                                'latin1')

                result['debug'] = {
                        'profile': timer.records,
                }
                return result
            else:
                raise NotImplementedError(plugin_def['type'])
        finally:
            if output_html is not None:
                os.unlink(output_html.name)


    async def api_decisions_get(self, options):
        """
        Args:
            options: {subsetSize, subsetRegex, subsetPartition}.
        """
        groups = {}
        files = []
        r = {'groups': groups, 'files': files}

        start = time.monotonic()
        print(f'Loading decisions for {options}')
        cursor = self._statsbyfile_cursor(options)
        async for gg in cursor:
            for g in gg:
                gid = len(files)
                files.append(g['_id'])
                for k, v in g.items():
                    if k.startswith('_'): continue
                    grp = groups.get(k)
                    if grp is None:
                        groups[k] = grp = []
                    grp.append([gid, v])

        print(f'...Decisions packaged in {time.monotonic() - start:.2f}s')

        return r


    def _cmd_plugin_template_replace(self, cmd, api_url, extra_template_vars,
            extra_api_info):
        if not api_url.endswith('/'):
            api_url = api_url+ '/'

        template_vals = {
                '<apiInfo>': lambda: json.dumps(_get_api_info(extra_api_info)),
                '<filesPath>': lambda: app_pdf_dir,
                '<mongo>': lambda: app_mongodb,
                '<workbenchApiUrl>': lambda: api_url,
        }
        for k, v in extra_template_vars.items():
            if k in template_vals:
                raise ValueError(f'Cannot overwrite {k}')
            template_vals[k] = v

        r = []
        for c in cmd:
            if c.startswith('<'):
                rr = template_vals.get(c)
                if rr is None:
                    raise ValueError(c)
                r.append(rr())
            else:
                r.append(c)
        return r


    async def _statsbyfile_cursor(self, options, match_id=None):
        """Async generator which yields from `statsbyfile` according to the
        working subsetting options in `options`.

        For efficiency, yields batches rather than individual documents! This
        cuts overhead by as much as 50% due to the way async works in python.

        Args:
            options:
                analysis_set_id: Analysis set to use for returning results
                file_ids?: Optional list of file IDs to restrict results.
            match_id: Document id to return.
        """

        assert 'analysis_set_id' in options
        cursor_db = app_mongodb_conn['as_c_' + options['analysis_set_id']]
        # Faster to convert from new format to old inline.
        # Goes from 50s down to 28s at 100k documents.
        pipeline = [
                {'$replaceRoot': {'newRoot': {'$mergeObjects': [
                    {'_id': '$_id'},
                    {'$arrayToObject': '$f'},
                ]}}},
        ]
        if options.get('file_ids'):
            pipeline.insert(0, {'$match': {'_id': {'$in': options['file_ids']}}})
        if match_id is not None:
            pipeline.insert(0, {'$match': {'_id': match_id}})
        cursor = cursor_db.aggregate(pipeline)

        doc_sz = 0
        doc_sz_n = 0

        while True:
            n_fetch = 10 if doc_sz == 0 else 1 + int(128e6 / doc_sz)
            docs = await cursor.to_list(n_fetch)
            if not docs:
                break
            yield docs

            if doc_sz == 0:
                doc_sz = asizeof.asizeof(docs) / len(docs)

            # Manual conversion
            #g_new = {'_id': g['_id']}
            #for o in g['f']:
            #    g_new[o['k']] = o['v']
            #yield g_new


    async def api_clear_db(self):
        _db_abort_process()
        await app_mongodb_conn.client.drop_database(app_mongodb_conn.name)
        _db_reprocess()


    async def api_reparse_db(self):
        await _db_reparse()


    async def api_reset_db_errors(self):
        _db_reprocess(retry_errors=True)


    async def api_loading_get(self, options):
        """Returns an object with {loading: boolean, message: string}.

        options: {}
        """
        col = app_mongodb_conn['observatory']
        query = {}

        if options:
            raise ValueError(options)

        pdfs_max = await col.count_documents(query)
        pdfs_not_done = await col.count_documents(dict(**query,
                **{'queueStop': {'$type': 10}}))
        pdfs_not_err = await col.count_documents(dict(**query,
                **{'queueErr': {'$type': 10}}))
        return {
                'config_mtime': app_config_loaded,
                'files_done': pdfs_max - pdfs_not_done,
                'files_max': pdfs_max,
                'files_err': pdfs_max - pdfs_not_err,
                'message': f'{pdfs_max - pdfs_not_done} / {pdfs_max}; {pdfs_max - pdfs_not_err} errors',
        }


    async def api_load_db(self, pdf, collection, options=None):
        """Loads a specific entry from the pdf-etl database, for manual
        inspection.
        """
        query = None
        postproc = None  # In-place post processor
        cursor_batched = False
        if collection == 'rawinvocations':
            query = {'file': os.path.join(app_pdf_dir, pdf)}
            cursor = app_mongodb_conn[collection].find(query)
        elif collection == 'invocationsparsed':
            query = {'file': pdf}
            cursor = app_mongodb_conn[collection].find(query)
            def postproc(d):
                d['result'] = {dr['k']: dr['v'] for dr in d['result']}
        elif collection == 'statsbyfile':
            cursor = self._statsbyfile_cursor(options, match_id=pdf)
            cursor_batched = True
            options = None
        else:
            raise NotImplementedError(collection)

        assert options is None

        docs = [d async for d in cursor]
        if cursor_batched:
            docs = [d for dd in docs for d in dd]
        for d in docs:
            if isinstance(d['_id'], bson.objectid.ObjectId):
                d['_id'] = str(d['_id'])
            # Better UI display if we use a non-mongo format
            if postproc is not None:
                postproc(d)
        return docs


    async def api_pipeline_task_reset(self, aset, pipeline, task):
        """Resets the given task, deleting all of its data.
        """
        api_info = _get_api_info({'aset': aset, 'pipeline': pipeline,
                'task': task})
        def run():
            api = faw_pipelines_util.Api(api_info, app_mongodb_conn.delegate)
            # What's important is that the version gets changed; the admin loop
            # will pick up the correct version and downstream tasks.
            api.destructive__task_change_version(0, [])
        loop = asyncio.get_running_loop()
        await loop.run_in_executor(None, run)


    async def api_pipeline_task_set_disabled(self, aset, pipeline, task,
            disabled):
        """Flip the state of a task being enabled or disabled.
        """
        api_info = _get_api_info({'aset': aset, 'pipeline': pipeline,
                'task': task})
        def run():
            api = faw_pipelines_util.Api(api_info, app_mongodb_conn.delegate)
            api._internal_task_status_set_disabled(disabled)
        loop = asyncio.get_running_loop()
        await loop.run_in_executor(None, run)


    async def api_pipeline_task_status(self, aset, pipeline, task):
        """Returns all task information, including last run information if it
        is not yet done.
        """
        api_info = _get_api_info({'aset': aset, 'pipeline': pipeline,
                'task': task})
        def run():
            api = faw_pipelines_util.Api(api_info, app_mongodb_conn.delegate)

            result = {}
            task_status = api._internal_task_status_get_state()
            result['version'] = task_status.version
            result['done'] = task_status.done
            result['disabled_by_config'] = (
                    app_config['pipelines'][pipeline]['disabled']
                    or app_config['pipelines'][pipeline]['tasks'][task]['disabled'])
            result['disabled_by_ui'] = task_status.disabled
            result['disabled'] = result['disabled_by_ui'] or result['disabled_by_config']
            result['status_msg'] = task_status.status_msg
            result['last_run_info'] = None
            if not task_status.done:
                result['last_run_info'] = api._internal_task_status_get_last_run_info()
            return result
        loop = asyncio.get_running_loop()
        return await loop.run_in_executor(None, run)



class _Timer:
    def __init__(self):
        self._last = time.monotonic()
        self._records = collections.defaultdict(float)


    @property
    def records(self):
        return self._records


    @contextlib.contextmanager
    def save(self, name):
        start = time.monotonic()
        try:
            yield
        finally:
            self._records[name] += time.monotonic() - start



if __name__ == '__main__':
    main()

