
import aiohttp.web as web
import asyncio
import bson
import cachetools
import click
import collections
import contextlib
import functools
import ujson as json
import logging
import mimetypes
import motor.motor_asyncio
import multiprocessing.connection
import os
import pathlib
import pickle
import psutil
import pymongo
import pympler.asizeof as asizeof
import re
import sys
import tempfile
import time
import traceback
import vuespa
from typing import Dict, Any, List

import app_util
import faw_analysis_set
import faw_analysis_set_parse
import faw_analysis_set_util
import faw_pipelines_util
import parserartifacts
import substitutions

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
app_production = False

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
            app_init, app_mongodb, app_mongodb_conn, app_pdf_dir, app_production

    logging.basicConfig()

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
    app_production = production

    mhost_port, db = app_mongodb.split('/')
    mhost, mport = mhost_port.split(':')
    app_mongodb_conn = motor.motor_asyncio.AsyncIOMotorClient(host=mhost,
            port=int(mport))[db]

    loop = asyncio.get_event_loop()
    # Important! Longer collection names requires 4.4+
    # https://docs.mongodb.com/manual/reference/limits/
    async def admin_cfg():
        await app_mongodb_conn.client.admin.command({
                'setFeatureCompatibilityVersion': '4.4'})
    loop.run_until_complete(admin_cfg())

    app_config_refresh = loop.create_task(_config_check_loop())
    app_init = loop.create_task(init_check_pdfs())
    loop.create_task(faw_analysis_set.main_loop(app_mongodb_conn,
            app_config, _get_api_info))
    loop.run_in_executor(None, _listen_for_clear_db_signal, port + 2, loop)
    vuespa.VueSpa('ui', Client, host=host, port=port,
            development=not production,
            config_web_callback=functools.partial(config_web, pdf_dir=pdf_dir)
            ).run()


def _listen_for_clear_db_signal(port: int, loop: asyncio.AbstractEventLoop):
    """Schedule a DB clear on the given loop when contacted on the given port.

    For communication with the FAW CLI, which (in development mode) can clear
    the DB.

    Blocks, so should be run in another thread.
    """
    # No need for real auth since the docker container won't expose this port
    listener = multiprocessing.connection.Listener(('localhost', port))
    while True:
        # Expect only a single message from a client, since the CLI
        # process is ephemeral
        conn = listener.accept()
        msg = conn.recv()
        if msg == 'clear_database':
            try:
                # Run `_clear_db` in the main thread's event loop
                future = asyncio.run_coroutine_threadsafe(_clear_db(), loop)
                # Block until done
                future.result()
                conn.send('done')
            finally:
                conn.close()


def config_web(app, pdf_dir):
    """Add an endpoint for direct downloading of files.
    """
    app.router.add_routes([
            web.get('/file_download/{file:.+}', _config_web_file_download_handler),
            web.get('/file_list', _config_web_file_list_handler),
            web.post('/decisions', _config_web_decisions_handler),
    ])


@contextlib.asynccontextmanager
async def _file_fetch(fname):
    """Due to the addition of the `file_transform` key at the root of config,
    we now route all files through the API. This utility method helps with that.
    """
    api_info = _get_api_info()
    def run_one():
        api = faw_pipelines_util.Api(api_info, app_mongodb_conn.delegate)
        ctx = api.file_fetch(fname)
        return ctx, ctx.__enter__()
    def run_two(ctx):
        ctx.__exit__(None, None, None)
    loop = asyncio.get_running_loop()
    res_ctx, res_path = await loop.run_in_executor(None, run_one)
    try:
        yield res_path
    finally:
        await loop.run_in_executor(None, run_two, res_ctx)


async def _config_web_file_download_handler(req):
    fname = req.match_info['file']
    async with _file_fetch(fname) as fpath:
        loop = asyncio.get_running_loop()
        def get_bytes():
            with open(fpath, 'rb') as ff:
                return ff.read()
        res = await loop.run_in_executor(None, get_bytes)
    return web.Response(body=res, content_type=mimetypes.guess_type(fname)[0])


async def _config_web_file_list_handler(req):
    file_list = list(_walk_pdf_files())
    return web.Response(text='\n'.join(file_list))


async def _config_web_decisions_handler(req):
    params = await req.post()
    analysis_set_id = params['analysis_set_id']
    decision_dsl = params['dsl']

    options = {'analysis_set_id': analysis_set_id}
    pdfGroups = await get_decisions(options)

    parameters = {'groups': pdfGroups, 'dsl': decision_dsl}
    parametersJson = json.dumps(parameters)

    # print("Calling from: " + os.path.join(etl_path, 'pdf-observatory', 'ci', 'dist'))
    # print("With data\n" + parametersJson)

    proc = await asyncio.create_subprocess_exec(
        'node', '--experimental-specifier-resolution=node', '.',
        #bufsize=-1,
        cwd=os.path.join(etl_path, 'pdf-observatory', 'ci', 'dist'),
        stdin=asyncio.subprocess.PIPE,
        stdout=asyncio.subprocess.PIPE,
        stderr=asyncio.subprocess.PIPE,
    )
    stdout, stderr = await proc.communicate(input=bytes(parametersJson, 'utf-8'))
    if await proc.wait() != 0:
        raise ValueError(f"non-zero exit: {stderr.decode('utf-8')}")

    pdfDecisions = json.loads(stdout.decode('utf-8'))
    return web.json_response(pdfDecisions)


async def _clear_db():
    print('Aborting any running DB task')
    _db_abort_process()
    print('Dropping DB')
    await app_mongodb_conn.client.drop_database(app_mongodb_conn.name)
    print('Running new initialization task')
    _db_reprocess()
    print('DB reset complete')


async def _config_check_loop():
    while True:
        try:
            ts = os.path.getmtime(app_config_path)
            if ts != app_config_loaded:
                # Note -- various system/docker configs can result in
                # development overwrites being in the past. So use not equal.
                _config_reload()
        except Exception:
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
            promises.append(asyncio.create_task(stat_pop(k, parser)))
    if promises:
        await asyncio.wait(promises)
    return parsers

def _get_api_info(extra_info={}):
    """NOTE: Updates here *used* to need to be mirrored at `faw/teaming/pyinfra/deploy.py`.
    However, we now rely on dask to distribute API information.
    """
    r = {
        'hostname': app_hostname,
        'hostport': app_hostport,
        'dask': f'{app_hostname}:8786',
        'mongo': app_mongodb,
        'pdfdir': app_pdf_dir,
        'pdftransform': app_config['file_transform'],
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


def _db_reprocess(*args, **kwargs):
    global app_init
    loop = asyncio.get_event_loop()
    app_init = loop.create_task(init_check_pdfs(*args, **kwargs))


class _DbLoaderProc:
    """Used to actually control a process; now just controls abort behavior for
    fs scan.
    """
    def __init__(self):
        self.aborted = False
    def abort(self):
        self.aborted = True
_db_loader_proc = _DbLoaderProc()
def _db_abort_process():
    """Returns new `_DbLoaderProc`"""
    global _db_loader_proc

    # Try to abort old `init_check_pdfs` call.
    _db_loader_proc.abort()
    _db_loader_proc = _DbLoaderProc()
    return _db_loader_proc
async def init_check_pdfs():
    try:
        await _init_check_pdfs()
    except Exception:
        # Doesn't trickle up, so just print it and raise an exit
        traceback.print_exc()
        sys.exit(1)
async def _init_check_pdfs():
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

        # Create additional indices required for other FAW components
        db['rawinvocations'].create_index([('file', pymongo.ASCENDING)]),
        db['rawinvocations'].create_index([('result._cons', pymongo.ASCENDING)]),

        db['invocationsparsed'].create_index([('file', pymongo.ASCENDING)]),
        db['invocationsparsed'].create_index([('result.k', pymongo.ASCENDING)]),
        # Another index for quickly gathering information about features from
        # different exit codes
        db['invocationsparsed'].create_index([('parser', pymongo.ASCENDING), ('exitcode', pymongo.ASCENDING)]),
    ])

    # Use a batch to support parallelism on mongodb's side
    batch = set()
    batch_max = 100
    n_inserted = 0
    async def insert_or_ignore(fpath):
        nonlocal n_inserted
        try:
            await col.insert_one({'_id': fpath, 'queueStart': None,
                    'queueStop': None, 'queueErr': None})
        except pymongo.errors.DuplicateKeyError:
            pass
        else:
            n_inserted += 1
    async def kick_asets_if_inserted():
        nonlocal n_inserted
        if n_inserted == 0:
            return

        # Files were added; mark all analysis sets as not having executed their
        # parsers AND needing a rebuild.
        # This is terrible in one sense, but should only really happen with
        # development-scale deployments. It also does not affect individual
        # parsers -- most files+parser combinations will be an expensive NOOP,
        # meaning the parsers won't be re-run.
        await db['as_metadata'].update_many({}, {
                '$set': {
                    # None != {}, will trigger index rebuild
                    'parser_versions': [None, None],
                    'parser_versions_done': [{}, {}],
                    # Ironically, setting UP_TO_DATE is needed so that
                    # `parser_versions` gets set correctly.
                    'status': faw_analysis_set.AsStatus.UP_TO_DATE.value,
                },
        })
        # For subsequent runs in development mode
        n_inserted = 0

    print(f'Scanning for new files...')
    tlast = time.monotonic()
    for ff_i, ff in enumerate(_walk_pdf_files()):
        if loader_proc.aborted:
            # User re-triggered this step, so stop processing in this loop
            # immediately.
            return

        batch.add(asyncio.create_task(insert_or_ignore(ff)))
        if len(batch) > batch_max:
            _, batch = await asyncio.wait(batch,
                    return_when=asyncio.FIRST_COMPLETED)

        tnew = time.monotonic()
        if tnew - tlast > 30.:
            print(f'...scanned {ff_i} files')
            tlast = tnew
    if batch:
        await asyncio.wait(batch)
    print(f'...scan complete, found {ff_i+1} files')
    await kick_asets_if_inserted()

    # For development mode -- use watchgod to live-reload files. Don't do this
    # in production because that would be weird.
    try:
        import watchgod
    except ImportError:
        return

    async for changes in watchgod.awatch(app_pdf_dir):
        for ctype, cpath in changes:
            cpath = os.path.relpath(cpath, app_pdf_dir)
            if ctype == watchgod.Change.added:
                print(f'File created: {cpath}; reprocessing if new')
                await insert_or_ignore(cpath)
            elif ctype == watchgod.Change.modified:
                n_inserted += 1
                # Clearing tool execution is enough to re-trigger all other
                # aspects, when coupled with kick_asets
                print(f'File modified: {cpath}; reprocessing')
                await db['rawinvocations'].delete_many({
                        'file': os.path.join(app_pdf_dir, cpath)})
                await db['observatory'].update_one(
                        {'_id': cpath}, {'$unset': {'idle_complete': True}})
            elif ctype == watchgod.Change.deleted:
                # FIXME We don't acknowledge file deletions at present -- here
                # or on init.
                print(f'File deleted: {cpath}; doing nothing')
            else:
                raise NotImplementedError(ctype)
        await kick_asets_if_inserted()

# A global cache for document information primarily for use by
# `get_decisions` and `fetch_statsbyfile`.
# Keys:
#  (*option_kv, match_id)
# Values:
#  List of lists of dict, where the dict contains:
#   "_id": filename
#   feature text: 1 if present, 0 otherwise
document_cache = cachetools.TTLCache(maxsize=100, ttl=5 * 60)


async def get_decisions(options):
    """
    Get decisions given a set of options.

    Args:
        options: See `fetch_statsbyfile` for details.

    NOTE: We make use of the global `document_cache` instance.
    """
    groups = {}
    files = []
    r = {'groups': groups, 'files': files}

    start = time.monotonic()
    print(f'Loading decisions for {options}')

    cursor = fetch_statsbyfile(options)
    async for gg in cursor:
        for g in gg:
            gid = len(files)
            files.append(g['_id'])
            for k, v in g.items():
                if k.startswith('_'):
                    continue
                grp = groups.get(k)
                if grp is None:
                    groups[k] = grp = []
                grp.append([gid, v])

    print(f'...Decisions packaged in {time.monotonic() - start:.2f}s')

    return r


async def fetch_statsbyfile(options, match_id=None):
    """Async generator which yields from `statsbyfile` according to the
    working subsetting options in `options`.

    For efficiency, yields batches rather than individual documents! This
    cuts overhead by as much as 50% due to the way async works in python.

    Args:
        options:
            analysis_set_id: Analysis set to use for returning results
            file_ids?: Optional list of file IDs to restrict results.
        match_id: Document id to return.

    NOTE: We make use of the global `document_cache` instance.
    """

    assert 'analysis_set_id' in options

    # Check the cache -- if analysis set has been updated, then we must bust
    # the cache
    adoc = await app_mongodb_conn['as_metadata'].find_one({
            '_id': options['analysis_set_id']})
    if adoc is None:
        raise ValueError(f"Failed to fetch analysis set {repr(options['analysis_set_id'])}")

    def dict_hash(vdict):
        '''hash dictionary elements reliably'''
        r = []
        for k, v in sorted(vdict.items(), key=lambda m: m[0]):
            if isinstance(v, dict):
                r.append((k, dict_hash(v)))
            else:
                r.append((k, v))
        return tuple(r)
    fresh_key = (
            # Hashable elements describing this analysis set's state
            adoc.get('status_done_time'),
            await app_mongodb_conn['as_c_' + options['analysis_set_id']].estimated_document_count(),
            dict_hash(adoc.get('parser_versions_done', [{}, {}])[1]))
    option_kv = [(k, v) for k, v in options.items()]
    option_kv = [(k, v) if k != 'file_ids' else (k, tuple(sorted(v)))
                 for k, v in option_kv]
    cache_key = (*option_kv, match_id)

    v = document_cache.get(cache_key)
    if v is not None:
        if v[0] == fresh_key:
            # Cache data is up-to-date
            print(f'Using cache for {adoc["_id"]} / {fresh_key}')
            for row in v[1]:
                yield row
            return

    # Else, update cache
    data_to_cache = []

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
    while True:
        n_fetch = 10 if doc_sz == 0 else 1 + int(128e6 / doc_sz)
        docs = await cursor.to_list(n_fetch)
        if not docs:
            break
        yield docs
        data_to_cache.append(docs)

        if doc_sz == 0:
            doc_sz = asizeof.asizeof(docs) / len(docs)

        # Manual conversion
        # g_new = {'_id': g['_id']}
        # for o in g['f']:
        #    g_new[o['k']] = o['v']
        # yield g_new

    # OK, iteration finished, add to cache
    document_cache[cache_key] = (fresh_key, data_to_cache)


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

        plugin_def, extra_api_info = _plugin_key_process(
            'file_detail_views', plugin_key
        )
        api_info = _get_api_info()
        parser_configs = faw_analysis_set_util.lookup_all_parsers(
            db=app_mongodb_conn.delegate, app_config=app_config
        )

        assert isinstance(input_spec, str), input_spec
        async with _file_fetch(input_spec) as input_path:
            if plugin_def['type'] == 'program_to_html':
                cmd = plugin_def.get('exec')
                assert cmd is not None, cmd

                artifact_types = substitutions.artifact_types(cmd)
                assert not artifact_types.output_artifact_types, cmd

                file_out = None
                def get_output_html():
                    nonlocal file_out
                    assert file_out is None, 'Cannot use <outputHtml> twice'
                    file_out = tempfile.NamedTemporaryFile(delete=False,
                            suffix='.html')
                    file_out.close()
                    return file_out.name
                with tempfile.TemporaryDirectory() as artifacts_root_dir:
                    # run upstream parsers, for artifacts
                    upstream_parsers = parserartifacts.ParserDependencyGraph(
                        parser_configs
                    ).parsers_upstream_from_artifact_types(
                        artifact_types.input_artifact_types
                    )
                    for parser in upstream_parsers:
                        faw_analysis_set_parse.as_run_tool(
                            fpath=input_path,
                            fpath_tool_name='',  # Unnecessary since we discard the result
                            parser_inv_name=parser,
                            parser_tool_version={'': ''},  # Unnecessary since we discard the result
                            parser_cfg=parser_configs[parser],
                            api_info=api_info,
                            timeout_default=app_config['parserDefaultTimeout'],
                            artifacts_root_dir=pathlib.Path(artifacts_root_dir),
                        )

                    # Run the command
                    with self._cmd_plugin_template_replace(
                        cmd,
                        vuespa_url,
                        json_args=json_args,
                        get_output_html_filename=get_output_html,
                        extra_substitutions=substitutions.file_plugin_substitutions(
                            filename=input_path,
                            artifacts_root_dir=pathlib.Path(artifacts_root_dir),
                        ),
                        extra_api_info=extra_api_info,
                    ) as cmd:
                        try:
                            proc = await asyncio.create_subprocess_exec(
                                    *cmd,
                                    cwd=plugin_def['cwd'],
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


    async def api_config_plugin_dec_run(
        self,
        plugin_key: str,
        api_url: str,
        json_args: Dict[str, Any],
        reference_decisions: List[Dict[str, Any]],
        subset_options: Dict[str, Any],
        extra_features_by_file: Dict[str, List[str]],
    ):
        """Runs a decision plugin.

        Args:
            extra_features_by_file:
                Mapping from filename to additional features derived
                from parser output. May not be complete.
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
                with self._cmd_plugin_template_replace(
                    cmd, api_url, json_args=json_args,
                    get_output_html_filename=get_output_html,
                    extra_api_info=extra_api_info,
                    extra_substitutions=[],
                ) as cmd:
                    proc = await asyncio.create_subprocess_exec(
                            *cmd,
                            cwd=plugin_def['cwd'],
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
                                    async for dd in fetch_statsbyfile(
                                            subset_options):
                                        if bytes_written > 500e6:
                                            await s.drain()
                                            bytes_written = 0
                                        for d in dd:
                                            filename = d['_id']
                                            extra_features = (
                                                extra_features_by_file.get(
                                                    filename, []
                                                )
                                            )
                                            if extra_features:
                                                d = {
                                                    **d,
                                                    **{
                                                        feature: 1 for feature
                                                        in extra_features
                                                    },
                                                }
                                            a = json.dumps(d, ensure_ascii=False
                                                    ).encode('utf-8')
                                            s.write(a)
                                            s.write(b'\n')
                                            bytes_written += len(a) + 1
                                    await s.drain()
                            else:
                                raise NotImplementedError(part)
                    finally:
                        s.close()
                        await s.wait_closed()

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
                        'html_size': len(result['html']),
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
            options: See documentation for `get_decisions` and `fetch_statsbyfile`
        """
        return await get_decisions(options)

    @contextlib.contextmanager
    def _cmd_plugin_template_replace(
        self,
        cmd,
        api_url,
        json_args,
        get_output_html_filename,
        extra_substitutions,
        extra_api_info,
    ):
        if not api_url.endswith('/'):
            api_url = api_url + '/'

        try:
            temp_root_dir = tempfile.TemporaryDirectory()
            r = substitutions.subsitute_arguments(
                cmd,
                [
                    *substitutions.common_substitutions(
                        api_info=_get_api_info(extra_api_info),
                        temp_root=temp_root_dir.name,
                    ),
                    *substitutions.plugin_substitutions(
                        json_args=json_args,
                        get_output_html_filename=get_output_html_filename,
                        files_path=app_pdf_dir,
                        mongodb_url=app_mongodb,
                        workbench_api_url=api_url,
                    ),
                    *extra_substitutions,
                ]
            )
            yield r
        finally:
            temp_root_dir.cleanup()

    async def api_loading_get(self, options):
        """Returns an object with {loading: boolean, message: string}.

        options: {}
        """
        col = app_mongodb_conn['observatory']

        if options:
            raise ValueError(options)

        # MongoDB efficiency note -- `count_documents` is rather expensive when
        # it returns a large number.
        pdfs_max = await col.estimated_document_count()
        pdfs_not_done = (
                await app_mongodb_conn['as_parse'].estimated_document_count())
        if pdfs_not_done != 0:
            # Mongodb estimates are terrible and this makes a UI element pop
            # up, so...
            if (await app_mongodb_conn['as_parse'].find_one({}, {})) is None:
                pdfs_not_done = 0
        pdfs_err = await (app_mongodb_conn[faw_analysis_set_parse.COL_NAME]
                .count_documents({'error_until': {'$exists': True}}))
        # Faster than one would think, because number of idle parses is capped
        pdfs_idle = await (app_mongodb_conn[faw_analysis_set_parse.COL_NAME]
                .count_documents({'parsers.idle_compute': True}))

        # Hack for pipeline debugging
        idle_text = '' if not pdfs_idle else f' ({pdfs_idle} from idle)'
        return {
                'config_mtime': app_config_loaded,
                'files_parsing': pdfs_not_done,
                'files_max': pdfs_max,
                'files_err': pdfs_err,
                'message': f'{pdfs_not_done} / {pdfs_max} files parsing{idle_text}; {pdfs_err} errors',
        }


    async def api_load_db(self, pdf, collection, as_options=None,
            other_options=None):
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
            # This is now a virtual collection; select data from relevant places
            if other_options and other_options.get('as_only'):
                cursor = fetch_statsbyfile(as_options, match_id=pdf)
                cursor_batched = True
                as_options = None
            else:
                docs = [{}]
                async for d in app_mongodb_conn['invocationsparsed'].find({
                        'file': pdf}):
                    p = d['parser']
                    docs[0].update({f'{p}_{kv["k"]}': kv['v'] for kv in d['result']})
                return docs
        else:
            raise NotImplementedError(collection)

        assert as_options is None

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

