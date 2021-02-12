
import asyncio
import bson
import click
import functools
import importlib.util
import json
import math
import motor.motor_asyncio
import os
import pymongo
import re
import shlex
import strictyaml
import sys
import tempfile
import traceback
import vuespa

app_config = None
app_config_loaded = None
app_config_path = None
app_docker = False

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
@click.option('--in-docker/--not-in-docker', default=False,
        help="Must specify if running in docker.")
@click.option('--production/--no-production', default=False,
        help="Specify to use pre-built version of UI, rather than building "
            "on the fly and using Vue's hot reload functionality.")
@click.option('--config', type=str, required=True,
        help="(Required) Path to .json defining this observatory deployment.")
@click.option('--quit-after-config/--no-quit-after-config', default=False)
def main(pdf_dir, mongodb, host, port, in_docker, production, config,
        quit_after_config):
    """Run the PDF observatory on the given mongodb instance and database,
    providing a UI.

    Args:
        PDF_DIR: Directory containing PDF files which should be observed.
        MONGODB: e.g. localhost:27017/observe or just "observe" - server, port,
                and database in which to store pdf observatory results.
    """

    global app_config, app_config_path, app_docker, app_init, \
            app_mongodb, app_mongodb_conn, app_pdf_dir

    assert in_docker, 'Config specifying parsers must be in docker'

    app_config_path = config
    _config_reload()

    if quit_after_config:
        return

    app_docker = in_docker
    app_pdf_dir = os.path.abspath(pdf_dir)
    if '/' in mongodb:
        app_mongodb = mongodb
        mhost_port, db = mongodb.split('/')
        mhost, mport = mhost_port.split(':')
        app_mongodb_conn = motor.motor_asyncio.AsyncIOMotorClient(host=mhost,
                port=int(mport))[db]
    else:
        app_mongodb = 'localhost:27017/' + mongodb
        app_mongodb_conn = motor.motor_asyncio.AsyncIOMotorClient()[mongodb]


    loop = asyncio.get_event_loop()
    app_config_refresh = loop.create_task(_config_check_loop())
    app_init = loop.create_task(init_check_pdfs())
    vuespa.VueSpa('ui', Client, host=host, port=port,
            development=not production,
            config_web_callback=functools.partial(config_web, pdf_dir=pdf_dir)
            ).run()


def config_web(app, pdf_dir):
    """Add an endpoint for direct downloading of files.
    """
    import aiohttp.web as web
    app.router.add_routes([
            web.static('/file_download', pdf_dir),
    ])


async def _config_check_loop():
    while True:
        try:
            ts = os.path.getmtime(app_config_path)
            if ts > app_config_loaded:
                _config_reload()
        except:
            traceback.print_exc()

        await asyncio.sleep(0.5)



def _config_reload():
    global app_config, app_config_loaded, app_config_path

    ts_loaded = os.path.getmtime(app_config_path)
    app_config = json.load(open(app_config_path))
    # Build invokers.cfg based on specified invokers.
    etl_tools_path = os.path.join(etl_path, 'pdf-etl-tools')
    os.makedirs(etl_tools_path, exist_ok=True)
    with open(os.path.join(etl_path, 'pdf-etl-tools', 'invokers.cfg'),
            'w') as f:
        lines = []
        lines.append('[')
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
                        '<inputFile>': 'InputFile',
                }[v]

            # Haskell requires double quote delimiters, which JSON works with
            vrepr = json.dumps(v)
            return f'Str {vrepr}'
        first_rec = True
        for i, (inv_name, inv_cfg) in enumerate(app_config['parsers'].items()):
            if inv_cfg.get('disabled'):
                continue

            assert inv_cfg.get('exec'), inv_name
            assert inv_cfg.get('version'), inv_name

            hdr = ', ' if not first_rec else ''
            first_rec = False
            lines.append(f'{hdr}Invoker')
            exec_args = ', '.join([exec_encode(a) for a in inv_cfg['exec']])
            timeout = 20  # We no longer use this feature, so fix it to a large value
            version = inv_cfg['version']
            lines.append(f'  {{ exec = [ {exec_args} ]')
            lines.append(f'  , timeoutScale = Just {timeout}')
            lines.append(f'  , version = "{version}"')
            lines.append(f'  , invName = "{inv_name}"')
            lines.append(f'  }}')
        lines.append(']')
        f.write('\n'.join(lines))

    # Set at end of method, to get around any async / threading issues
    app_config_loaded = ts_loaded


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

    for path, subfolders, files in os.walk(app_pdf_dir):
        # Filter subsequent subfolders to not go into hidden directories
        for i in range(len(subfolders)-1, -1, -1):
            if subfolders[i].startswith('.'):
                subfolders.pop(i)

        for f in files:
            if f.startswith('.'):
                continue

            if loader_proc.aborted:
                # User re-triggered this step, so stop processing.
                return

            ff = os.path.relpath(os.path.join(path, f), app_pdf_dir)
            try:
                await col.insert_one({'_id': ff, 'queueStart': None,
                        'queueStop': None, 'queueErr': None})
            except pymongo.errors.DuplicateKeyError:
                pass

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


    async def api_config_get(self):
        return app_config


    async def api_config_plugin_run(self, plugin_key, vuespa_url, json_args,
            input_spec):
        plugin_def = app_config['file_detail_views'].get(plugin_key, {})
        t = plugin_def.get('type')
        assert t is not None, f'{plugin_key} -> .type -> {plugin_def}'

        if t == 'program_to_html':
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
                file_out = tempfile.NamedTemporaryFile(delete=False)
                file_out.close()
                return file_out.name
            cmd = self._cmd_plugin_template_replace(cmd, vuespa_url, {
                    '<inputFile>': lambda: input_spec,
                    '<jsonArguments>': lambda: json.dumps(json_args),
                    '<outputHtml>': get_output_html,
            })

            try:
                proc = await asyncio.create_subprocess_exec(
                        *cmd,
                        cwd=os.path.join(etl_path, 'dist'),
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
            raise NotImplementedError(t)


    async def api_config_plugin_dec_run(self, plugin_key, api_url, json_args,
            reference_decisions, subset_options):
        """Runs a decision plugin.
        """
        output_html = None
        def get_output_html():
            nonlocal output_html
            assert output_html is None, 'Cannot use <outputHtml> twice'
            output_html = tempfile.NamedTemporaryFile(delete=False)
            output_html.close()
            return output_html.name

        plugin_def = app_config['decision_views'].get(plugin_key, {})
        t = plugin_def.get('type')
        assert t is not None, f'{plugin_key} -> .type -> {plugin_def}'

        try:
            if t == 'program':
                cmd = plugin_def.get('exec')
                assert cmd is not None, 'exec'
                cmd = self._cmd_plugin_template_replace(cmd, api_url, {
                        '<jsonArguments>': lambda: json.dumps(json_args),
                        '<outputHtml>': get_output_html,
                })
                proc = await asyncio.create_subprocess_exec(
                        *cmd,
                        cwd=os.path.join(etl_path, 'dist'),
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
                                for q in reference_decisions:
                                    s.write((json.dumps(q) + '\n').encode('utf-8'))
                                    await s.drain()
                            elif part == '<statsbyfile>':
                                async for d in self._statsbyfile_cursor(
                                        subset_options):
                                    s.write((json.dumps(d) + '\n').encode('utf-8'))
                                    await s.drain()
                            else:
                                raise NotImplementedError(part)
                    finally:
                        s.close()

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

                if output_html is not None:
                    result['html'] = open(output_html.name, 'rb').read().decode(
                            'latin1')
                return result
            else:
                raise NotImplementedError(t)
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

        cursor = self._statsbyfile_cursor(options)
        async for g in cursor:
            gid = len(files)
            files.append(g['_id'])
            for k, v in g.items():
                if k.startswith('_'): continue
                grp = groups.get(k)
                if grp is None:
                    groups[k] = grp = []
                grp.append([gid, v])

        return r


    def _cmd_plugin_template_replace(self, cmd, api_url, extra_template_vars):
        if not api_url.endswith('/'):
            api_url = api_url+ '/'

        template_vals = {
                '<filesPath>': lambda: app_pdf_dir,
                '<mongo>': lambda: app_mongodb,
                '<workbenchApiUrl>': lambda: api_url,
        }
        for k, v in extra_template_vars.items():
            if k in template_vals:
                raise ValueError(f'Cannot overwrite {k}')
            template_vals[k] = v
        return [template_vals.get(c, lambda: c)() for c in cmd]


    async def _statsbyfile_cursor(self, options):
        """Async generator which yields from `statsbyfile` according to the
        working subsetting options in `options`.
        """
        query = {}
        if options['subsetRegex'] and options['subsetRegex'] != '^':
            query['_id'] = {'$regex': options['subsetRegex']}

        r_skip = 0
        r_every = 1
        if options['subsetSize'] > 0:
            max_docs = await app_mongodb_conn['statsbyfile'].count_documents(query)
            max_subset = max(1, int(math.ceil(max_docs / options['subsetSize'])))
            if options['subsetPartition'] >= max_subset:
                raise ValueError(f'Found {max_docs} docs, '
                        f'max partition {max_subset}, '
                        f'but partition {options["subsetPartition"]} was requested.')

            r_skip = options['subsetPartition']
            r_every = max_subset

        gi = 0
        cursor = app_mongodb_conn['statsbyfile'].find(query).sort('_id')
        if r_skip > 0:
            cursor = cursor.skip(r_skip)
        async for g in cursor:
            gi += 1
            if r_every > 1 and gi % r_every != 1:
                continue

            yield g


    async def api_clear_db(self):
        _db_abort_process()
        await app_mongodb_conn.client.drop_database(app_mongodb_conn.name)
        self.reprocess_db()


    async def api_reparse_db(self):
        _db_abort_process()
        await app_mongodb_conn.drop_collection('observatory')
        # A more robust method would be fixing queue_client to remove a
        # document from the groups to which it belongs, but this works since
        # the UI always re-processes the full DB at the moment.
        await app_mongodb_conn.drop_collection('invocationsparsed')
        await app_mongodb_conn.drop_collection('statsbyfile')
        self.reprocess_db()


    async def api_reset_db_errors(self):
        self.reprocess_db(retry_errors=True)


    async def api_loading_get(self, options):
        """Returns an object with {loading: boolean, message: string}.

        options: {subsetRegex}
        """
        col = app_mongodb_conn['observatory']
        query = {}
        if options['subsetRegex'] and options['subsetRegex'] != '^':
            query['_id'] = {'$regex': options['subsetRegex']}

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


    async def api_load_db(self, pdf, collection):
        """Loads a specific entry from the pdf-etl database, for manual
        inspection.
        """
        query = None
        if collection == 'rawinvocations':
            query = {'file': os.path.join(app_pdf_dir, pdf)}
        elif collection == 'invocationsparsed':
            query = {'file': pdf}
        elif collection == 'statsbyfile':
            query = {'_id': pdf}
        else:
            raise NotImplementedError(collection)

        docs = [d async for d in app_mongodb_conn[collection].find(query)]
        for d in docs:
            if isinstance(d['_id'], bson.objectid.ObjectId):
                d['_id'] = str(d['_id'])
        return docs


    def reprocess_db(self, *args, **kwargs):
        global app_init
        loop = asyncio.get_event_loop()
        app_init = loop.create_task(init_check_pdfs(*args, **kwargs))



if __name__ == '__main__':
    main()

