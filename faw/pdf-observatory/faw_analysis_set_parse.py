"""Manages parsing within analysis sets.
"""

import faw_analysis_set_util
import faw_internal_util
import faw_pipelines_util

import dask.distributed
import io
import json
import os
import psutil
import pymongo
import re
import shutil
import subprocess
import tempfile
import threading
import time
import traceback
from typing import Dict, Tuple, Union

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

COL_NAME = 'as_parse'

# `exit_flag` is issues/5975
async def as_parse_main(exit_flag, app_config, api_info):
    """This is a workaround for https://github.com/dask/distributed/issues/5975.
    Basically, by using an async task, we spawn directly on the worker's thread,
    so we can use our own management to keep the actor going.
    """
    import asyncio
    loop = asyncio.get_running_loop()
    await loop.run_in_executor(None, _as_parse_main, exit_flag, app_config, api_info)

def _as_parse_main(exit_flag, app_config, api_info):
    """Continuously goes through `COL_NAME` to find documents which need to be
    parsed; distributes files to workers by batch.
    """
    mongo_info = api_info['mongo']
    db = faw_internal_util.mongo_api_info_to_db_conn(mongo_info)
    col = db[COL_NAME]
    col_obs = db['observatory']

    col.create_index([('error_until', 1)])
    col.create_index([('priority_order', 1)])
    col_obs.create_index([('idle_complete', 1)])

    # issues/5975
    #with dask.distributed.worker_client() as client:
    # For the occupancy bug
    client = dask.distributed.get_client()
    if True:
        [app_config_future] = client.scatter([app_config], broadcast=True)

        # Begin by cleaning up the idle definition; otherwise, might start
        # running idle parsers pre-maturely, or they might be requested to run
        # at the wrong version.
        _as_parse_ensure_idle_versions(app_config, db)

        # We have to track everything here, rather than relying on `pure=True`.
        # This is a result of
        # So, track outstanding ids for e.g. transient errors.
        outstanding_docs = []  # (doc, future)
        outstanding = dask.distributed.as_completed(with_results=True,
                raise_errors=False)

        work_max = 0
        last_update = 0.
        def next_work(from_idle=False):
            nonlocal last_update, work_max
            t = time.monotonic()
            if t - last_update > 10:
                # Update cluster size
                work_max = 0
                for w in client.scheduler_info()['workers'].values():
                    work_max += 4 * w['nthreads']

                # Clear out errors that are old
                col.update_many({'error_until': {'$lt': time.time()}},
                        {'$unset': {'error_until': True}})

                # Wait another X sec before doing this bit
                last_update = t


            if outstanding.count() > work_max // 2:
                # Busy enough, no need to query DB
                return

            bad_ids = [o[0]['_id'] for o in outstanding_docs]
            data = (col.find({'_id': {'$nin': bad_ids},
                        'error_until': {'$exists': False}})
                    .sort([('priority_order', 1)])
                    .limit(work_max - outstanding.count())
                    )
            num_added = 0
            for doc in data:
                num_added += 1
                future = client.submit(_dask_as_parse_file,
                        app_config_future, api_info, doc,
                        resources={'faw_parse': 1.},
                        pure=False)
                outstanding_docs.append((doc, future))
                outstanding.add(future)

            if not from_idle and num_added == 0:
                # Not enough new work that is a subset of parsers. Therefore,
                # add all parsers as idle, busy work
                idle_work()

        def idle_work():
            # First, see if we have new versions of the idle parsers
            _as_parse_ensure_idle_versions(app_config, db)

            data = col_obs.aggregate([
                    {'$match': {'idle_complete': {'$exists': False}}},
                    {'$lookup': {
                        'from': col.name,
                        'let': {'id': '$_id'},
                        'pipeline': [
                            {'$match': {'$expr': {'$eq': ['$_id', '$$id']}}},
                        ],
                        'as': 'col_parse',
                    }},
                    {'$match': {'col_parse': []}},
                    {'$project': {'col_parse': 0}},
                    {'$limit': work_max - outstanding.count()},
            ])
            data_seen = 0
            for doc_obs in data:
                # Cast from 'observatory' to 'as_parse'-typed document
                r = col.update_one({'_id': doc_obs['_id']},
                        {
                            '$set': {'parsers.idle_compute': True},
                            '$min': {'priority_order': 1e30},
                        },
                        upsert=True)
                data_seen += 1

            if (data_seen == 0
                    and col_obs.find_one({'idle_complete': {'$exists': False}}
                        ) is None):
                # All idle processing is complete. Update parser_versions_done
                # so we can skip this step in subsequent analysis set
                # compilations.
                doc = db['misc'].find_one({'_id': 'as_idle'})
                if doc is not None:
                    # This is safe only because this is the only spot
                    # parser_versions_done is set on the idle analysis set.
                    changed = False
                    doc_done = doc.get('parser_versions_done', [{}, {}])
                    for k, v in doc['parser_versions'][1].items():
                        if (
                                # Not disabled
                                v is not None
                                # Wrong version
                                and doc_done[1].get(k) != v):
                            doc_done[1][k] = v
                            changed = True
                    if changed:
                        db['misc'].update_one(
                                {'_id': 'as_idle', 'parser_versions': doc['parser_versions']},
                                {'$set': {'parser_versions_done': doc_done}})

            # Now that documents are in col_parse (maybe), initiate a standard
            # load
            next_work(from_idle=True)

        # was while True before issues/5975
        while not exit_flag[0]:
            # Initial load
            next_work()

            # Iterator will keep us from maxing out CPU
            for f, f_result in outstanding:
                # issues/5975
                if exit_flag[0]:#faw_internal_util.dask_check_if_cancelled():
                    return

                # Clean up list
                for i, (doc, fut) in enumerate(outstanding_docs):
                    if f is fut:
                        f_doc = doc
                        outstanding_docs.pop(i)
                        break
                else:
                    raise ValueError("Future expected, but not found?")

                # Check for error -- blacklist for some time
                error_min = 5.
                error_scale = 2.
                error_max = 3600 * 24
                if f.exception() is not None:
                    # Log exception, mark as error for some time
                    traceback.print_exception(*f_result)
                    error_delay = f_doc.get('error_delay', error_min)
                    error_until = time.time() + error_delay
                    error_delay *= error_scale
                    error_delay = min(error_max, error_delay)
                    col.update_one({'_id': f_doc['_id']},
                            {'$set': {
                                'error_until': error_until,
                                'error_delay': error_delay}})
                else:
                    # Success -- clear `error_delay` if the document still exists
                    col.update_one({'_id': f_doc['_id']},
                            {'$unset': {'error_delay': True}})

                # Purely for dask dashboard; prevent future from hanging around
                del f

                next_work()

            # Done with all work; sleep awhile and see if there's more (already
            # seceded, so don't worry about that)
            time.sleep(2)


def _as_parse_ensure_idle_versions(app_config, db):
    """Ensure that any idle parsing catches all version changes.
    """
    versions_id, versions_data = faw_analysis_set_util.aset_parser_versions_calculate_idle(app_config)
    versions = [versions_id, versions_data]
    doc = db['misc'].find_one({'_id': 'as_idle'})
    if doc is None or doc['parser_versions'] != versions:
        # Reset all idle_complete
        db['observatory'].update_many({}, {'$unset': {'idle_complete': True}})
        # Flag as the version we'll be running
        db['misc'].update_one({'_id': 'as_idle'},
                {'$set': {'parser_versions': versions}},
                upsert=True)


def _dask_as_parse_file(app_config, api_info, doc):
    """Fetch the given file with API, and run required parser(s) on it.

    When done, remove specific versions from queue.
    """
    db = faw_internal_util.mongo_api_info_to_db_conn(api_info['mongo'])
    col = db[COL_NAME]
    col_obs = db['observatory']

    doc_id = doc['_id']
    doc_invname = f'/home/pdf-files/{doc_id}'
    parsers_done = {}  # {name: [tool, parser]}
    parsers_to_run_tool = {}
    parsers_to_run_parser = {}

    idle_compute = False
    if doc['parsers'].pop('idle_compute', None) is not None:
        idle_compute = True
        # On complete, delete this parser trace
        parsers_done['idle_compute'] = True

        # Modify doc['parsers'] s.t. all parsers are represented and will be ran
        # on this file.
        idle_parsers_doc = db['misc'].find_one({'_id': 'as_idle'})
        if idle_parsers_doc is None:
            raise ValueError("Could not find idle?")
        doc['parsers'].update(idle_parsers_doc['parser_versions'][1])

    def get_cfg(k):
        """Given a parser name `k`, resolve to relevant app_config"""
        parser_pipeline_name = faw_analysis_set_util.deconstruct_pipeline_parser_name(k)
        if parser_pipeline_name is None:
            parser_config = app_config['parsers'][k]
        else:
            parser_config = (app_config['pipelines']
                    [parser_pipeline_name.pipe]
                    ['parsers']
                    [parser_pipeline_name.parser])
            parser_config = parser_config.copy()
            parser_config['aset'] = parser_pipeline_name.aset
            parser_config['pipeline'] = parser_pipeline_name.pipe
        return parser_config

    parser_set = list(doc.get('parsers', {}).keys())
    if parser_set:
        # Collect rawinvocations and invocationsparsed versions, if any
        rawinv = list(db['rawinvocations'].find(
                {'file': doc_invname, 'invoker.invName': {'$in': parser_set}},
                {'invoker': True, 'result._cons': True}))
        invpar = list(db['invocationsparsed'].find(
                {'file': doc_id, 'parser': {'$in': parser_set}},
                {'parser': True, 'version_tool': True, 'version_parse': True}))
        versions_keyed = {}
        for v in rawinv:
            # Convert from old style to new style versions
            if not isinstance(v['invoker']['version'], dict):
                v['invoker']['version'] = {'': v['invoker']['version']}

            # Find version information, log this mongo document
            vv = versions_keyed.setdefault(v['invoker']['invName'],
                    [[v['invoker']['version'], None], [], []])
            # Should only be one, never know. Better to have self-healing db
            vv[1].append(v)
            if v['result']['_cons'].lower() not in ('goodresult', 'timeout'):
                # Want to re-run this one
                vv[0][0] = None
        for v in invpar:
            # Convert from old style to new style versions
            if not isinstance(v['version_tool'], dict):
                v['version_tool'] = {'': v['version_tool']}
            if not isinstance(v['version_parse'], dict):
                v['version_parse'] = {'': v['version_parse']}

            # Note that we want to delete parser if it doesn't match tool
            # version
            vv = versions_keyed.setdefault(v['parser'],
                    [[None, None], [], []])
            vv[2].append(v)

            if v['version_tool'] == vv[0][0]:
                vv[0][1] = v['version_parse']

        for k in parser_set:
            # Check version info -- not equal means it needs to run; equal means
            # it needs to be marked done
            ver_db_info = versions_keyed.get(k, [[None, None], [], []])
            ver_db = ver_db_info[0]
            ver_cfg = doc['parsers'][k]

            cfg = get_cfg(k)
            if cfg.get('disabled'):
                # We *do* want to unset based on version. A different version
                # may not be disabled.
                parsers_done[k] = ver_cfg
                continue

            if ver_db[0] != ver_cfg[0]:
                parsers_to_run_tool[k] = ver_cfg[0]
                parsers_to_run_parser[k] = ver_cfg[1]

                # Delete matching tool documents
                db['rawinvocations'].delete_many({'_id': {'$in': [
                        v['_id'] for v in ver_db_info[1]]}})
                db['invocationsparsed'].delete_many({'_id': {'$in': [
                        v['_id'] for v in ver_db_info[2]]}})
            elif ver_db[1] != ver_cfg[1]:
                parsers_to_run_parser[k] = ver_cfg[1]
                db['invocationsparsed'].delete_many({'_id': {'$in': [
                        v['_id'] for v in ver_db_info[2]]}})

            # Regardless, need to set this as done when we've run everything
            parsers_done[k] = ver_cfg

    if faw_internal_util.dask_check_if_cancelled():
        return

    tool_docs = {}
    if parsers_to_run_tool:
        # We need the file for these
        api = faw_pipelines_util.Api(api_info)
        with api.file_fetch(doc_id) as fpath:
            for k, v in parsers_to_run_tool.items():
                tool_doc = _as_run_tool(db['rawinvocations'],
                        fpath, doc_invname,
                        k, v,
                        get_cfg(k), api_info,
                        timeout_default=app_config['parserDefaultTimeout'])
                if faw_internal_util.dask_check_if_cancelled():
                    return

                if tool_doc is not None:
                    tool_docs[k] = tool_doc

    # Don't need file, do need results
    for k, v in parsers_to_run_parser.items():
        tool_doc = tool_docs.get(k)
        if tool_doc is None:
            tool_doc = db['rawinvocations'].find_one({'file': doc_invname,
                    'invoker.invName': k})
        assert tool_doc is not None, f'For {doc_invname} / {k}'

        def coll_resolver(name):
            _, name = name.rsplit('/', 1)
            return db[name]
        parsers_config = {k: get_cfg(k)}
        pdf_etl_parse.handle_doc(tool_doc, coll_resolver, fname_rewrite=doc_id,
                db_dst='/invocationsparsed', parse_version=v,
                parsers_config=parsers_config,
                parser_parsers_shared=app_config['parser_parsers_shared'])

        if faw_internal_util.dask_check_if_cancelled():
            return

    # Update database -- first remove parsers matching versions we ran, then
    # remove the document entirely if all done
    updates = []
    for k, v in parsers_done.items():
        updates.append(pymongo.UpdateOne(
            {'_id': doc_id, 'parsers.' + k: v},
            {'$unset': {'parsers.' + k: True}}))
    updates.append(pymongo.DeleteOne({'_id': doc_id, 'parsers': {}}))
    col.bulk_write(updates, ordered=True)
    if idle_compute:
        col_obs.update_one({'_id': doc_id}, {'$set': {'idle_complete': True}})


def _as_run_tool(col_dst, fpath, fpath_tool_name, parser_inv_name,
        parser_tool_version, parser_cfg, api_info, *, timeout_default):
    """Re-implementation of pdf-etl-tool.
    """

    timeout = parser_cfg['timeout'] or timeout_default
    doc = {
            'invoker': {'version': parser_tool_version, 'invName': parser_inv_name},
            'file': fpath_tool_name,
            'result': {
                'exitcode': None,
                'timeElapsed': 0.,
                'stdoutRes': '',
                'stderrRes': '',
                '_cons': 'RuntimeError',
            },
    }

    # Emulate args from pdf-etl-tool
    temps = []
    args = []
    try:
        for e in parser_cfg['exec']:
            if e == '<inputFile>':
                args.append(fpath)
            elif e == '<apiInfo>':
                parser_api_info = api_info.copy()
                if 'pipeline' in parser_cfg:
                    parser_api_info['aset'] = parser_cfg['aset']
                    parser_api_info['pipeline'] = parser_cfg['pipeline']
                args.append(json.dumps(parser_api_info))
            elif e.startswith('<tempFile'):
                suffix = e[9:-1]
                if suffix:
                    assert suffix[0] == ' ', suffix
                    assert ' ' not in suffix[1:], suffix
                    suffix = suffix[1:]
                    assert '"' not in suffix, suffix
                temps.append(tempfile.NamedTemporaryFile(suffix=suffix, delete=False))
                temps[-1].close()
                args.append(temps[-1].name)
            elif e.startswith('<tempPrefix'):
                suffix = e[11:-1]
                if suffix:
                    assert suffix[0] == ' ', suffix
                    assert ' ' not in suffix[1:], suffix
                    suffix = suffix[1:]
                    assert '"' not in suffix, suffix
                temps.append([tempfile.NamedTemporaryFile(suffix=suffix, delete=False)])
                temps[-1][0].close()
                args.append(temps[-1][0].name)
            else:
                args.append(e)

        t_start = time.monotonic()
        p = subprocess.Popen(args,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                cwd=parser_cfg['cwd'])

        psutil_mem = 0
        psutil_cpu = {}  # {pid: last known stats, summed at end}
        psutil_proc = psutil.Process(p.pid)
        def psutil_check():
            nonlocal psutil_mem
            try:
                s = psutil_proc.children(recursive=True)
            except psutil.NoSuchProcess:
                # Can no longer gather information
                return

            s.append(psutil_proc)
            mem = 0
            for p in s:
                try:
                    p_cpu = p.cpu_times()
                    p_mem = p.memory_info()
                except psutil.NoSuchProcess:
                    continue

                mem += p_mem.rss
                if p.pid == psutil_proc.pid:
                    mem += p_mem.shared

                psutil_cpu[p.pid] = p_cpu

            psutil_mem = max(psutil_mem, mem)

        finished = False
        timed_out = False
        try:
            # Subprocess may want to spawn dask workers; ensure it may
            dask.distributed.secede()

            def reader(stream, buf):
                for line in stream:
                    buf.write(line)
            buf_out = io.BytesIO()
            buf_err = io.BytesIO()
            reader_out = threading.Thread(target=reader, args=(p.stdout, buf_out))
            reader_err = threading.Thread(target=reader, args=(p.stderr, buf_err))
            reader_out.start()
            reader_err.start()
            while not faw_internal_util.dask_check_if_cancelled():
                psutil_check()
                try:
                    # Timeout affects psutil measurements!
                    p.wait(timeout=0.1)
                except subprocess.TimeoutExpired:
                    if time.monotonic() - t_start > timeout:
                        timed_out = True
                        finished = True
                        break
                else:
                    finished = True
                    break
        finally:
            dask.distributed.rejoin()

        # One final measurement if possible
        psutil_check()

        if not finished or timed_out:
            # Early stop -- kill, abort
            p.kill()
            p.wait()

        while reader_out.is_alive() or reader_err.is_alive():
            time.sleep(0.1)

        if not finished:
            # Dask requested we cancel -- do not write to DB
            return

        doc['result']['timeElapsed'] = time.monotonic() - t_start
        doc['result']['memMax'] = psutil_mem
        doc['result']['cpuUser'] = sum(p.user for p in psutil_cpu.values())
        doc['result']['cpuSystem'] = sum(p.system for p in psutil_cpu.values())
        doc['result']['cpuIowait'] = sum(p.iowait for p in psutil_cpu.values())
        doc['result']['exitcode'] = p.wait()
        paths = (
                [(t.name if not isinstance(t, list) else t[0].name, '<tempFile>')
                    for t in temps]
                + [(fpath, '<inputFile>')])
        doc['result']['stdoutRes'] = _trim_program_output(buf_out.getvalue(), paths)
        doc['result']['stderrRes'] = _trim_program_output(buf_err.getvalue(), paths)
        doc['result']['_cons'] = 'Timeout' if timed_out else 'GoodResult'
    finally:
        for t in temps:
            if isinstance(t, list):
                tdir = os.path.dirname(t[0].name)
                tbase = os.path.basename(t[0].name)
                for f in os.listdir(tdir):
                    if not f.startswith(tbase):
                        continue
                    f = os.path.join(tdir, f)
                    if os.path.isdir(f):
                        shutil.rmtree(f)
                    else:
                        os.unlink(f)
            else:
                os.unlink(t.name)

    exit_code = doc['result']['exitcode']
    if parser_cfg.get('mustSucceed') and exit_code != 0:
        stderr = doc['result']['stderrRes']
        raise ValueError(f'Parser {parser_inv_name} mustSucceed, but got {exit_code}\n\nstderr: {stderr}')

    if faw_internal_util.dask_check_if_cancelled():
        return

    # Write to db; return document
    col_dst.insert_one(doc)
    return doc


def _trim_program_output(s, paths):
    """s: bytes -> str"""
    s = s.decode('utf-8', errors='replace')
    max_bytes = 8 * 1024 ** 2
    assert max_bytes > 128, max_bytes
    if len(s) >= max_bytes:
        s = s[:max_bytes - 128]
        s = s.rsplit('\n', 1)[0] + '\npdf-etl-tool: ERROR: DATA-TRUNCATED'

    # Additionally, replace any instances of paths which might vary with
    # the blank string.
    for p, pname in paths:
        s = re.sub(re.escape(p), pname, s, flags=re.I)
    return s

