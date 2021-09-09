"""Manages parsing within analysis sets.
"""

import faw_analysis_set_util
import faw_internal_util
import faw_pipelines_util

import dask.distributed
import json
import os
import pymongo
import re
import subprocess
import tempfile
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

def as_parse_main(app_config, api_info):
    """Continuously goes through `COL_NAME` to find documents which need to be
    parsed; distributes files to workers by batch.
    """
    mongo_info = api_info['mongo']
    db = faw_internal_util.mongo_api_info_to_db_conn(mongo_info)
    col = db[COL_NAME]

    col.create_index([('error_until', 1)])
    col.create_index([('priority_order', 1)])

    with dask.distributed.worker_client() as client:
        # We have to track everything here, rather than relying on `pure=True`.
        # This is a result of
        # So, track outstanding ids for e.g. transient errors.
        outstanding_docs = []  # (doc, future)
        outstanding = dask.distributed.as_completed(with_results=True,
                raise_errors=False)

        [app_config_future] = client.scatter([app_config], broadcast=True)

        work_max = 0
        last_update = 0.
        def next_work():
            nonlocal last_update, work_max
            t = time.monotonic()
            if t - last_update > 10:
                # Update cluster size
                work_max = 0
                for w in client.scheduler_info()['workers'].values():
                    work_max += 4 * w['nthreads']
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
            for doc in data:
                future = client.submit(_dask_as_parse_file,
                        app_config_future, api_info, doc, pure=False)
                outstanding_docs.append((doc, future))
                outstanding.add(future)

        while True:
            # Before starting a new round, clear out errors that are old
            col.update_many({'error_until': {'$lt': time.time()}},
                    {'$unset': {'error_until': True}})

            # Initial load
            next_work()
            # Iterator will keep us from maxing out CPU
            for f, f_result in outstanding:
                if faw_internal_util.dask_check_if_cancelled():
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
                    # Success -- clear `error_delay`
                    col.update_one({'_id': f_doc['_id']},
                            {'$unset': {'error_delay': True}})

                # Purely for dask dashboard; prevent future from hanging around
                del f

                next_work()

            # Done with all work; sleep awhile and see if there's more (already
            # seceded, so don't worry about that)
            time.sleep(2)


def _dask_as_parse_file(app_config, api_info, doc):
    """Fetch the given file with API, and run required parser(s) on it.

    When done, remove specific versions from queue.
    """
    db = faw_internal_util.mongo_api_info_to_db_conn(api_info['mongo'])
    col = db[COL_NAME]

    doc_id = doc['_id']
    doc_invname = f'/home/pdf-files/{doc_id}'
    parsers_done = {}  # {name: [tool, parser]}
    parsers_to_run_tool = {}
    parsers_to_run_parser = {}

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
            vv = versions_keyed.setdefault(v['invoker']['invName'],
                    [[v['invoker']['version'], None], [], []])
            # Should only be one, never know. Better to have self-healing db
            vv[1].append(v)
            if v['result']['_cons'].lower() not in ('goodresult', 'timeout'):
                # Want to re-run this one
                vv[0][0] = None
        for v in invpar:
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
                db_dst='/invocationsparsed', parsers_config=parsers_config)

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


def _as_run_tool(col_dst, fpath, fpath_tool_name, parser_inv_name,
        parser_tool_version, parser_cfg, api_info, *, timeout_default):
    """Re-implementation of pdf-etl-tool.
    """

    timeout = parser_cfg['timeout'] or timeout_default

    if 'pipeline' in parser_cfg:
        ok = parser_tool_version.startswith(parser_cfg['version'])
    else:
        ok = parser_tool_version == parser_cfg['version']

    if not ok:
        raise ValueError(f'Requested parse {parser_tool_version}; config version {parser_cfg["version"]}')

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
            else:
                args.append(e)

        t_start = time.monotonic()
        p = subprocess.Popen(args,
                stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                cwd=f"/home/dist/{parser_cfg['cwd']}")

        finished = False
        timed_out = False
        try:
            # Subprocess may want to spawn dask workers; ensure it may
            dask.distributed.secede()
            while not faw_internal_util.dask_check_if_cancelled():
                try:
                    stdout, stderr = p.communicate(timeout=2)
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

        if not finished or timed_out:
            # Early stop -- kill, abort
            p.kill()
            stdout, stderr = p.communicate()

        if not finished:
            # Dask requested we cancel -- do not write to DB
            return

        doc['result']['timeElapsed'] = time.monotonic() - t_start
        doc['result']['exitcode'] = p.wait()
        paths = [(t.name, '<tempFile>') for t in temps] + [(fpath, '<inputFile>')]
        doc['result']['stdoutRes'] = _trim_program_output(stdout, paths)
        doc['result']['stderrRes'] = _trim_program_output(stderr, paths)
        doc['result']['_cons'] = 'Timeout' if timed_out else 'GoodResult'
    finally:
        for t in temps:
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
    if len(s) >= 8 * 1024 ** 2:
        s = s[:8 * 1024 ** 2 - 128]
        s = s.rsplit('\n', 1)[0] + '\npdf-etl-tool: ERROR: DATA-TRUNCATED'

    # Additionally, replace any instances of paths which might vary with
    # the blank string.
    for p, pname in paths:
        s = re.sub(re.escape(p), pname, s, flags=re.I)
    return s

