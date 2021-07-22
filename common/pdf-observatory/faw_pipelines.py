"""Functionality for pipelines.

Basically, each pipeline/task has an administrative thread on the Dask cluster
which tracks calculated versions of dependent tasks and generates a new version
of the task as needed.
"""

import faw_analysis_set
from faw_analysis_set_util import lookup_pipeline_parser_name
from faw_internal_util import dask_check_if_cancelled, mongo_api_info_to_db_conn
import faw_pipelines_util

import asyncio
import collections
import dask.distributed
import functools
import json
import requests
import subprocess
import time
import traceback

def pipeline_admin(app_config_pipelines, api_info, aset_id):
    """Manages the pipelines for a given aset. Loops forever at low yield, so
    removes itself from dask queue.

    api_info: Generic, does not have `aset` set.
    """
    client = dask.distributed.get_client()
    dask.distributed.secede()

    db = mongo_api_info_to_db_conn(api_info['mongo'])
    tasks_running = {}

    while not dask_check_if_cancelled():
        adoc = db['as_metadata'].find_one({'_id': aset_id})
        if adoc is None:
            tasks_running.clear()
        else:
            # Scan through, see active / inactive pipelines. For active
            # pipelines, run outstanding tasks. For inactive, delete the db.
            adoc_pipe = adoc.get('pipelines', {})
            tasks_new = {}
            for pipe_name, pipe_cfg in app_config_pipelines.items():
                if pipe_cfg.get('disabled', False):
                    continue

                pipe_info = api_info.copy()
                pipe_info['aset'] = aset_id
                pipe_info['pipeline'] = pipe_name

                if pipe_name not in adoc_pipe:
                    # Reset / delete all data
                    api = faw_pipelines_util.Api(pipe_info, db)
                    api.destructive__purge_pipeline()
                    continue
                else:
                    # Before running any tasks, ensure that the index collection
                    # exists
                    api = faw_pipelines_util.Api(pipe_info, db)
                    col_name = api._file_col_name()
                    if col_name not in db.list_collection_names():
                        faw_analysis_set.as_create_id_collection(db=db,
                                aset_id=pipe_info['aset'],
                                col_name=col_name,
                                disable_sampling=True)

                tasks = _pipeline_check_tasks(db, client, pipe_info, pipe_name,
                        pipe_cfg, tasks_running)
                tasks_new.update(tasks)
            tasks_running = tasks_new
        time.sleep(1)


def _pipeline_check_tasks(db, client, api_info, pipe_name, pipe_cfg,
        tasks_running):
    """Given some `api_info` which has `aset` and `pipe` set, check all tasks
    for the pipeline and run them as needed.
    """
    pipe_tasks = list(pipe_cfg['tasks'].items())
    pipe_tasks.append(('internal--faw-final-reprocess-db', {
        'version': '1',
        'dependsOn': [p[0] for p in pipe_tasks],
    }))

    # Build complete downstream dependency set -- used to reset downstream
    # data when we re-compute new results.
    pipe_depends_downstream = collections.defaultdict(set)
    for task_name, task_cfg in pipe_tasks:
        for d in task_cfg['dependsOn']:
            pipe_depends_downstream[d].add(task_name)
    while True:
        changed = False
        for k, kv in list(pipe_depends_downstream.items()):
            if k in kv:
                raise ValueError(f'Loop detected: {k} depends on itself')
            for kk in list(kv):
                kkv = pipe_depends_downstream[kk]
                if kkv.issubset(kv):
                    continue
                changed = True
                kv.update(kkv)
        if not changed:
            break

    # Because tasks now can overwrite one another's databases to ensure
    # accuracy, we have to do this serially. Fortunately, pipelines should
    # have a quite limited number of tasks.
    pipe_tasks.sort(key=functools.cmp_to_key(lambda a, b:
            -1 if b[0] in pipe_depends_downstream[a[0]]
            else
                1 if a[0] in pipe_depends_downstream[b[0]]
                else 0))

    tasks_to_continue = {}
    for task_name, task_cfg in pipe_tasks:
        if task_cfg.get('disabled', False):
            # Skip this task
            continue

        combined_name = f'{pipe_name}_{task_name}'
        task_api_info = api_info.copy()
        task_api_info['task'] = task_name

        # Ensure this task is either up-to-date or running
        api = faw_pipelines_util.Api(task_api_info, db)

        needs_run = True
        should_run = True
        last_task_status = api._internal_task_status_get_state()

        if last_task_status.disabled:
            needs_run = False
            should_run = False
        else:
            for dep in task_cfg['dependsOn']:
                task_status = api._internal_task_status_get_state(taskname=dep)
                if not task_status.done:
                    should_run = False

        run_version = task_cfg['version']
        task_api_info['task_version'] = run_version

        # Abort if out of date
        old_future_info = tasks_running.get(combined_name)
        if not should_run:
            if needs_run and task_name == 'internal--faw-final-reprocess-db':
                # Mark as incomplete for UI
                db['as_metadata'].update_one(
                        {'_id': api_info['aset'], 'pipelines.' + pipe_name + '.done': True},
                        {'$set': {'pipelines.' + pipe_name + '.done': False}})
            # Don't let this task run
            continue
        elif run_version != last_task_status.version:
            # Running an update on an old version OR not running at all
            if old_future_info is not None:
                old_future_info['future'].cancel()

                # Wait 1s -- reasonable time to expect the process to have been
                # killed.
                time.sleep(1)

            # Clear database + set version + unset done flag.
            api.destructive__task_change_version(run_version,
                    pipe_depends_downstream[task_name])
        else:
            # DB is on currect version; is it up to date (finished)?
            if last_task_status.done:
                # Done
                continue

            # Not done -- ensure it's running
            if old_future_info is not None and not old_future_info['future'].done():
                # Keep the reference
                tasks_to_continue[combined_name] = old_future_info
                continue

        # If we reach here, we want to launch a new task runner
        if task_api_info['task'] == 'internal--faw-final-reprocess-db':
            # Special case -- signal UI to reprocess DB, deleting our parsers
            # so they reprocess
            tools_to_reset = []
            for k, v in pipe_cfg['parsers'].items():
                # This parser needs to be recomputed when we finish
                if v.get('disabled'):
                    continue
                tools_to_reset.append(lookup_pipeline_parser_name(
                        api_info['aset'], pipe_name, k))
            if tools_to_reset:
                api._internal_db_reparse(tools_to_reset)
            # Mark the pipeline as done in the UI
            db['as_metadata'].update_one(
                    {'_id': api_info['aset'], 'pipelines.' + pipe_name + '.done': False},
                    {'$set': {'pipelines.' + pipe_name + '.done': True}})
            # Set done so the pipeline completes
            api._internal_task_status_set_completed()
            continue


        # Launch a dask task which spawns the FAW task
        future = client.submit(
                lambda: _pipeline_task_run(task_cfg, task_api_info),
                pure=False)
        future_info = {
                'future': future,
        }
        tasks_to_continue[combined_name] = future_info
    return tasks_to_continue


def _pipeline_task_run(task_cfg, api_info):
    """Run this task's executable on this machine.
    """
    # If spawned task uses dask, we don't want to occupy all worker threads.
    # Downside: will increase load on machine.
    dask.distributed.secede()

    # Run the program requested by the task.

    eargs = []
    for e in task_cfg['exec']:
        if e.startswith('<'):
            if e == '<apiInfo>':
                eargs.append(json.dumps(api_info))
            else:
                raise NotImplementedError(e)
        else:
            eargs.append(e)


    p = subprocess.Popen(eargs, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
            cwd=task_cfg['cwd'])

    stdout = None
    stderr = None
    while not dask_check_if_cancelled() and p.poll() is None:
        try:
            stdout, stderr = p.communicate(timeout=1)
        except subprocess.TimeoutExpired:
            pass
        else:
            break

    # Shutting down -- kill program gracefully.
    p.terminate()
    retcode = p.wait()

    stdout = stdout.decode('latin1')
    stderr = stderr.decode('latin1')

    max_out_chars = 4000

    api = faw_pipelines_util.Api(api_info)
    api._internal_task_status_set_last_run_info(
            call={'commandline': eargs, 'cwd': task_cfg['cwd']},
            exitcode=retcode,
            stdout=stdout[-max_out_chars:], stderr=stderr[-max_out_chars:])

    if retcode == 0 and not dask_check_if_cancelled():
        api._internal_task_status_set_completed()

    # Can be helpful to see output in normal console, too
    print(f'output from {api_info}')
    print(stdout)
    print(stderr)
