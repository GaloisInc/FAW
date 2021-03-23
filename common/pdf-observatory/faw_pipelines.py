"""Functionality for pipelines.

Basically, each task has an administrative thread on the Dask cluster which 
tracks calculated versions of dependent tasks and generates a new version of
the task as needed.

Note that dask will automatically re-submit canceled work, so we're OK on that
front. We (ab)use `dask.distributed.Client.datasets` to track running 
administrative threads as futures.
"""

import faw_pipelines_util

import asyncio
import dask.distributed
import functools
import json
import subprocess
import time
import traceback

_config = None  # pipelines key of config
_reparse_db_fn = None  # Async function ... a hack, basically.

async def main_loop(app_mongodb_conn, app_config_pipelines, get_api_info_fn, 
        reparse_db_fn):
    global _config
    global _reparse_db_fn
    _config = app_config_pipelines
    _reparse_db_fn = reparse_db_fn

    client = await dask.distributed.Client('localhost:8786', asynchronous=True)
    current_future_info = {}
    while True:
        try:
            current_future_info = await _pipeline_spawn_admins(
                    current_future_info,
                    app_mongodb_conn, client, get_api_info_fn)
        except:
            traceback.print_exc()
            # Never abort.
        await asyncio.sleep(0.5)


def config_update(new_app_config_pipelines):
    """Updates faw_pipelines' understanding of the app config.
    """
    global _config
    _config = new_app_config_pipelines


def dask_check_if_cancelled():
    """Checks if current task is cancelled. Useful for breaking long-running
    tasks.

    Should usually be called immediately before work is e.g. committed to 
    database, to minimize latency between cancellation and side effects.
    """
    w = dask.distributed.get_worker()
    try:
        w.tasks[w.get_current_task()]
    except KeyError:
        return True

    # Run still required (not cancelled or forgotten)
    return False


async def _pipeline_spawn_admins(current_future_info, mongodb_conn, dask_client, 
        get_api_info_fn):
    """Loop through all tasks, (re)running them as needed.
    """
    api_info = get_api_info_fn()

    promises = []
    # Make a brand new dictionary for new futures; this allows old futures which
    # don't correspond to entries in the updated config to be forgotten, which
    # will cancel them.
    new_future_info = {}
    for pipe_name, pipe_cfg in _config.items():
        if pipe_cfg.get('disabled', False):
            # Don't run this pipeline
            continue

        pipe_tasks = list(pipe_cfg['tasks'].items())
        pipe_tasks.append(('internal--faw-final-reprocess-db', {
            'version': '1',
            'dependsOn': [p[0] for p in pipe_tasks],
        }))
        for task_name, task_cfg in pipe_tasks:
            combined_name = f'{pipe_name}_{task_name}'
            task_api_info = api_info.copy()
            task_api_info['pipeline'] = pipe_name
            task_api_info['task'] = task_name
            promises.append(_pipeline_spawn_task(mongodb_conn, dask_client, 
                    task_cfg, task_api_info, 
                    old_future_info=current_future_info.get(combined_name),
                    new_future_info_cb=functools.partial(
                        lambda x, *, combined_name: new_future_info.update(
                            {combined_name: x}),
                        combined_name=combined_name)))
    if promises:
        await asyncio.wait(promises)
    return new_future_info


async def _pipeline_spawn_task(mongodb_conn, dask_client, task_cfg, 
        task_api_info, old_future_info, new_future_info_cb):
    """Ensure that a given task is either up-to-date or running.
    """
    # Figure out the correct version of this task. There are two versions:
    # the config file version, and the data version. Each pipeline task depends
    # on its own config file version and the data versions of its dependents.
    # Furthermore, a task must only run if its dependents are up to date.
    api = faw_pipelines_util.Api(task_api_info, mongodb_conn)

    should_run = True
    run_version = [task_cfg['version']]
    old_version, task_is_up_to_date = await api.task_get_state()

    for dep in task_cfg['dependsOn']:
        version, up_to_date = await api.task_get_state(taskname=dep)
        if not up_to_date:
            should_run = False
        
        run_version.append(version)

    run_version = '__'.join([str(s) for s in run_version])
    task_api_info['task_version'] = run_version

    # Abort if out of date
    if not should_run:
        if old_future_info is not None:
            await old_future_info['future'].cancel()
        return
    elif run_version != old_version:
        # Running an update on an old version OR not running at all
        if old_future_info is not None:
            await old_future_info['future'].cancel()

            # Wait 1s -- reasonable time to expect the process to have been 
            # killed.
            await asyncio.sleep(1)

        # Clear database + set version + unset done flag.
        await api.destructive__task_change_version(run_version)
    else:
        # DB is on currect version; is it up to date (finished)?
        if task_is_up_to_date:
            # Done
            return

        # Not done -- ensure it's running
        if old_future_info is not None and not old_future_info['future'].done():
            # Keep the reference
            new_future_info_cb(old_future_info)
            return

    # If we reach here, we want to launch a new task runner
    if task_api_info['task'] == 'internal--faw-final-reprocess-db':
        # Special case -- reprocess DB, set done
        await _reparse_db_fn()
        await api.task_set_done()
        return

    # Normal case -- spawn a dask task which spawns the FAW task.
    future = dask_client.submit(
            lambda: _pipeline_task_run(task_cfg, task_api_info),
            pure=False)

    future_info = {
        'future': future,
    }
    new_future_info_cb(future_info)


def _pipeline_task_run(task_cfg, api_info):
    """Run this task's executable on this machine.
    """
    # If spawned task uses dask, we don't want to occupy all worker threads.
    # Downside: may increase load on machine.
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

    print(f'output from {api_info}')
    print(stdout.decode('latin1'))
    print(stderr.decode('latin1'))
