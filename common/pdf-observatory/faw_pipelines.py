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
import traceback

_config = None

async def main_loop(app_mongodb_conn, app_config, get_api_info_fn):
    global _config
    _config = app_config

    client = await dask.distributed.Client('localhost:8786', asynchronous=True)
    while True:
        try:
            await _pipeline_spawn_admins(app_mongodb_conn, client,
                    get_api_info_fn)
        except:
            traceback.print_exc()
            # Never abort.
        await asyncio.sleep(0.5)


def config_update(new_app_config):
    """Updates faw_pipelines' understanding of the app config.
    """
    global _config
    _config = new_app_config


def submit_with_self_future(client, fn, *args, **kwargs):
    @functools.wraps(fn)
    def unwrap_self_future(self_var, *args, **kwargs):
        self_future = self_var.get(10)  # Raises TimeoutError if issue
        self_var.delete()
        return fn(self_future, *args, **kwargs)
    
    v = dask.distributed.Variable(client=client)
    t = client.submit(unwrap_self_future, v, *args, **kwargs)
    
    if not client.asynchronous:
        v.set(t)
        return t
    else:
        async def inner():
            await v.set(t)
            return t
        return inner()


async def _pipeline_spawn_admins(mongodb_conn, dask_client, get_api_info_fn):
    """Loop through all tasks, running them as needed.
    """
    name_prefix = 'faw-task_'
    api_info = get_api_info_fn()

    promises = []
    for pipe_name, pipe_cfg in _config['pipelines'].items():
        for task_name, task_cfg in pipe_cfg['tasks'].items():
            combined_name = f'{name_prefix}{pipe_name}_{task_name}'
            task_api_info = api_info.copy()
            task_api_info['pipeline'] = pipe_name
            task_api_info['task'] = task_name
            task_api_info['db_prefix'] = combined_name
            promises.append(_pipeline_spawn_task(mongodb_conn, dask_client, 
                    task_cfg, task_api_info))
    await asyncio.wait(promises)


async def _pipeline_spawn_task(mongodb_conn, dask_client, task_cfg, 
        task_api_info):
    """Ensure that a given task is either up-to-date or running.
    """
    # Figure out the correct version of this task. There are two versions:
    # the config file version, and the data version. Each pipeline task depends
    # on its own config file version and the data versions of its dependents.
    # Furthermore, a task must only run if its dependents are up to date.
    api = faw_pipelines_util.Api(task_api_info, mongodb_conn)

    should_run = True
    run_version = [task_cfg['version']]
    old_version, task_is_up_to_date = api.get_task_state()

    for dep in task_cfg['dependsOn']:
        version, up_to_date = api.get_task_state_ancestor(dep)
        if not up_to_date:
            should_run = False
        
        run_version.append(version)

    run_version = '__'.join([str(s) for s in run_version])

    # Find running task
    future = None
    try:
        future = await dask_client.datasets[task_api_info['db_prefix']]
    except KeyError:
        pass

    # Abort if out of date
    if not should_run:
        if future is not None:
            await future.cancel()
        return
    elif run_version != old_version:
        # Running an update on an old version
        if future is not None:
            await future.cancel()
            await dask_client.unpublish_dataset(task_api_info['db_prefix'])
            # Wait 5s -- reasonable time to expect the process to have been 
            # killed.
            await asyncio.sleep(5)

        # Clear database + set version + unset done flag.
        api.destructive__change_task_version(run_version)
    else:
        # DB is on currect version; is it up to date (finished)?
        if task_is_up_to_date:
            # Done
            return

        # Not done -- ensure it's running
        if future is None:
            pass
        elif future.status == 'pending':
            return
        else:
            # Stopped or crashed -- re-run
            await dask_client.unpublish_dataset(task_api_info['db_prefix'])

    # If we reach here, we want to launch a new task runner
    await dask_client.publish_dataset(
        await submit_with_self_future(
            dask_client,
            lambda self_future: _pipeline_task_run(self_future, task_cfg, 
                task_api_info),
            pure=False,
        ),
        name=task_api_info['db_prefix'])


async def _pipeline_task_run(self_future, task_cfg, api_info):
    """Run this task's executable on this machine.
    """
    # If spawned task uses dask, we don't want to occupy all worker threads.
    # Downside: may increase load on machine.
    dask.distributed.secede()

    print(f'Pipeline for {api_info}')

    while not self_future.cancelled():
        time.sleep(1)
