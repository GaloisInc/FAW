
import dask.distributed
import pymongo

def dask_check_if_cancelled():
    """Checks if current task is cancelled. Useful for breaking long-running
    tasks.

    Should usually be called immediately before work is e.g. committed to
    database, to minimize latency between cancellation and side effects.
    """
    w = dask.distributed.get_worker()
    try:
        # Compatibility with old and new versions of Dask
        if hasattr(w, 'state') and hasattr(w.state, 'tasks'):
            tasks_obj = w.state.tasks
        else:
            tasks_obj = w.tasks
        t = tasks_obj[w.get_current_task()]
        # 2022-11 sometime between now and mid 2021, this became necessary
        if t.state.startswith('cancel'):
            return True
    except KeyError:
        return True

    # Run still required (not cancelled or forgotten)
    return False


def mongo_api_info_to_db_conn(mongo_info):
    assert '/' in mongo_info
    mhost_port, db = mongo_info.split('/')
    mhost, mport = mhost_port.split(':')
    pym_client = pymongo.MongoClient(host=mhost, port=int(mport))
    return pym_client[db]

