"""Utility functionality for FAW pipelines.
"""

class Api:
    """Synchronous API for exchanging information with the FAW's other 
    components.

    Args:
        api_info: API information mapping.
        db_conn: To re-use an existing motor or pymongo connection to the 
                database, specify it here.
    """
    def __init__(self, api_info, db_conn=None):
        self._api_info = api_info
        self._db_conn = db_conn


    def get_task_state(self):
        """Returns the task specified by `self._api_info`'s state, as 
        ``(version, up_to_date)``.
        """
        return None, True


    def get_task_state_ancestor(self, taskname):
        """Returns ``(version, up_to_date)`` for the given `taskname` within the
        same pipeline.
        """
        return None, True


    def destructive__change_task_version(self, version):
        """Clears database + unsets finished flag + sets version to version.
        """
        print('Not impl')
