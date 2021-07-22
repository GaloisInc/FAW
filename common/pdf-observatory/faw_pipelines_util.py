"""Utility functionality for FAW pipelines.
"""

import faw_internal_util

from abc import ABCMeta, abstractmethod
import contextlib
import dataclasses
import gridfs
import json
import motor.motor_asyncio as motor_asyncio
import os
import pymongo
import tempfile
import urllib.request

_FAW_ALL = '__FAW_ALL__'

def _fn_implements(fn_base):
    def wrapper(fn_impl):
        assert fn_base.__name__ == fn_impl.__name__
        if fn_impl.__doc__ is None:
            fn_impl.__doc__ = ''
        fn_impl.__doc__ = fn_base.__doc__ + '\n\n' + fn_impl.__doc__
        return fn_impl
    return wrapper

def _GridFsFileWriter(db, col, filename):
    """Context manager for dealing with open(..., 'wb') functionality. That is,
    it makes a new file, but atomically, which does not affect interim reads.
    """
    is_async = db.__class__.__name__.startswith('AsyncIO')
    if is_async:
        return _GridFsFileWriter_Async(db, col, filename)
    else:
        return _GridFsFileWriter_Sync(db, col, filename)


@contextlib.asynccontextmanager
async def _GridFsFileWriter_Async(db_conn, col, filename):
    gfs = motor_asyncio.AsyncIOMotorGridFSBucket(
            db_conn, bucket_name=col)

    filename_new = '.new_' + filename
    gfs_file = await gfs.open_upload_stream(filename_new)
    try:
        yield gfs_file
    except:
        await gfs_file.abort()  # Allowed after close
        raise
    else:
        gfs_file.close()
        # Contents written successfully; do the swap
        await gfs.rename(gfs_file._id, filename)
        # Delete old
        async for f in gfs.find({'filename': filename}).sort('uploadDate', -1).skip(1):
            await gfs.delete(f._id)


@dataclasses.dataclass
class TaskStatusState:
    version: str
    disabled: bool
    done: bool
    status_msg: str



assert not hasattr(gridfs.GridIn, 'flush'), 'Monkey patch broken'
gridfs.GridIn.flush = lambda self: None

@contextlib.contextmanager
def _GridFsFileWriter_Sync(db_conn, col, filename):
    gfs = gridfs.GridFSBucket(db_conn, bucket_name=col)

    filename_new = '.new_' + filename
    gfs_file = gfs.open_upload_stream(filename_new)
    try:
        yield gfs_file
    except:
        gfs_file.abort()  # Allowed after close
        raise
    else:
        gfs_file.close()
        # Contents written successfully; do the swap
        gfs.rename(gfs_file._id, filename)
        # Delete old
        for f in gfs.find({'filename': filename}).sort('uploadDate', -1).skip(1):
            gfs.delete(f._id)


class ApiBase(metaclass=ABCMeta):
    """API for exchanging information with the FAW's other
    components. May be either synchronous (default) or asynchronous if `db_conn`
    is specified as asynchronous.

    This abstract class, and the synchronous API, are fashioned as user-facing
    APIs. The asynchronous API has additional methods, prefixed with _internal.

    The `file_` methods deal with files available to the FAW for investigation.

    The `task_file_` methods deal with file-like APIs, for storing information
    within the mongo database.

    The `task_status_set_message` method is for debugging progress or other
    information through the FAW UI.

    Args:
        api_info: API information mapping.
        db_conn: To re-use an existing motor or pymongo connection to the
                database, specify it here.
    """
    def __init__(self, api_info, db_conn):
        self._api_info = api_info
        self._db_conn = db_conn


    def get_info(self):
        """Gets the ApiInfo that instantiated this class; may be used to
        create a new `Api` instance somewhere else.
        """
        return self._api_info


    def dask_get_client(self):
        """TODO fix for asynchronous"""
        import dask.distributed as d
        return d.Client(address=self._api_info['dask'],
                # We have direct access to workers anywhere this Api is used
                direct_to_workers=True,
                )


    def _file_col_name(self):
        """Returns the full name of the file collection for this pipeline.
        """
        return self._task_col_prefix(taskname=_FAW_ALL) + 'faw_idx'


    @abstractmethod
    def file_count(self):
        """Returns total count of all files."""


    @abstractmethod
    @contextlib.contextmanager
    def file_fetch(self, filename):
        """Returns locally-accessible, absolute path to `filename`.

        MAY BE DIFFERENT FROM ABSOLUTE PATH SHOWN BY THE FAW!!!

        Will potentially be deleted when the context manager expires.

        Note: may enter an unknown number with:

        ```python
        import contextlib
        with contextlib.ExitStack() as stack:
            file_paths = [stack.enter_context(api.file_fetch(f)) for f in file_names]
            # Do something with file_paths
        ```
        """

    @abstractmethod
    def file_list(self):
        """Retrieve a listing of all files available to the FAW. Suitable for
        smaller distributions, but not huge ones.
        """

    @abstractmethod
    def file_sample(self, n):
        """Returns a sampling of 'n' file names. Uses mongodb under the hood.
        """


    @abstractmethod
    def mongo_collection(self, colname, *, taskname=None):
        """Gets the specified mongodb collection."""


    def pipeline_name(self):
        """When ran inside a pipeline, returns a unique identifier denoting the
        analysis set and pipeline combination.

        Raises KeyError if called outside of a pipeline.
        """
        aset = self._api_info['aset']
        pipe = self._api_info['pipeline']
        return f'{aset}!{pipe}'


    def _task_metadata_col(self, taskname=None):
        col_name = f'{self._task_col_prefix(taskname=taskname)}faw_metadata'
        return self._db_conn[col_name]


    def _task_col_prefix(self, taskname=None, pipeline=None):
        """May set `taskname == _FAW_ALL` to get the pipeline prefix.

        Similar for `pipeline == _FAW_ALL` to get the aset prefix
        """

        info = self._api_info
        aset = info['aset']

        if pipeline is not None:
            # Non-standard usage
            assert pipeline == _FAW_ALL, pipeline
            return f'faw_pipelines_{aset}!__'
        else:
            pipeline = info['pipeline']

        if taskname == _FAW_ALL:
            # Internal hook
            return f'faw_pipelines_{aset}!__{pipeline}!__'

        if taskname is None:
            assert 'task' in info, 'Must specify taskname when API used outside of a task'
            taskname = info['task']

        assert '!' not in aset, aset
        assert '!' not in pipeline, pipeline
        assert '!' not in taskname, taskname
        return f'faw_pipelines_{aset}!__{pipeline}!__{taskname}!__'


    @abstractmethod
    def task_file_list(self, *, taskname=None):
        """
        Returns a list of all file objects written to the given task.

        Compatible with `task_file_read` and `task_file_write`.
        """


    @abstractmethod
    def task_file_read(self, filename, *, taskname=None):
        """
        Returns a file-like object which should be read from and closed.

        The file is always opened in binary mode; that is, if using string
        data, use `str.encode()` before writing, and `bytes.decode()` after
        reading.
        """


    @abstractmethod
    def task_file_write(self, filename, *, taskname=None):
        """
        Returns a file-like object which should be written to and closed. If the
        file already existed, the old version will be deleted on success.

        The file is always opened in binary mode; that is, if using string
        data, use `str.encode()` before writing, and `bytes.decode()` after
        reading.
        """


    def task_name(self):
        """Returns the current task's name; raises a ValueError exception if
        this API info is not associated with a task.
        """
        if 'task' not in self._api_info:
            raise ValueError("Not an API associated with a task")
        return self._api_info['task']


    @abstractmethod
    def task_status_set_message(self, msg):
        """Logs a message for user inspection. Shown from the UI, so this can
        be handy.
        """



def Api(api_info, db_conn=None):
    if db_conn is None:
        # default to Synchronous
        return ApiSync(api_info)
    elif db_conn.__class__.__name__ == 'AsyncIOMotorDatabase':
        raise NotImplementedError("For simplicity, all API now synchronous")
    else:
        return ApiSync(api_info, db_conn)



class ApiSync(ApiBase):
    def __init__(self, api_info, db_conn=None):
        super().__init__(api_info, db_conn)
        if self._db_conn is None:
            self._db_conn = faw_internal_util.mongo_api_info_to_db_conn(
                    api_info['mongo'])


    def file_count(self):
        """Returns total count of all files available to the pipeline."""
        return self._db_conn[self._file_col_name()].count_documents({})


    @contextlib.contextmanager
    def file_fetch(self, filename):
        """Returns locally-accessible, absolute path to `filename`.

        MAY BE DIFFERENT FROM ABSOLUTE PATH SHOWN BY THE FAW!!!

        Will potentially be deleted when the context manager expires.

        Note: may enter an unknown number with:

        ```python
        import contextlib
        with contextlib.ExitStack() as stack:
            file_paths = [stack.enter_context(api.file_fetch(f)) for f in file_names]
            # Do something with file_paths
        ```
        """
        # See if we're local
        if not hasattr(self, '_is_main_node'):
            self._is_main_node = (not os.path.lexists('/home/worker.sh'))

        if self._is_main_node:
            yield os.path.join(self._api_info['pdfdir'], filename)
        else:
            api_info = self._api_info
            host = api_info['hostname']
            port = api_info['hostport']
            url = f'http://{host}:{port}/file_download/' + filename
            data = urllib.request.urlopen(url).read()
            with tempfile.NamedTemporaryFile(suffix=os.path.basename(filename),
                    mode='w+b') as f:
                f.write(data)
                f.flush()
                yield f.name

    def file_list(self):
        """Retrieve a listing of all files available to the FAW. Suitable for
        smaller distributions, but not huge ones.
        """
        api_info = self._api_info
        return [d['_id'] for d in self._db_conn[self._file_col_name()].find()]

    def file_sample(self, n):
        """Returns a sampling of 'n' file names. Uses mongodb under the hood.
        """
        return [
                d['_id']
                for d in self._db_conn[self._file_col_name()].aggregate([
                    {'$sample': {'size': n}},
                ])
        ]

    def mongo_collection(self, colname, *, taskname=None):
        """Gets the specified mongodb collection."""
        prefix = self._task_col_prefix(taskname=taskname)
        return self._db_conn[f'{prefix}{colname}']

    @_fn_implements(ApiBase.task_file_list)
    def task_file_list(self, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        gfs = gridfs.GridFSBucket(
                self._db_conn, bucket_name=f'{prefix}fs')
        files = list(set([v.filename for v in gfs.find()]))
        return files
    @_fn_implements(ApiBase.task_file_read)
    def task_file_read(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        gfs = gridfs.GridFSBucket(
                self._db_conn, bucket_name=f'{prefix}fs')
        return gfs.open_download_stream_by_name(filename)
    @_fn_implements(ApiBase.task_file_write)
    def task_file_write(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        return _GridFsFileWriter(self._db_conn, f'{prefix}fs', filename)
    def task_status_set_message(self, msg):
        info = self._api_info
        metadata = self._task_metadata_col()
        metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'status_msg': msg}})

    # Internals below
    def destructive__purge_aset(self):
        """Purge all data relating to any pipeline in the current aset. Used
        when deleting an aset.
        """
        self._task_col_drop(pipeline=_FAW_ALL, taskname=_FAW_ALL)
    def destructive__purge_pipeline(self):
        """Purge all data relating to this pipeline. Used when disabling for an
        analysis set.
        """
        self._task_col_drop(taskname=_FAW_ALL)
    def destructive__task_change_version(self, version, tasks_downstream):
        """Internal usage -- delete all task data and change the version number.

        This is always called first.

        Internally, OK to call with `tasks_downstream=[]`, if outside of
        pipelines admin loop. In that case, the admin loop will re-call with
        correct version and downstream tasks.
        """

        # First, unset done/version information for us and for downstream tasks
        # so they kick off once we're done, and not before.
        self._task_col_drop()
        for td in tasks_downstream:
            self._task_col_drop(taskname=td)

        metadata = self._task_metadata_col()
        # Keep status information separate for mongodb efficiency
        metadata.replace_one(
            {'_id': 'computation-inspect'},
            {
                'version': version,
            },
            upsert=True)

        # Finally, overwrite our done flag and set the proper version,
        # signalling the reset as complete.
        metadata.replace_one(
            {'_id': 'computation'},
            {
                'version': version,
                'done': False,
                'status_msg': '',
            },
            upsert=True)
    def _internal_db_reparse(self, tools_to_reset):
        api_info = self._api_info
        host = api_info['hostname']
        port = api_info['hostport']
        req = urllib.request.Request(f'http://{host}:{port}/db_reparse',
                data=json.dumps({'tools_to_reset': tools_to_reset}).encode(),
                method='POST')
        with urllib.request.urlopen(req) as f:
            resp = f.read().decode()
        if f.status != 200:
            raise ValueError(f'{f.status} - {resp}')
        return resp
    def _internal_task_status_get_last_run_info(self, taskname=None):
        metadata = self._task_metadata_col(taskname=taskname)
        doc = metadata.find_one({'_id': 'computation-inspect'})
        return doc
    def _internal_task_status_get_state(self, taskname=None):
        metadata = self._task_metadata_col(taskname=taskname)
        doc = metadata.find_one({'_id': 'computation'})
        if doc is None:
            return TaskStatusState(version=None, done=False, disabled=False,
                    status_msg='<<Not yet started>>')
        return TaskStatusState(version=doc['version'], done=doc['done'],
                disabled=doc.get('disabled', False), status_msg=doc['status_msg'])
    def _internal_task_status_set_completed(self):
        info = self._api_info
        metadata = self._task_metadata_col()
        metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'done': True}},
        )
    def _internal_task_status_set_disabled(self, disable):
        info = self._api_info
        metadata = self._task_metadata_col()
        if disable:
            change = {'$set': {'disabled': True}}
        else:
            change = {'$unset': {'disabled': True}}
        metadata.update_one({'_id': 'computation'}, change)
    def _internal_task_status_set_last_run_info(self, call, exitcode, stdout, stderr):
        info = self._api_info
        metadata = self._task_metadata_col()
        metadata.update_one(
                {'_id': 'computation-inspect', 'version': info['task_version']},
                {'$set': {'call': call, 'exitcode': exitcode,
                    'stdout': stdout, 'stderr': stderr}})
    def _task_col_drop(self, taskname=None, pipeline=None):
        prefix = self._task_col_prefix(taskname=taskname, pipeline=pipeline)
        for col in self._db_conn.list_collection_names():
            if col.startswith(prefix):
                self._db_conn.drop_collection(col)

