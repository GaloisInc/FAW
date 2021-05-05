"""Utility functionality for FAW pipelines.
"""

from abc import ABCMeta, abstractmethod
import contextlib
import dataclasses
import gridfs
import motor.motor_asyncio as motor_asyncio
import os
import pymongo
import urllib.request

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


    @abstractmethod
    def file_count(self):
        """Returns total count of all files."""


    @abstractmethod
    @contextlib.contextmanager
    def file_fetch(self, filename):
        """Returns locally-accessible, absolute path to `filename`.

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
        """Returns `None` or the pipeline name.
        """
        return self._api_info.get('pipeline')


    def _task_metadata_col(self, taskname=None):
        col_name = f'{self._task_col_prefix(taskname=taskname)}faw_metadata'
        return self._db_conn[col_name]


    def _task_col_prefix(self, taskname=None):
        info = self._api_info
        pipeline = info['pipeline']
        if taskname is None:
            assert 'task' in info, 'Must specify taskname when API used outside of a task'
            taskname = info['task']

        return f'faw_pipelines_{pipeline}_{taskname}_'


    @abstractmethod
    def task_file_read(self, filename, *, taskname=None):
        """
        Returns a file-like object which should be read from and closed.
        """


    @abstractmethod
    def task_file_write(self, filename, *, taskname=None):
        """
        Returns a file-like object which should be written to and closed. If the
        file already existed, the old version will be deleted on success.
        """


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
        return ApiAsync(api_info, db_conn)
    else:
        return ApiSync(api_info, db_conn)



class ApiAsync(ApiBase):
    async def file_count(self):
        raise NotImplementedError()
    @contextlib.asynccontextmanager
    async def file_fetch(self, filename):
        raise NotImplementedError()
    async def file_list(self):
        raise NotImplementedError()
    async def file_sample(self, n):
        raise NotImplementedError()
    def mongo_collection(self, colname, *, taskname=None):
        raise NotImplementedError()
    async def task_file_read(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        gfs = motor_asyncio.AsyncIOMotorGridFSBucket(
                self._db_conn, bucket_name=f'{prefix}fs')
        return await gfs.open_download_stream_by_name(filename)
    async def task_file_write(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        return _GridFsFileWriter(self._db_conn, f'{prefix}fs', filename)
    async def _internal_task_status_get_last_run_info(self, taskname=None):
        metadata = self._task_metadata_col(taskname=taskname)
        doc = await metadata.find_one({'_id': 'computation-inspect'})
        return doc
    async def _internal_task_status_get_state(self, taskname=None):
        metadata = self._task_metadata_col(taskname=taskname)
        doc = await metadata.find_one({'_id': 'computation'})
        if doc is None:
            return TaskStatusState(version=None, done=False, disabled=False,
                    status_msg='<<Not yet started>>')
        return TaskStatusState(version=doc['version'], done=doc['done'],
                disabled=doc.get('disabled', False),
                status_msg=doc['status_msg'])
    async def _internal_task_status_set_completed(self):
        info = self._api_info
        metadata = self._task_metadata_col()
        await metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'done': True}},
        )
    async def _internal_task_status_set_disabled(self, disable):
        info = self._api_info
        metadata = self._task_metadata_col()
        if disable:
            change = {'$set': {'disabled': True}}
        else:
            change = {'$unset': {'disabled': True}}
        await metadata.update_one(
                # Version not necessary, this is UI-driven
                {'_id': 'computation'},
                change,
        )
    async def _internal_task_status_set_last_run_info(self, call, exitcode, stdout, stderr):
        info = self._api_info
        metadata = self._task_metadata_col()
        await metadata.update_one(
                {'_id': 'computation-inspect', 'version': info['task_version']},
                {'$set': {'call': call, 'exitcode': exitcode,
                    'stdout': stdout, 'stderr': stderr}})
    async def task_status_set_message(self, msg):
        info = self._api_info
        metadata = self._task_metadata_col()
        await metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'status_msg': msg}})
    async def destructive__task_change_version(self, version, tasks_downstream):
        """Internal usage -- delete all task data and change the version number.

        This is always called first.

        Internally, OK to call with `tasks_downstream=[]`, if outside of
        pipelines admin loop. In that case, the admin loop will re-call with
        correct version and downstreama tasks.
        """

        # First, unset done/version information for us and for downstream tasks
        # so they kick off once we're done, and not before.
        await self._task_col_drop()
        for td in tasks_downstream:
            await self._task_col_drop(taskname=td)

        metadata = self._task_metadata_col()
        # Keep status information separate for mongodb efficiency
        await metadata.replace_one(
            {'_id': 'computation-inspect'},
            {
                'version': version,
            },
            upsert=True)

        # Finally, overwrite our done flag and set the proper version,
        # signalling the reset as complete.
        await metadata.replace_one(
            {'_id': 'computation'},
            {
                'version': version,
                'done': False,
                'status_msg': '',
            },
            upsert=True)


    async def _task_col_drop(self, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        for col in await self._db_conn.list_collection_names():
            if col.startswith(prefix):
                await self._db_conn.drop_collection(col)



class ApiSync(ApiBase):
    def __init__(self, api_info, db_conn=None):
        super().__init__(api_info, db_conn)
        if self._db_conn is None:
            assert '/' in api_info['mongo']
            mhost_port, db = api_info['mongo'].split('/')
            mhost, mport = mhost_port.split(':')
            pym_client = pymongo.MongoClient(host=mhost, port=int(mport))
            self._db_conn = pym_client[db]


    def file_count(self):
        """Returns total count of all files."""
        api_info = self._api_info
        host = api_info['hostname']
        port = api_info['hostport']
        url = f'http://{host}:{port}/file_count'
        data = urllib.request.urlopen(url).read().decode()
        return int(data)

    @contextlib.contextmanager
    def file_fetch(self, filename):
        """Returns locally-accessible, absolute path to `filename`.

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
            self._is_main_node = (
                os.path.lexists('/home/pdf-observatory/main.py')
                and os.path.lexists('/home/config.json'))

        if self._is_main_node:
            yield os.path.join(self._api_info['pdfdir'], filename)
        else:
            raise NotImplementedError()

    def file_list(self):
        """Retrieve a listing of all files available to the FAW. Suitable for
        smaller distributions, but not huge ones.
        """
        api_info = self._api_info
        host = api_info['hostname']
        port = api_info['hostport']
        url = f'http://{host}:{port}/file_list'
        data = urllib.request.urlopen(url).read().decode()
        return data.strip().split('\n')

    def file_sample(self, n):
        """Returns a sampling of 'n' file names. Uses mongodb under the hood.
        """
        return [
                d['_id']
                for d in self._db_conn['observatory'].aggregate([
                    {'$sample': {'size': n}},
                    {'$project': {'_id': 1}},
                ])
        ]

    def mongo_collection(self, colname, *, taskname=None):
        """Gets the specified mongodb collection."""
        prefix = self._task_col_prefix(taskname=taskname)
        return self._db_conn[f'{prefix}{colname}']

    def task_file_read(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        gfs = gridfs.GridFSBucket(
                self._db_conn, bucket_name=f'{prefix}fs')
        return gfs.open_download_stream_by_name(filename)
    def task_file_write(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        return _GridFsFileWriter(self._db_conn, f'{prefix}fs', filename)
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
        metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                change,
        )
    def _internal_task_status_set_last_run_info(self, call, exitcode, stdout, stderr):
        info = self._api_info
        metadata = self._task_metadata_col()
        metadata.update_one(
                {'_id': 'computation-inspect', 'version': info['task_version']},
                {'$set': {'call': call, 'exitcode': exitcode,
                    'stdout': stdout, 'stderr': stderr}})
    def task_status_set_message(self, msg):
        info = self._api_info
        metadata = self._task_metadata_col()
        metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'status_msg': msg}})

