"""Utility functionality for FAW pipelines.
"""

from abc import ABCMeta, abstractmethod
from async_generator import asynccontextmanager  # Pre-3.7
import contextlib
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


@asynccontextmanager
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
        async for f in gfs.find({'filename': filename}, {'_id': 1}).sort('uploadDate', -1).skip(1):
            await gfs.delete(f['_id'])


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
        for f in gfs.find({'filename': filename}, {'_id': 1}).sort('uploadDate', -1).skip(1):
            gfs.delete(f['_id'])


class ApiBase(metaclass=ABCMeta):
    """API for exchanging information with the FAW's other 
    components. May be either synchronous (default) or asynchronous if `db_conn`
    is specified as asynchronous.

    Args:
        api_info: API information mapping.
        db_conn: To re-use an existing motor or pymongo connection to the 
                database, specify it here.
    """
    def __init__(self, api_info, db_conn):
        self._api_info = api_info
        self._db_conn = db_conn


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
    def task_get_state(self, taskname=None):
        """Returns ``(version, up_to_date)`` for the given `taskname` within the
        same pipeline.

        If `taskname` is unspecified, use the current task (or error if this is
        not run from a task).
        """


    @abstractmethod
    def task_set_done(self):
        """Sets the current task as done, assuming our version matches the 
        version in the DB.
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
    async def task_file_read(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        gfs = motor_asyncio.AsyncIOMotorGridFSBucket(
                self._db_conn, bucket_name=f'{prefix}fs')
        return await gfs.open_download_stream_by_name(filename)
    async def task_file_write(self, filename, *, taskname=None):
        prefix = self._task_col_prefix(taskname=taskname)
        return _GridFsFileWriter(self._db_conn, f'{prefix}fs', filename)
    async def task_get_state(self, taskname=None):
        metadata = self._task_metadata_col(taskname=taskname)
        doc = await metadata.find_one({'_id': 'computation'})
        if doc is None:
            return None, False
        return doc['version'], doc['done']
    async def task_set_done(self):
        info = self._api_info
        metadata = self._task_metadata_col()
        await metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'done': True}},
        )
    async def destructive__task_change_version(self, version):
        """Internal usage -- delete all task data and change the version number.
        """
        prefix = self._task_col_prefix()
        for col in await self._db_conn.list_collection_names():
            if col.startswith(prefix):
                await self._db_conn.drop_collection(col)

        metadata = self._task_metadata_col()
        await metadata.replace_one(
            {'_id': 'computation'},
            {
                'version': version,
                'done': False,
            },
            upsert=True)



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
    def task_get_state(self, taskname=None):
        metadata = self._task_metadata_col(taskname=taskname)
        doc = metadata.find_one({'_id': 'computation'})
        if doc is None:
            return None, False
        return doc['version'], doc['done']
    def task_set_done(self):
        info = self._api_info
        metadata = self._task_metadata_col()
        metadata.update_one(
                {'_id': 'computation', 'version': info['task_version']},
                {'$set': {'done': True}},
        )
