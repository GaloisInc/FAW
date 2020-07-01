"""A helper which runs a MongoDB-queue consumer.

Note that, if this gets too bogged down, the collection would need to be
multiplexed (queue1, queue2, ...) to avoid mongoDB's write-lock bottleneck.
Current design should make that fairly straightforward...

"""
# System-level imports
import bson
import click
import datetime
import functools
import itertools
import logging
import multiprocessing
import os
import pymongo
import sys
import threading
import time
import traceback

class ProcessException(Exception):
    pass


class _ProperProcess(multiprocessing.Process):
    """Same as threading.Thread... see _ProperThread, below."""
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self._pconn, self._cconn = multiprocessing.Pipe()
        self._exception = None


    def run(self):
        try:
            super().run()
        except Exception as e:
            tb = traceback.format_exc()
            self._cconn.send((e, tb))
        else:
            self._cconn.send(None)
        finally:
            self._cconn.close()


    def join(self):
        self._exception = self._pconn.recv()
        super().join()
        # Connections are closed on garbage collect
        if self._exception is not None:
            raise ProcessException(self._exception[1])


class _ProperThread(threading.Thread):
    """Shame on you, Python: https://bugs.python.org/issue36666"""
    def run(self):
        self.exc = None
        try:
            self.ret = self._target(*self._args, **self._kwargs)
        except Exception as e:
            self.exc = e
    def join(self):
        super().join()
        if self.exc is not None:
            raise self.exc
        return self.ret


def run(live_mode, processes, threads, clean, db_src, db_queue,
        processing_timeout, handle_queue_callback, handle_clean_callback=None,
        handle_init_callback=None, run_one=None, allow_exceptions=False):
    """Runs a mongoDB queue consumer.

    Note that, since MongoDB cannot index missing fields, db_src should match
    db_queue only if the needed metadata fields ('queueStart', 'queueStop',
    and 'queueErr') have been set to ``None`` in the source documents.

    Args:
        live_mode: True to run indefinitely; False to run only until currently
                queued items are exhausted.
        processes: Number of processes to run, or 0 for number of cores.
        threads: Number of threads per process, useful for preventing downtime
                due to IO.
        clean: True to clean (delete) all queue-related data, which would
                force all items to be reprocessed.
        db_src: Path (as in, 'localhost:27017/db/collection') to collection
                with source documents (each of which indicate a piece of
                work to be processed by this consumer).
        db_queue: Path (as in, 'localhost:27017/db/collection') to collection
                with queue meta-information.  If != db_src, then all OK.  If
                same as db_src, ensure that documents inserted into db_src have
                keys 'queueStart', 'queueStop', and 'queueErr' set to ``None``.
                They must not be missing.
        processing_timeout: Time (in seconds) after which a processing task
                is assumed to have timed out, and should be retried.
        handle_clean_callback: Function to call with (conn_resolver) when
                a clean was requested.  Can, for instance, drop the results
                collection, making way for new results.
        handle_queue_callback: Function to call with (doc, conn_resolver).
                ``doc`` is the queue document to be processed.
                ``conn_resolver`` is a lambda which takes a collection path
                and resolves it to a ``pymongo`` Collection object, using
                appropriate pooling and caching.
        handle_init_callback: Function to call with (conn_resolver) when
                initializing the queue consumer.  Useful for creating indexes.
        run_one: If not ``None``, the ``_id`` of a doc to (re-)process,
                regardless of current done state in db.
        allow_exceptions: If ``True``, do not crash on an exception, instead
                logging the exception in the queue collection.
    """
    if processes == 0:
        processes = multiprocessing.cpu_count()

    if clean:
        p = _ProperProcess(target=handle_queue_clean,
                args=(db_src, db_queue, handle_clean_callback))
        p.start()
        p.join()

    if handle_init_callback is not None:
        # Perform exactly once - not once per process.
        # And do it first in case it's an index.
        t = _ProperProcess(target=handle_init_callback,
                args=(_db_collection,))
        t.start()
        t.join()

    # Per machine, spin up exactly one thread to deal with the fact that the
    # 'queue' collection is the 'dst' collection, not the 'src' collection.
    # This code looks a little weird to allow the same code to be used for the
    # more efficient connection.
    procs = []
    if db_src != db_queue:
        if not live_mode:
            # Wait for it to finish before parsing; PyMongo requires that
            # processes fork BEFORE connecting to a db, so do this in a
            # separate process
            t = _ProperProcess(target=handle_queue_stubs,
                    args=(live_mode, db_src, db_queue, run_one))
            t.start()
            t.join()
        else:
            # Local thread OK, won't be started immediately
            t = _ProperThread(target=handle_queue_stubs, args=(live_mode,
                    db_src, db_queue, run_one))
            procs.append(t)

    if run_one:
        # Run this doc, then quit.
        handle_queue_one(run_one, db_src, db_queue, handle_queue_callback)
        return

    kw = {
            'live_mode': live_mode,
            'threads': threads,
            'db_src': db_src,
            'db_queue': db_queue,
            'processing_timeout': processing_timeout,
            'handle_queue_callback': handle_queue_callback,
            'allow_exceptions': allow_exceptions,
            'parent_pid': os.getpid(),
    }
    procs = (
            [
                _ProperProcess(target=handle_queue_process, kwargs=kw)
                for _ in range(processes)
            ]
            # As per pymongo docs, multiprocessing should precede other
            # database access.  Doing this makes the stub thread run last.
            + procs)
    [p.start() for p in procs]
    [p.join() for p in procs]


def handle_queue_clean(db_src, db_queue, handle_clean_callback):
    """Cleans up db_queue so no entries are marked as started or completed.
    """
    conn = _db_collection(db_queue)
    if db_src != db_queue:
        conn.drop()
    else:
        conn.update_many({}, {
            '$set': {
                'queueStart': None,
                'queueStop': None,
                'queueErr': None,
            },
        })
    # Handle user code
    if handle_clean_callback is not None:
        handle_clean_callback(_db_collection)


def handle_queue_stubs(live_mode, db_src, db_queue, run_one):
    """Fills ``db_queue`` with one doc per entry in ``db_src``, where the _id
    fields match and the entries in ``db_queue`` have the requisite
    fields ``queueStart``, ``queueStop``, and ``queueErr``.

    Less efficient than when db_src == db_queue, but more semantically clear.
    """
    log = logging.getLogger('handle_queue_stubs')
    conn_src = _db_collection(db_src)
    conn_queue = _db_collection(db_queue)

    up_to_date_shown = False
    query = None if run_one is None else {'_id': run_one}

    while True:
        batch_size = 1024
        with conn_src.find(
                query,
                # Only get _id field.
                projection=[],
                ) as cursor:
            cursor.batch_size(batch_size)
            done = False
            while not done:
                batch = []
                try:
                    while len(batch) < batch_size:
                        batch.append(next(cursor))
                except StopIteration:
                    done = True

                if not batch:
                    break

                # Update all docs with the queueStart and queueStop fields
                for d in batch:
                    d['queueStart'] = None
                    d['queueStop'] = None
                try:
                    conn_queue.insert_many(batch)
                except pymongo.errors.BulkWriteError as e:
                    # BulkWriteError for duplicate ID, almost certainly.
                    pass

        # All up-to-date.  Wait for more docs?
        if not up_to_date_shown:
            up_to_date_shown = True
            log.debug('All items enqueued.')
        if not live_mode:
            return

        # OK to wait a second, lower CPU usage.
        time.sleep(1)


def handle_queue_process(live_mode, threads, db_src, db_queue,
        processing_timeout, handle_queue_callback, allow_exceptions,
        parent_pid):
    """Multiprocessing target - runs ``threads`` which process invocations
    stored in ``db_src``, whose metadata for queueing is stored in ``db_queue``,
    and create results in some other collection.
    """

    # Database init - no point in doing it per thread
    conn_queue = _db_collection(db_queue)
    _db_collection_init_queue(conn_queue)

    def die_predicate():
        # Interestingly, the "Reprocess DB Errors" button would not halt other
        # processing before. This was a direct result of the fact that these
        # processes keep running after their parent process dies. We could rely
        # on prctl(PR_SET_PDEATHSIG), but there's a window of time during which
        # that result is invalid, so relying on the passed `parent_pid` is more
        # reliable.
        return os.getppid() != parent_pid

    kw = {
            'live_mode': live_mode,
            'db_src': db_src,
            'db_queue': db_queue,
            'processing_timeout': processing_timeout,
            'handle_queue_callback': handle_queue_callback,
            'allow_exceptions': allow_exceptions,
            'die_predicate': die_predicate,
    }
    thread_objs = []
    for i in range(threads):
        k = kw.copy()
        k['thread_id'] = i
        thread_objs.append(_ProperThread(target=handle_queue_thread, kwargs=k))
    [t.start() for t in thread_objs]
    [t.join() for t in thread_objs]


def handle_queue_thread(live_mode, thread_id, db_src, db_queue,
        processing_timeout, handle_queue_callback, allow_exceptions,
        die_predicate):
    log = logging.getLogger(f'handler-{os.getpid()}-{thread_id}')

    conn_src = _db_collection(db_src)
    conn_queue = _db_collection(db_queue)

    log.debug('starting')
    while not die_predicate():
        time_start = datetime.datetime.utcnow().timestamp()
        doc = conn_queue.find_one_and_update(
                {
                    # Find items not yet completed
                    'queueStop': {'$type': 10},
                    # Which are either...
                    '$or': [
                        # Not yet started
                        {'queueStart': {'$type': 10}},
                        # Or started so long ago the task must certainly be
                        # dead or stuck.
                        {'queueStart': {'$lt': time_start - processing_timeout}},
                    ],
                },
                # Stake a claim on this task
                update={'$set': {'queueStart': time_start}},
                new=True,
        )
        if doc is None:
            if not live_mode:
                # All docs handled.
                return
            # Lower CPU usage
            time.sleep(1)
            continue
        e = handle_queue_doc(log, doc, handle_queue_callback, db_src, db_queue,
                die_predicate)
        if not allow_exceptions and e is not None:
            raise ValueError(f'Handling failed for {doc["_id"]}') from e


def handle_queue_one(doc_id, db_src, db_queue, handle_queue_callback):
    """Process one document in the queue, regardless of its current state.
    """
    log = logging.getLogger(f'handler-one')
    conn_queue = _db_collection(db_queue)

    time_start = datetime.datetime.utcnow().timestamp()
    doc_ids = [doc_id]
    try:
        # Also try BSON object ID, if this string works as one.
        bson_doc_id = bson.objectid.ObjectId(doc_id)
        doc_ids.append(bson_doc_id)
    except bson.errors.InvalidId:
        pass
    for doc_id in doc_ids:
        doc = conn_queue.find_one_and_update(
                {'_id': doc_id},
                update={'$set': {
                    'queueStart': time_start,
                    'queueStop': None,
                    'queueErr': None,
                }},
                new=True,
        )
        if doc is not None:
            break
    if doc is None:
        raise ValueError(f'No such doc in {conn_queue}? {doc_id}.  Note that '
                f'`run_one` should not be co-specified with `clean`.')
    die_predicate = lambda: False
    e = handle_queue_doc(log, doc, handle_queue_callback, db_src, db_queue,
            die_predicate)
    if e is not None:
        raise Exception('Handling failed; see above')


def handle_queue_doc(log, doc, handle_queue_callback, db_src, db_queue,
        die_predicate):
    """Given a doc in the db_queue collection (``doc``), and a user-space
    callback for dealing with queue data (``handle_queue_callback``), fetch
    needed information from the source collection and run the user code.

    Args:
        die_predicate: Checked before writing successful result to DB. If True,
                then instead of writing successful result, roll back queue
                document to all None before continue. This is distributed-safe.

    Returns the Exception raised in processing the document, if any.
    """
    log.debug(f'handling {doc["_id"]}')
    exc_task = None
    exc_task_str = None
    try:
        src_doc = doc
        if db_src != db_queue:
            # Using another collection as an intermediary
            conn_src = _db_collection(db_src)
            src_doc = conn_src.find_one(doc['_id'])
            if src_doc is None:
                raise Exception("Could not find source document")

        handle_queue_callback(src_doc, _db_collection)
    except Exception as e:
        exc_task = e
        exc_task_str = traceback.format_exc()
        log.exception(f'exception handling {doc["_id"]}')
    except:
        # Catch e.g. SIGINT -- very important we set exc_task_str so that
        # the below call to `find_one_and_update` doesn't make it look like this
        # task succeeded
        exc_task = Exception(f'Unknown, external exception handling {doc["_id"]}')
        exc_task_str = ''.join(traceback.format_stack() + [str(exc_task)])
        log.error(exc_task_str)
    finally:
        conn_queue = _db_collection(db_queue)
        if die_predicate():
            # Die, do NOT write result (whether success or fail)
            conn_queue.find_one_and_update(
                    {'_id': doc['_id'], 'queueStop': {'$type': 10}},
                    update={'$set': {
                        'queueStart': None,
                        'queueErr': None,
                    }},
            )
        else:
            # Mark that we've finished, and whether or not there was an error
            time_end = datetime.datetime.utcnow().timestamp()
            conn_queue.find_one_and_update(
                    # If someone else finished first, that's OK, keep their
                    # data.
                    {'_id': doc['_id'], 'queueStop': {'$type': 10}},
                    update={'$set': {
                        'queueStop': time_end,
                        'queueErr': exc_task_str,
                    }},
            )
    return exc_task


@functools.lru_cache(None)
def _db_client(hostport):
    """Returns a (pooled) pymongo.MongoClient object for the given host:port
    combination.
    """
    host, port = hostport.split(':')
    if not port:
        port = 27017
    else:
        port = int(port)
    return pymongo.MongoClient(host=host, port=port)


@functools.lru_cache(None)
def _db_collection(connstr):
    """Returns a pymongo.collection.Collection object for the given connection
    string.
    """
    hostport, db, coll = connstr.split('/')

    client = _db_client(hostport)
    client_coll = client[db][coll]
    return client_coll


def _db_collection_init_queue(coll):
    # Create an index for not-yet-completed items.
    coll.create_index([('queueStart', 1)],
        # $type: 10 corresponds with documents where null is explicitly
        # set, rather than missing (which would be the default meaning of
        # ``None``).
        partialFilterExpression={'queueStop': {'$type': 10}},
    )

