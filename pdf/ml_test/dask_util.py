"""
For straightforward computation, Dask is straightforward: just use the delayed
API: https://docs.dask.org/en/latest/delayed-best-practices.html

However, if you have a large resource that takes a long time to load, dask
provides the Actor API (https://distributed.dask.org/en/latest/actors.html).
This gives us a means of allocating an instance of a thing (CachedParserInstance
for this file), and managing communication with that thing using dask.
"""

import asyncio
import dask.distributed
import faw_pipelines_util
import io
import os, sys, pickle
import random
import threading
import time
import torch

class CachedParser:
    def __init__(self):
        raise NotImplementedError('static')

    @classmethod
    def get(cls, api, filename, taskname=None, ttl=600):
        """Returns a Dask pointer to an Actor which has the specified model
        loaded in eval mode.

        This function is a bit complicated because we generate a race condition
        to attempt to enforce a singleton constraint.
        """
        client = api.dask_get_client()
        name = cls._cached_name(api, filename, taskname)

        actor = None
        actor_var = dask.distributed.Variable(name=name, client=client)

        # Outer loop to prevent making too many instances... this is an intended
        # race condition, though.
        while True:
            future = None
            try:
                future = actor_var.get(timeout=1 + random.random())
            except asyncio.exceptions.TimeoutError:
                pass
            else:
                try:
                    # Guaranteed OK based on assignment below
                    actor = future.result()
                except:
                    # Fall through to creating a new actor. This branch happens when
                    # the __init__ method for `CachedParserInstance` crashes.
                    pass
                else:
                    # Create new if actor is out of TTL time.
                    age = actor.get_age_since_use().result()
                    if age < ttl:
                        return actor

            # We want to create a new actor
            future = client.submit(CachedParserInstance, api_info=api.get_info(),
                    actor=True)
            actor_var.set(future)

            time.sleep(1.)
            if future.key != actor_var.get().key:
                # Someone else has stamped on our initialization, so go back
                # to top of loop
                continue

            # Ensure we see raised errors. Helps with debugging
            return future.result()


    @classmethod
    def purge(cls, api, filename, taskname=None):
        """Purge the cache entry. Forces reload on next request for the parser.
        """
        client = api.dask_get_client()
        name = cls._cached_name(api, filename, taskname)
        actor_var = dask.distributed.Variable(name=name, client=client)
        actor_var.delete()


    @classmethod
    def _cached_name(cls, api, filename, taskname):
        pipename = api.pipeline_name()
        return f'CachedParser_{pipename}_{taskname}_{filename}'


class CachedParserInstance:
    """Performance note: async functions run on worker's thread rather than in
    separate thread.

    IMPORTANT: Until this issue is resolved (https://github.com/dask/dask/issues/7626),
    error messages during development (and production...) will only show up
    in the terminal. Thus, testing is fairly important, and try to code up dask
    methods to return Exception objects when needed in the meantime.
    """

    def __init__(self, api_info):
        self.api = faw_pipelines_util.Api(api_info)
        with self.api.task_file_read('model.chkpt', taskname='learn') as f:
            buf = io.BytesIO(f.read())
        self._data = torch.load(buf)
        self._lock = threading.Lock()

        # This happens on a dask worker -- aka, needs a path adjustment
        path = os.path.dirname(os.path.abspath(__file__))
        sys.path.insert(0, path)

        from . import model as model_module
        self._model = model_module.Model.argparse_create(self._data['model_args'])
        self._model.build()
        self._model.load_state_dict(self._data['model']['state_dict'])
        self._model.state_restore(pickle.loads(self._data['model']['model_extra']))
        for optim, state_dict in zip([self._model._optim], self._data['model']['optims']):
            optim.load_state_dict(state_dict)

        self._model.eval()
        self._lastuse = time.monotonic()
        self._queue_todo = asyncio.Queue()


    def get_age_since_use(self):
        return time.monotonic() - self._lastuse


    def get_description(self):
        """Returns description of this parser's progress.
        """
        d = self._data
        c = d['current']
        t = d['total']
        return f'{c} / {t}'


    def get_header_bytes(self):
        """Returns number of header bytes on which this parser was trained.
        """
        return self._data['header_bytes']


    async def parse(self, sents):
        """Parse the given sentences, returning the resulting parse trees.
        """
        # Until actor exceptions propagate...
        try:
            # Note - could get fancy, make this an async method and use buffering
            # behind the scenes. As written, only avoids load time.
            self._lastuse = time.monotonic()

            queue_result = asyncio.Queue()
            await self._queue_todo.put((queue_result, sents))
            loop = asyncio.get_event_loop()
            # TODO error here should trickle
            await loop.run_in_executor(None, self._run_batch, loop)
            result = await queue_result.get()
            return result
        except Exception as e:
            return e


    def _run_batch(self, loop):
        """Called after items are added to `_queue_todo`. May have already been
        processed in another batch.

        NOTE: As of 2021-05-03, dask doesn't propagate exceptions from
        async methods (https://github.com/dask/dask/issues/7626). This is an
        issue.
        """
        with self._lock:
            batch = []
            batch_queues = []
            batch_slices = []
            async def read_up_to_batch_size():
                # Happens in context of asyncio loop
                while len(batch) < 32:  # self._model.config.batch_size:
                    try:
                        res_queue, res_input = self._queue_todo.get_nowait()
                    except asyncio.QueueEmpty:
                        break
                    else:
                        si = len(batch_slices)
                        batch.extend(res_input)
                        batch_queues.append(res_queue)
                        batch_slices.append(slice(si, si + len(res_input)))
            asyncio.run_coroutine_threadsafe(read_up_to_batch_size(),
                    loop=loop).result()

            if not batch:
                return

            result, _stats = self._model.parse(batch)

            # Push results back on
            for q, idx in zip(batch_queues, batch_slices):
                asyncio.run_coroutine_threadsafe(
                        q.put(result[idx]), loop=loop).result()

