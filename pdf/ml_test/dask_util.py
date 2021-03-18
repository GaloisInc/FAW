
import asyncio
import dask.distributed
import faw_pipelines_util
import io
import time
import torch

class CachedParser:
    def __init__(self):
        raise NotImplementedError('static')

    @classmethod
    def get(cls, api, filename, taskname=None, ttl=600):
        """Returns a Dask pointer to an Actor which has the specified model
        loaded in eval mode.
        """
        client = api.dask_get_client()
        name = cls._cached_name(api, filename, taskname)

        actor = None
        actor_var = dask.distributed.Variable(name=name, client=client)

        future = None
        try:
            future = actor_var.get(timeout=1)
        except asyncio.exceptions.TimeoutError:
            pass

        if future is not None:
            actor = future.result()

        while True:
            if actor is not None:
                age = actor.get_age_since_use().result()
                if age < ttl:
                    return actor

            future = client.submit(CachedParserInstance, api_info=api.get_info(),
                    actor=True)
            actor_var.set(future)
            actor = future.result()


    @classmethod
    def purge(cls, api, filename, taskname=None):
        """Purge the cache entry. Forces reload on next request for the parser.
        """
        client = api.dask_get_client()
        name = cls._cached_name(api, filename, taskname)
        client.unpublish_dataset(name=name)


    @classmethod
    def _cached_name(cls, api, filename, taskname):
        pipename = api.pipeline_name()
        return f'CachedParser_{pipename}_{taskname}_{filename}'


class CachedParserInstance:
    """Performance note: async functions run on worker's thread rather than in
    separate thread.
    """

    def __init__(self, api_info):
        self.api = faw_pipelines_util.Api(api_info)
        with self.api.task_file_read('model.chkpt', taskname='learn') as f:
            buf = io.BytesIO(f.read())
        self._data = torch.load(buf)
        self._model = self._data['model']
        self._model.eval()
        self._lastuse = time.monotonic()


    def get_age_since_use(self):
        return time.monotonic() - self._lastuse


    def get_description(self):
        """Returns description of this parser's progress.
        """
        d = self._data
        c = d['current']
        t = d['total']
        return f'{c} / {t}'


    def parse(self, sents):
        # Note - could get fancy, make this an async method and use buffering
        # behind the scenes. As written, only avoids load time.
        self._lastuse = time.monotonic()
        result = self._model.parse(sents)
        return result
