"""Since the hash lookup lists are several hundred MB, this class manages them.
"""

import asyncio
import dask
import faw_pipelines_util
import os

_path = os.path.dirname(os.path.abspath(__file__))

def main(api_info, fname):
    api = faw_pipelines_util.Api(api_info)
    client = api.dask_get_client()

    var = dask.distributed.Variable(name='filelist-f-lookup', client=client)
    try:
        future = var.get(timeout=1)
    except asyncio.exceptions.TimeoutError:
        with dask.distributed.Lock(name='filelist-f-create', client=client):
            future = None

            try:
                future = var.get(timeout=1)
            except asyncio.exceptions.TimeoutError:
                # Create, have lock
                future = client.submit(LookupInstance,
                        path=os.path.join(_path, 'filelists'),
                        actor=True)
                # Allow this exception to trickle up __init__ errors
                future.result()

                # Finally, assign on success
                var.set(future)
            else:
                # Retrieved future successfully
                pass

    actor = future.result()
    sets = actor.lookup(os.path.basename(fname)).result()

    for s in sets:
        print(f'member: {s}')


class LookupInstance:
    def __init__(self, path):
        self._sets = {}
        for f in os.listdir(path):
            fname = f.rsplit('.', 1)[0]
            fset = set()
            self._sets[fname] = fset

            for line in open(os.path.join(path, f)):
                line_f = line.strip().rsplit('/', 1)[-1]
                fset.add(line_f)


    def lookup(self, fname):
        return {k for k, v in self._sets.items() if fname in v}

