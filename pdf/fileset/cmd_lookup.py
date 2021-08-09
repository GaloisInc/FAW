"""Since the hash lookup lists are several hundred MB, this class manages them.
"""

import asyncio
import dask
import dask_actor_singleton
import faw_pipelines_util
import os

_path = os.path.dirname(os.path.abspath(__file__))

def main(api_info, fname):
    api = faw_pipelines_util.Api(api_info)
    client = api.dask_get_client()

    actor = dask_actor_singleton.get('filelist-f-lookup',
            create=lambda: LookupInstance(path=os.path.join(_path, 'filelists')),
            client=client)
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

