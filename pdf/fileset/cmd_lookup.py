"""Since the hash lookup lists are several hundred MB, this class manages them.
"""

import asyncio
import dask
import dask_actor_singleton
import faw_pipelines_util
import os
import sys

_path = os.path.dirname(os.path.abspath(__file__))

def main(api_info, fname):
    if api_info:
        api = faw_pipelines_util.Api(api_info)
        client = api.dask_get_client()

        actor = dask_actor_singleton.get('filelist-f-lookup',
                create=lambda: LookupInstance(path=os.path.join(_path, 'filelists')),
                client=client)
        sets = actor.lookup(os.path.basename(fname)).result()

        for s in sets:
            print(f'member: {s}')

    pp = os.path.dirname(fname)
    prefix = '/home/pdf-files/'
    len_prefix = len(prefix)
    while pp.startswith(prefix):
        pp_end = pp[len_prefix:]
        print(f'dir full: {pp_end}')
        print(f'dir tag: {os.path.basename(pp_end)}')
        pp = os.path.dirname(pp)


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

