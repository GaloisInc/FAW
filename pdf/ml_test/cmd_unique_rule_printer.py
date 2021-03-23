"""Show parsing result on given file.
"""

import argparse
import collections
import faw_pipelines_util
import json

from .dask_util import CachedParser

def main(api_info, cmd_args):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('input_file')
    args = ap.parse_args(cmd_args)

    api = faw_pipelines_util.Api(api_info)
    model = CachedParser.get(api, 'model.chkpt', taskname='learn')

    with open(args.input_file, 'rb') as f:
        fdata = f.read(100)
    sents = [[fdata[i:i+1] for i in range(len(fdata))]]
    trees, _stats = model.parse(sents).result()

    def bytes_to_str(b):
        return json.dumps(b.decode('latin1'))[1:-1]

    rules = collections.defaultdict(int)
    stack = [trees[0]]
    seen = set()
    while stack:
        node = stack.pop()

        r = node.rep
        if len(r) < 25:
            # Short enough to maybe be valid
            rules[bytes_to_str(r)] += 1

        for c in node.children():
            if c in seen:
                continue
            stack.append(c)
            seen.add(c)

    for k, v in rules.items():
        print(f'{k}: {v}')

