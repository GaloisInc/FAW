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
    ap.add_argument('--max-feature-size', default=30, type=int)
    args = ap.parse_args(cmd_args)

    api = faw_pipelines_util.Api(api_info)
    model = CachedParser.get(api, 'model.chkpt', taskname='learn')

    with open(args.input_file, 'rb') as f:
        fdata = f.read(model.get_header_bytes().result())
    sents = [[fdata[i:i+1] for i in range(len(fdata))]]
    trees = model.parse(sents).result()
    if isinstance(trees, Exception):
        raise trees

    def bytes_to_str(b):
        return json.dumps(b.decode('latin1'))[1:-1]

    rules = collections.defaultdict(int)
    stack = [trees[0]]
    seen = set()
    while stack:
        node = stack.pop()

        r = node.rep
        if len(r) < args.max_feature_size:
            # Short enough to maybe be valid
            rules[bytes_to_str(r)] += 1

        for c in node.children():
            if c in seen:
                continue
            stack.append(c)
            seen.add(c)

    for k, v in rules.items():
        print(f'rule: {k}: {v}')

