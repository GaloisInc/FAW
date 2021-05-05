"""Print 2-byte combinations from given file.
"""

import argparse
import collections
import faw_pipelines_util
import json

# Needed to load number of header bytes allowed
from .dask_util import CachedParser

def main(api_info, cmd_args):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('input_file')
    args = ap.parse_args(cmd_args)

    api = faw_pipelines_util.Api(api_info)
    model = CachedParser.get(api, 'model.chkpt', taskname='learn')

    with open(args.input_file, 'rb') as f:
        fdata = f.read(model.get_header_bytes().result())

    def bytes_to_str(b):
        return json.dumps(b.decode('latin1'))[1:-1]

    rules = collections.defaultdict(int)
    for i in range(len(fdata)):
        # range(n gram size)
        for j in range(4):
            if i + 1 + j > len(fdata):
                continue

            rules[bytes_to_str(fdata[i:i+j+1])] += 1
    for k, v in rules.items():
        print(f'bytes: {k}: {v}')

