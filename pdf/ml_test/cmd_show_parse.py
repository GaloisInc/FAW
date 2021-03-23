"""Show parsing result on given file.
"""

import argparse
import faw_pipelines_util
import torch

from model.gram_inf.model_10_paper.debug_html import save_parse_html

from .dask_util import CachedParser

def main(api_info, cmd_args):
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('output_html')
    ap.add_argument('input_file')
    args = ap.parse_args(cmd_args)

    api = faw_pipelines_util.Api(api_info)
    model = CachedParser.get(api, 'model.chkpt', taskname='learn')

    with open(args.input_file, 'rb') as f:
        fdata = f.read(100)
    sents = [[fdata[i:i+1] for i in range(len(fdata))]]
    trees, _stats = model.parse(sents).result()
    desc = model.get_description().result()
    epoch_name = f'{args.input_file} -- processed by {desc}'
    save_parse_html(args.output_html, epoch_name, trees)

