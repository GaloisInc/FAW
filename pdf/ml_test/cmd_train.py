import argparse
import contextlib
import faw_pipelines_util
import gridfs
import io
import pickle
import sys
import torch

from .dask_util import CachedParser

def main(api_info, cmd_args):
    ap = argparse.ArgumentParser()
    ap.add_argument('--header-bytes', type=int, default=100)

    from . import model as model_module

    model_module.Model.argparse_setup(ap)

    args = ap.parse_args(cmd_args)

    api = faw_pipelines_util.Api(api_info)
    batch_size = 8
    header_bytes = args.header_bytes

    data = None
    try:
        with api.task_file_read('model.chkpt') as f:
            buf = io.BytesIO(f.read())
        data = torch.load(buf)
    except gridfs.NoFile:
        pass

    if data is None:
        data = {
                'files': api.file_count(),
                'header_bytes': header_bytes,
        }

        # With 2 uniform iterations, ~10% chance any given file isn't included
        data['total'] = 100  # max(data['files'] * 2, 100000) // batch_size + 1
        data['current'] = 0

        # Create model
        model_args = args.__dict__.copy()
        model_args.pop('header_bytes')
        model = model_module.Model.argparse_create(model_args)
        model.build()

        # Remember enough to rebuild model
        data['model_args'] = model_args
        data['model'] = None
    else:
        model = model_module.Model.argparse_create(data['model_args'])
        model.build()
        model.load_state_dict(data['model']['state_dict'])
        model.state_restore(pickle.loads(data['model']['model_extra']))
        for optim, state_dict in zip([model._optim], data['model']['optims']):
            optim.load_state_dict(state_dict)

    # Load model
    while data['current'] < data['total']:
        print(f'Training batch {data["current"]}')
        batch = api.file_sample(batch_size)

        # Polyfile examples
        if False:
            if False:
                # Non-distribued
                polyfile_json = [polyfile_run(api_info, f) for f in batch]
            else:
                # Distributed
                import dask
                polyfile_json = dask.compute([
                        dask.delayed(polyfile_run)(api_info, f)
                        for f in batch])[0]


        with contextlib.ExitStack() as stack:
            file_paths = [stack.enter_context(api.file_fetch(f)) for f in batch]

            # Train on this batch of files
            train_step(model, file_paths, header_bytes)

        data['current'] += 1
        api.task_status_set_message(f"{data['current']} out of {data['total']}")
        if data['current'] % 100 == 1 or data['current'] == data['total']:
            # Write out model to save most recent work (Save first batch to
            # bootstrap viewing file detail views for debugging UI)
            data['model'] = {
                    'state_dict': model.state_dict(),
                    'model_extra': pickle.dumps(model.state_to_save(),
                        protocol=pickle.HIGHEST_PROTOCOL),
                    'optims': [o.state_dict() for o in [model._optim]],
            }

            buf = io.BytesIO()
            torch.save(data, buf)
            with api.task_file_write('model.chkpt') as f:
                f.write(buf.getvalue())

    # Bust the cache for any downstream applications
    CachedParser.purge(api, 'model.chkpt')


def train_step(model, file_paths, header_bytes):
    sents = []
    for fp in file_paths:
        with open(fp, 'rb') as f:
            data = f.read(header_bytes)

        sents.append([data[i:i+1] for i in range(len(data))])
    model.train_batch(sents)


# Get polyfile results for each file, or None for unable to
# parse.
def polyfile_run(api_info, f_in_db):
    """
    This could also be done in dask, but that's a pretty small
    change if the desired functionality is already in a top-level
    module function.
    """
    import json, subprocess

    api = faw_pipelines_util.Api(api_info)
    with api.file_fetch(f_in_db) as f_local:
        p = subprocess.Popen(['polyfile', f_local],
                stdout=subprocess.PIPE,
                stderr=subprocess.DEVNULL,
                stdin=subprocess.PIPE,
                text=True)
        stdout, stderr = p.communicate()
        ret = p.wait()
        if ret != 0:
            return None
        try:
            return json.loads(stdout)
        except json.decoder.JSONDecodeError:
            print(f'BAD JSON: {stdout[:1000]}', file=sys.stderr)
            raise

