import argparse
import contextlib
import faw_pipelines_util
import gridfs
import io
import json
import torch

from ml_test.dask import CachedParser

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('api_info', type=json.loads)
    ap.add_argument('--header-bytes', type=int, default=100)

    import model.gram_inf.model_10_paper as model_10_paper
    model_10_paper.model.Model.argparse_setup(ap)

    args = ap.parse_args()

    api = faw_pipelines_util.Api(args.api_info)
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
        }

        # With 2 uniform iterations, ~10% chance any given file isn't included
        data['total'] = max(data['files'] * 2, 100000) // batch_size + 1
        data['current'] = 0

        # Create model
        model_args = args.__dict__.copy()
        model_args.pop('api_info')
        model_args.pop('header_bytes')
        model = model_10_paper.model.Model.argparse_create(model_args)
        model.build()

        # Not recommended, but easiest way for prototyping, low-ish tech debt
        data['model'] = model
    else:
        model = data['model']
        # TODO why is this needed?
        model._optim.weight_decouple = True
        model._optim.fixed_decay = False
        model._optim.rectify = True

    # Load model
    while data['current'] < data['total']:
        print(f'Training batch {data["current"]}')
        batch = api.file_sample(batch_size)

        with contextlib.ExitStack() as stack:
            file_paths = [stack.enter_context(api.file_fetch(f)) for f in batch]

            # Train on this batch of files
            train_step(model, file_paths, header_bytes)

        data['current'] += 1
        if data['current'] % 100 == 0 or data['current'] == data['total']:
            # Write out model to save most recent work
            buf = io.BytesIO()
            torch.save(data, buf)
            with api.task_file_write('model.chkpt') as f:
                f.write(buf.getvalue())

    # Bust the cache for any downstream applications
    CachedParser.purge(api, 'model.chkpt')

    # Let the system know they can stop running this task, and downstream tasks
    # may process.
    api.task_set_done()


def train_step(model, file_paths, header_bytes):
    sents = []
    for fp in file_paths:
        with open(fp, 'rb') as f:
            data = f.read(header_bytes)

        sents.append([data[i:i+1] for i in range(len(data))])
    model.train_batch(sents)


if __name__ == '__main__':
    main()
