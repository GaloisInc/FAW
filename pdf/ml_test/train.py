import argparse
import contextlib
import faw_pipelines_util
import gridfs
import json
import torch

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('api_info', type=json.loads)
    args = ap.parse_args()

    api = faw_pipelines_util.Api(args.api_info)
    batch_size = 64

    try:
        with api.task_file_read('model.chkpt') as f:
            data = torch.load(f)
    except gridfs.NoFile:
        data = {
                'files': api.file_count(),
        }

        # With 2 uniform iterations, ~10% chance any given file isn't included
        data['total'] = max(data['files'] * 2, 100000) // batch_size + 1
        data['current'] = 0

    # Load model
    while data['current'] < data['total']:
        batch = api.file_sample(batch_size)

        with contextlib.ExitStack() as stack:
            file_paths = [stack.enter_context(api.file_fetch(f)) for f in batch]

            # Train on this batch of files
            print(file_paths)

        data['current'] += 1
        if data['current'] % 100 == 0 or data['current'] == data['total']:
            # Write out model to save most recent work
            with api.task_file_write('model.chkpt') as f:
                torch.save(data, f)

    # Let the system know they can stop running this task, and downstream tasks
    # may process.
    api.task_set_done()


if __name__ == '__main__':
    main()
