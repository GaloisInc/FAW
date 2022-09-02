#!/usr/bin/env python

import argparse
import os
import pathlib
import subprocess
import tempfile
import textwrap


def main():
    """
    A client to interact with the CI infrastructure of FAW.

    Currently the only feature available is to send updated configurations to a running CI instance. It is
    generally expected that users work with the FAW directory tree to modify plugins/parsers. To send updated
    configurations execute:

        python3 faw_ci_cli.py update-config <file or dir> <file or dir> ...

    Each argument can be the path to a (presumably modified) configuration file or a plugin/parser folder.
    """

    parser = argparse.ArgumentParser(
        description=textwrap.dedent(main.__doc__),
        formatter_class=argparse.RawTextHelpFormatter
    )

    parser.add_argument('--host', default='localhost', help="Hostname where the FAW CI server is running")
    parser.add_argument('--port', type=int, default=9001, help="The port on which the FAW CI server is running")

    subparsers = parser.add_subparsers(dest='subcommand', required=True)
    update_parser = subparsers.add_parser('update-config', help='Send updated configurations to the FAW CI instance')
    update_parser.add_argument('file_or_dir', nargs='+', metavar='<FILE-OR-DIR>')
    update_parser.set_defaults(func=_handle_config_change)

    args = parser.parse_args()
    args.func(args)


def _handle_config_change(args):
    # Step 1 - find common path, reframe everything relative to that
    paths = [pathlib.Path(a).absolute() for a in args.file_or_dir]
    for p in paths:
        if not p.exists:
            raise ValueError(f'{p} does not exist')

    common = pathlib.Path(os.path.commonpath([p.resolve() for p in paths]))
    if not common.is_dir():
        common = common.parent
    for parent in reversed([common] + list(common.parents)):
        if (
                (parent / 'config.json5').exists()
                and not (parent.parent / 'config.json5').exists()):
            # Found the root config.
            common = parent
            break

    paths = [p.relative_to(common) for p in paths]

    # Create the tar file
    with tempfile.TemporaryDirectory() as temp:
        file_path = pathlib.Path(temp, 'config.tar.gz')
        subprocess.check_call(
                ['tar', 'czvf', str(file_path.resolve()), '-C', str(common.resolve())]
                + [str(p) for p in paths])

        # Upload to server
        subprocess.check_call([
            'curl', '-v', '-i', '-F', f'config=@{str(file_path)}', f'http://{args.host}:{args.port}/configuration'
        ])


if __name__ == '__main__':
    main()
