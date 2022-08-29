#!/usr/bin/env python

import argparse
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
    # Figure out the directories we need to zip
    folders = set()
    for f in args.file_or_dir:
        ford = pathlib.Path(f)
        if not ford.exists:
            raise f"File or directory {ford} does not exist"

        if ford.is_dir():
            # We are going to assume this is a plugin directory
            # and go with it.
            folders.add(ford)
        else:
            # If this a (config) file, we will just move a level up
            # and treat that as the plugin directory
            folders.add(ford.parent)

    # Create the parameters related to folders that we need to pass on to tar
    folder_tar_params = []
    for f in folders:
        folder_tar_params.extend(['-C', str(f.parent.resolve())])
        folder_tar_params.append(f.name)

    # Tar it up to a temporary directory (and a fixed filename)
    # and then send it to the CI container's server
    with tempfile.TemporaryDirectory() as temp:
        file_path = pathlib.Path(temp, "config.tar.gz")
        subprocess.check_call([
            'tar', 'cvf', str(file_path.resolve()),
        ] + folder_tar_params)

        subprocess.check_call([
            'curl', '-v', '-i', '-F', f'config=@{str(file_path)}', f'http://{args.host}:{args.port}/configuration'
        ])


if __name__ == '__main__':
    main()
