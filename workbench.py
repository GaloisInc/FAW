import argparse
import itertools
import os
import shlex
import subprocess
import textwrap

from pathlib import Path

# These variables are a bit strange, but they exist to make distribution easier.
# Basically, this script is copied and these variables are overwritten
# automatically for deployments.
#
# Shorthand scripts will overwrite `CONFIG_FOLDER`, and deployments will
# overwrite `IMAGE_TAG`.
CONFIG_FOLDER = None
IMAGE_TAG = None
VOLUME_SUFFIX = '-data'


def main():
    """
    Launches a production instance of Galois Format Analysis Workbench (GFAW)
    using the distribution `CONFIG_DIR` to investigate the files in `FILE_DIR`.
    `CONFIG_DIR` may be at most one parent directory up; doing so is mostly a
    convenience for development. Note that Docker requires all sibling folders
    to the one-folder-up idiom will also be uploaded to Docker, so keep the
    containing folder sparse if that feature is used.

    Each directory `FILE_DIR` will be hashed to a unique build of the
    observatory to maximize caching.

    Example invocations:

        python workbench.py pdf path/to/pdfs
        python workbench.py pdf build/pdf-dist-2021-02-10

    For running a FAW deployment on multiple machines, see
    `workbench-teaming-init.py`.
    """

    # Parse arguments
    args = parse_args()

    # Build an image for the ci container
    cname = container_name(args)
    dockerfile = 'ci-container-dockerfile'
    subprocess.check_call(['docker', 'build', '-t', cname, '-f', dockerfile, '.'])

    # Before running, compute all the mount paths
    mount_paths = compute_mount_paths(args.config_dir, args.file_dir)
    mount_paths_as_args = list(
        itertools.chain.from_iterable((("-v", f"{src}:{target}") for (src, target) in mount_paths))
    )

    # And the full command line
    script_path = Path(__file__).resolve().with_name('ci-container-workbench.py')
    command_line = ['python3', str(script_path)] + to_script_args(args)

    subprocess.check_call(
        ['docker', 'run']
        + mount_paths_as_args
        + ['-p', f"{args.port_ci}:9001"]
        + ['-it', '--rm', cname]
        + command_line
    )


def parse_args():
    def folder_exists(v):
        assert os.path.lexists(v), f'Path must exist: {v}'
        return v

    def folder_exists_or_build(v):
        assert os.path.lexists(v) or v.startswith('build'), f'Path must exist or start with build: {v}'
        return v

    parser = argparse.ArgumentParser(
        description=textwrap.dedent(main.__doc__),
        formatter_class=argparse.RawTextHelpFormatter
    )

    # TODO: Verify our understanding of when CONFIG_FOLDER and/or IMAGE_TAG is provided
    if CONFIG_FOLDER is None and IMAGE_TAG is None:
        parser.add_argument('config_dir', type=folder_exists, help="Folder "
                "containing configuration for workbench to analyze a format.")
    parser.add_argument('file_dir', type=folder_exists_or_build, help="Folder containing "
            "files to investigate.")
    parser.add_argument('--port', default=8123, type=int,
            help="Port on which Galois Workbench is accessed.")
    parser.add_argument('--port-dask', default=None, type=int,
            help="If specified, port on which to expose the dask dashboard.")
    parser.add_argument('--port-mongo', default=None, type=int,
            help="If specified, port on which to expose the mongo instance.")
    parser.add_argument('--port-ci', default=9001, type=int,
            help="If specified, port on which to expose the CI endpoints.")
    parser.add_argument('--copy-mongo-from', default=None, type=str,
            help="Replace the pdf-etl database used by the observatory with a "
            "copy of an existing database. Format: localhost:27019/120pdfs.\n\n"
            "May also restore from file: /abs/path. Such files should be "
            "created with: mongodump --archive=output.file --uri='mongodb://path.to.faw:port/db'")
    parser.add_argument('--copy-mongo-to', default=None, type=str,
            help="Backup the pdf-etl database used by the observatory to a "
            "given file. Format: /abs/path.\n\n"
            "May be used with `--copy-mongo-from` to backup a different live "
            "database. In that case, always happens AFTER the effect of "
            "`--copy-mongo-from`, so this still overwrites the local database.")
    parser.add_argument('--production', action='store_true',
            help="Developer option on by default: mount source code over docker image, for "
            "Vue.js hot reloading. Also includes `sys_ptrace` capability to docker "
            "container for profiling purposes.")

    args = parser.parse_args()
    if IMAGE_TAG is None and CONFIG_FOLDER is not None:
        args.config_dir = CONFIG_FOLDER

    return args

def to_script_args(args):
    """
    Convert the arguments from the args object to a list suitable for
    passing to a subprocess call. Unfortunately, this is a somewhat manual
    process at the moment
    """

    arglist = []

    # First the arguments we parsed
    # NOTE: Since we are mounting things by absolute path, we
    # need paths here to be absolute
    # NOTE: We use the `--param=value`` format to allow some of these
    # parameter values to start with a '-' (example VOLUME_SUFFIX)
    if 'config_dir' in vars(args) and args.config_dir:
        arglist.append(f"--config-dir={to_absolute_path(args.config_dir)}")
    if args.file_dir:
        arglist.append(f"--file-dir={to_absolute_path(args.file_dir)}")
    if args.port:
        arglist.append(f"--port={args.port}")
    if args.port_dask:
        arglist.append(f"--port-dask={args.port_dask}")
    if args.port_mongo:
        arglist.append(f"--port-mongo={args.port_mongo}")
    if args.copy_mongo_from:
        arglist.append(f"--copy-mongo-from={args.copy_mongo_from}")
    if args.copy_mongo_to:
        arglist.append(f"--copy-mongo-to={args.copy_mongo_to}")
    if args.production:
        arglist.append('--production')

    # Then the extras we need to push along
    # TODO: Check if these ever get populated
    if IMAGE_TAG:
        arglist.append(f"--image-tag={IMAGE_TAG}")
    if VOLUME_SUFFIX:
        arglist.append(f"--volume-suffix={VOLUME_SUFFIX}")

    return arglist


def container_name(args):
    # TODO: Use a name derived from the container config
    return "faw-ci-container" if args.production else "faw-ci-container-dev"


def compute_mount_paths(config_dir, file_dir):
    """
    Compute the absolute path of directories we need to mount in the CI container.
    Returns a list where each one is a tuple of source and target paths.

    NOTE: Currently they are loaded exactly at the same path in both. Theoretically,
    it is possible to load them at other roots as long as the path "relativity" is
    maintained. (Not keeping the relativity will require work in the ci container workbench
    script)
    """

    # We are going to mount the docker socket anyway
    paths = [('/var/run/docker.sock', '/var/run/docker.sock')]

    # We need to deal with absolute paths
    faw_dir_p = to_absolute_path(__file__).parent
    config_dir_p = to_absolute_path(config_dir)
    file_dir_p = to_absolute_path(file_dir)

    # We must definitely mount the faw_dir
    paths.append((str(faw_dir_p), str(faw_dir_p)))

    # If the other two are not subdirectories, then we must mount those too
    # NOTE: With Python 3.9+ you also can directly use `is_relative_to``
    if faw_dir_p not in config_dir_p.parents:
        paths.append((str(config_dir_p), str(config_dir_p)))
    if faw_dir_p not in file_dir_p.parents:
        paths.append((str(file_dir_p), str(file_dir_p)))

    return paths

def to_absolute_path(path):
    return Path(path).resolve()

if __name__ == '__main__':
    main()
