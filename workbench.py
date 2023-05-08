#! /usr/bin/env python3

import argparse
import atexit
import itertools
import os
from pathlib import Path
import pyjson5
import re
import shlex
import shutil
import stat
import subprocess
import sys
import tempfile
import textwrap
import traceback
import threading

# Keep track of the directory containing the FAW
faw_dir = os.path.dirname(os.path.abspath(__file__))

sys.path.insert(0, os.path.join(faw_dir, 'faw/ci'))
from ci_container_service import get_faw_container_name, get_faw_image_name, get_default_image_tag

# Ensure docker BUILDKIT is enabled:
os.environ['DOCKER_BUILDKIT'] = '1'


# These variables are a bit strange, but they exist to make distribution easier.
# Basically, this script is copied and these variables are overwritten
# automatically for deployments.
#
# Shorthand scripts will overwrite `CONFIG_FOLDER`, and deployments will
# overwrite `IMAGE_TAG`.
CONFIG_FOLDER = None
IMAGE_TAG = None
VOLUME_SUFFIX = '-data'

# Where the CI container logs should go. This needs to be a local path since
# we need to mount this in to the FAW as well. However, multiple FAWs can run,
# so we use a temporary directory.
CI_TEMPDIR = tempfile.TemporaryDirectory()
atexit.register(CI_TEMPDIR.cleanup)
CI_CONTAINER_LOG_PATH_HOST = os.path.join(CI_TEMPDIR.name, 'logs', 'ci-container')
CI_CONTAINER_LOG_PATH_CONTAINER = '/var/log/ci-container'


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

    # Compute all the mount paths for the CI container
    standard_mount_paths = compute_mount_paths(
        args.config_dir, args.file_dir,
        [args.copy_mongo_from, args.copy_mongo_to]
    )
    dev_mount_paths = compute_devmount_paths(args.config_dir)
    mount_paths_as_args = list(
        itertools.chain.from_iterable(
            (("-v", f"{src}:{target}") for (src, target) in standard_mount_paths + dev_mount_paths)
        )
    )

    # And the full command line for running the CI container as a service
    container_script_path = Path(faw_dir) / 'faw' / 'ci' / 'ci_container_service.py'
    command_line = ['python3', str(container_script_path)] + to_script_args(args)

    # Build an image for the ci container
    cname = container_name(args)
    imgname = image_name(args)
    faw_container_name = get_faw_container_name(IMAGE_TAG, args.config_dir, args.file_dir)
    ci_container_cmd = shlex.join(command_line)
    r = subprocess.run(
            ['docker', 'build', '-t', imgname,
                '-f', os.path.join(faw_dir, 'faw', 'ci', 'ci-container-dockerfile'),
                '.'])
    if r.returncode != 0:
        raise Exception("Docker build failed; see above")

    interactive_script_path = Path(faw_dir) / 'faw' / 'ci' / 'ci_container_interactive.py'
    # If we were in build mode, we need to now export the image and do associated activities
    if args.build_mode:
        # First, build the image
        image_tag = IMAGE_TAG or get_default_image_tag(args.config_dir)
        command_line = [
                'python3', str(interactive_script_path),
                '--build-mode',
                '--config-dir', str(to_absolute_path(args.config_dir)),
                '--file-dir', str(to_absolute_path(args.file_dir)),
                '--image-tag', image_tag]
        docker_args = (
                ['docker', 'run',
                  ] + mount_paths_as_args + [
                  '-e', f'FAW_CI_CMD={ci_container_cmd}',
                  '-e', f'FAW_CONTAINER_NAME={faw_container_name}',
                  '--name', cname,
                  '-it', '--rm', imgname
                  ] + command_line)
        r = subprocess.run(docker_args)
        if r.returncode != 0:
            raise ValueError("Image build failed; see above")

        # Save the docker image here. This ensures appropriate permissions on the file(s) etc
        export_faw_image(image_tag, args.config_dir, args.file_dir)

    # Otherwise run an interactive process in the CI container to support, well, interactivity
    else:
        command_line = (
            ["python3", str(interactive_script_path)] +
            ['--config-dir', str(to_absolute_path(args.config_dir))] +
            ['--file-dir', str(to_absolute_path(args.file_dir))] +
            [] if not IMAGE_TAG else ['--image-tag', IMAGE_TAG]
        )
        command = (
            ['docker', 'run']
            + mount_paths_as_args
            + ['-p', f"{args.port_ci}:9001"]
            + ['-e', f'FAW_CI_CMD={ci_container_cmd}']
            + ['-e', f'FAW_CONTAINER_NAME={faw_container_name}']
            + ['-e', f'FAW_HOST_CI_LOG_DIR={CI_CONTAINER_LOG_PATH_HOST}']
            + ['--name', cname]
            + ['--add-host', 'host.docker.internal:host-gateway']
            + ['-it', '--rm', imgname]
            + command_line
            # + ['/bin/bash']
        )

        if not (args.copy_mongo_from or args.copy_mongo_to):
            # Pop open the UI for the user
            def open_browser():
                import time, webbrowser
                time.sleep(1.5)
                try:
                    webbrowser.open(f'http://localhost:{args.port}')
                except Exception:
                    traceback.print_exc()
            open_browser_thread = threading.Thread(target=open_browser)
            open_browser_thread.daemon = True
            open_browser_thread.start()

        print('Command: ', command)
        subprocess.run(command)


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
            help="Disable developer mode (mount source code over docker image, for "
            "Vue.js hot reloading. Also includes `sys_ptrace` capability to docker "
            "container for profiling purposes.).")

    args = parser.parse_args()
    if IMAGE_TAG is None and CONFIG_FOLDER is not None:
        args.config_dir = CONFIG_FOLDER

    # Check if we are in build mode, which is determined by the target path and not
    # the command line arguments interestingly.
    faw_dir = os.path.dirname(os.path.abspath(__file__))
    build_mode = (os.path.split(os.path.relpath(args.file_dir, faw_dir))[0] == 'build')
    if build_mode and not args.production:
        assert args.production, "Build must use --production"
    args.build_mode = build_mode

    return args


def export_faw_image(image_tag, config_dir, target_dir):
    # Populate the given directory with a built version of the workbench.
    # Assume that the CI container has already built the image

    print("Saving image ...")

    # First remove the directory if it exists
    try:
        shutil.rmtree(target_dir)
    except FileNotFoundError:
        pass
    os.makedirs(target_dir)

    # Export docker image

    # relpath() here used to strip trailing slash, which messes up basename
    dist_name = os.path.basename(os.path.relpath(config_dir))
    image_file_name = f'{image_tag}.image'
    subprocess.check_call(
        f'docker save {image_tag} | gzip > {os.path.join(target_dir, image_file_name)}', shell=True
    )

    # Export readme
    with \
            open(os.path.join(faw_dir, 'faw', 'README-dist.md')) as fin, \
            open(os.path.join(target_dir, 'README.md'), 'w') as fout:
        fout.write(
            re.sub(
                r'{distribution}', dist_name,
                re.sub(r'{imageName}', image_tag, fin.read())
            )
        )

    # Build modified script, put it in the right place and make it executable
    source_script_path = os.path.join(faw_dir, 'faw', 'ci', 'ci_container_service.py')
    target_script_path = os.path.join(target_dir, f'workbench-{dist_name}.py')
    with open(source_script_path) as fin, open(target_script_path, 'w') as fout:
        fout.write(
            re.sub('IMAGE_TAG = [N]one', f'IMAGE_TAG = {repr(image_tag)}', fin.read())
        )
        st = os.stat(target_script_path)
        os.chmod(target_script_path, st.st_mode | stat.S_IEXEC)

    if False:
        # Package up whole file
        # On second thought, don't. It takes up disk space and the user could
        # run this step on their own
        file_name = os.path.abspath(os.path.join(os.path.abspath(target_dir), '..',
                f'{config_data["name"]}.tar.gz'))
        try:
            os.remove(file_name)
        except FileNotFoundError:
            pass
        subprocess.check_call(['tar', '-czvf', file_name] + os.listdir(target_dir),
                cwd=target_dir)

    print(f'Package available as {target_dir}')


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
    if args.build_mode:
        arglist.append('--build-mode')

    # Then the extras we need to push along
    # TODO: Check if these ever get populated
    if IMAGE_TAG:
        arglist.append(f"--image-tag={IMAGE_TAG}")
    if VOLUME_SUFFIX:
        arglist.append(f"--volume-suffix={VOLUME_SUFFIX}")

    # Finally if we are launching from here, it is always as a service
    arglist.append("--service")

    return arglist


def image_name(args):
    return get_faw_image_name(IMAGE_TAG, args.config_dir, '' if args.production else '-dev') + '-ci'


def container_name(args):
    faw_container_name = get_faw_container_name(IMAGE_TAG, args.config_dir, args.file_dir)
    return f"{faw_container_name}-ci"


def compute_mount_paths(config_dir, file_dir, maybe_file_list):
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

    # Ensure this exists; if it does not, then docker will create it as `root`,
    # which makes it difficult to delete
    os.makedirs(CI_CONTAINER_LOG_PATH_HOST, exist_ok=True)
    paths.append((CI_CONTAINER_LOG_PATH_HOST, CI_CONTAINER_LOG_PATH_CONTAINER))

    # Mount any potential files' folders s.t. the CI system can read/write those
    # files.
    for f in maybe_file_list:
        # Quick way to count out URLs... not great
        if f and f.strip() and ':' not in f:
            f_dir = os.path.dirname(os.path.abspath(f))
            assert os.path.lexists(f_dir), f'parent folder must exist: {f_dir}'
            paths.append((f_dir, f_dir))

    return paths


def to_absolute_path(path):
    return Path(path).resolve()


def compute_devmount_paths(config_dir):
    """
    Do a light-weight parse of config.json5(s) both at the top-level and in plugin folders
    to pick out any devmount configurations. For each valid devmount, pick the source and
    target paths for mounting within the CI container. Note that the target location
    must be derivable independently within the CI container as well.
    """

    # NOTE: Each devmount section is a dictionary where the key is an environment
    # variable and value is a configuration (of type 'dict') for that devmount. At this
    # level, we only care about the environment variables (i.e. keys) and whether
    # they are valid (i.e. defined). The configuration etc will be dealt with by the
    # CI container separately.
    devmount_vars = set()

    # A simple function to extend a set, but issue a warning if the new elements
    # overlap with the old
    def _warn_update_set(current, values):
        overlaps = current.intersection(values)
        if overlaps:
            for name in overlaps:
                print(f'Warning: A devmount by the name "{name}" already exists')

        current.update(values)

    with open((os.path.join(config_dir, 'config.json5'))) as f:
        config_data = pyjson5.load(f)
        if v := safe_dict_fetch(config_data, 'build', 'devmounts'):
            _warn_update_set(devmount_vars, v.keys())

    for child_path in os.listdir(config_dir):
        child_path = os.path.join(config_dir, child_path)
        if not os.path.isdir(child_path):
            continue

        child_config_path = os.path.join(child_path, 'config.json5')
        if os.path.lexists(child_config_path):
            with open(child_config_path) as f:
                child_config_data = pyjson5.load(f)
                if v := safe_dict_fetch(child_config_data, 'build', 'devmounts'):
                    _warn_update_set(devmount_vars, v.keys())

    return [(os.getenv(v), f"/home/devmounts/{v}") for v in devmount_vars if os.getenv(v)]


def safe_dict_fetch(dct, *args):
    last = dct
    for k in args:
        if not isinstance(last, dict) or k not in last:
            # print("Looked for " + str(args) + ", did not find")
            return None
        else:
            last = last[k]

    return last


if __name__ == '__main__':
    main()
