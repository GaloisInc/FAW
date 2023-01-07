#! /usr/bin/env python3

"""This is the convenience script for developing, building, distributing, and
running end-user copies of the Galois Format Analysis Workbench.
"""

# IMPORTANT - Only stdlib here! We want to minimize dependencies required by
# downstream users. Other imports are allowed in functions not used by the
# version published as part of a build.

# NOTE: We are not completely devoid of non-standarddependencies, but it is
# important to note that they are all imported only on demand. Thus if a
# feature is not available, the corresponding package will not be required.

# NOTE: While it is true that this file runs in a container most of the time
# a modified copy of this can *also* be used to run a saved image.

import argparse
import asyncio
import atexit
import dataclasses
import io
import hashlib
import logging
import os
import pathlib
import re
import shlex
import shutil
import subprocess
import sys
import tarfile
import textwrap
import threading
import time
import traceback
import uuid


# These variables are used throughout the script. So we keep them for now
# TODO: Remove them and pass as parameters
CONFIG_FOLDER = None
IMAGE_TAG = None
VOLUME_SUFFIX = None

# In copied scripts (see `build` functionality), IMAGE_TAG is written
STATIC_BUILD = IMAGE_TAG is not None

# Keep track of the directory containing the FAW
# Must not be used for built distributions (e.g., target `build/`)
faw_dir = str(pathlib.Path(__file__).parent.parent.parent.resolve())
ALL_FOLDER = os.path.abspath(os.path.join(faw_dir, 'all'))

# Initialize a logger
logging.basicConfig(level=logging.DEBUG,
                    format="%(name)s - %(levelname)s - %(message)s",
                    datefmt='%d-%b-%y %H:%M:%S')

# Service notification FD
SERVICE_NOTIFICATION_FD = 3


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
    global IMAGE_TAG, VOLUME_SUFFIX

    parser = argparse.ArgumentParser(description=textwrap.dedent(main.__doc__),
            formatter_class=argparse.RawTextHelpFormatter)
    def folder_exists(v):
        assert os.path.lexists(v), f'Path must exist: {v}'
        return v
    def folder_exists_or_build(v):
        assert os.path.lexists(v) or v.startswith(os.path.join(faw_dir, 'build')), f'Path must exist or start with build: {v}'
        return v

    if not STATIC_BUILD:
        parser.add_argument('--config-dir', type=folder_exists, help="Folder "
            "containing configuration for workbench to analyze a format.")

    if not STATIC_BUILD:
        parser.add_argument('--file-dir', type=folder_exists_or_build, required=True, help="Folder containing "
                "files to investigate.")
    else:
        # Note the positional style of parameter used here. This allows for fluent usage in deployment
        # scenarios
        parser.add_argument('file_dir', type=folder_exists_or_build, help="Folder containing files to investigate.")

    parser.add_argument('--port', default=8123, type=int,
            help="Port on which Galois Workbench is accessed.")
    parser.add_argument('--port-dask', default=None, type=int,
            help="If specified, port on which to expose the dask dashboard.")
    parser.add_argument('--port-mongo', default=None, type=int,
            help="If specified, port on which to expose the mongo instance.")
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

    if not STATIC_BUILD:
        parser.add_argument('--build-mode', action='store_true',
                help="Indicates whether the script should run in build mode")
        parser.add_argument('--service', action='store_true',
                help="Indicates whether the script should run in a non-interactive service mode")

    if IMAGE_TAG is None:
        parser.add_argument('--image-tag', default=None, help="Tag prefix for the image being built")
    if VOLUME_SUFFIX is None:
        parser.add_argument('--volume-suffix', default='-data', help="Suffix for the database Volume")

    args = parser.parse_args()
    pdf_dir = os.path.abspath(args.file_dir)
    port = args.port
    port_mongo = args.port_mongo
    copy_mongo_from = args.copy_mongo_from
    copy_mongo_to = args.copy_mongo_to
    development = not args.production
    build_mode = 'build_mode' in vars(args) and args.build_mode
    running_as_service = 'service' in vars(args) and args.service

    # Update the global variables
    # TODO: Remove globals and pass as parameters
    if not IMAGE_TAG:
        IMAGE_TAG = args.image_tag
    VOLUME_SUFFIX = args.volume_suffix

    if build_mode:
        # Exit before building image, which can be expensive
        assert not development, "Build cannot use --development"

    if IMAGE_TAG is None:
        config = args.config_dir
        # # Not a deployment -- need to load spec so we can build the image.
        # if CONFIG_FOLDER is None:
            # config = args.config_dir
        # else:
            # config = CONFIG_FOLDER

        # Figure out build folder
        config_rel = os.path.relpath(config, faw_dir)
        parts = []
        while config_rel:
            h, t = os.path.split(config_rel)
            parts.insert(0, t)
            config_rel = h
        if parts[0] == '..':
            assert len(parts) > 1, parts
            assert parts[1] != '..', 'Only one parent directory allowed to reach distribution folder'

            build_dir = os.path.dirname(faw_dir)
            build_faw_dir = os.path.basename(faw_dir)

            # Ensure the docker build sees the correct .dockerignore, or it will
            # upload a lot of extra cruft.
            shutil.copy2(os.path.join(faw_dir, '.dockerignore'),
                    os.path.join(build_dir, '.dockerignore'))
        else:
            build_dir = faw_dir
            build_faw_dir = '.'

        # Ensure config is up to date
        config_data = _check_config_file(config, build_dir)
    else:
        # We have an IMAGE_TAG and that means we are working off a saved image
        # We shouldn't need to care about config/config_data etc really. We also
        # only support production mode in this case
        config_data, config, build_dir, build_faw_dir = None, None, None, None
        development = False

    # Check that observatory image is loaded / built
    _check_image(development=development, config=config, config_data=config_data,
            build_dir=build_dir, build_faw_dir=build_faw_dir)

    # If we are in build mode, we have no more work to do in the CI container
    if build_mode:
        assert running_as_service
        _service_write_ready()
        return

    # Hash absolute path to folder to generate consistent DB name.
    db_name = get_db_name(pdf_dir)

    if copy_mongo_from is not None or copy_mongo_to is not None:
        # Auxiliary command for copying data from an existing mongo instance.
        # Used internally.
        _mongo_copy(db_name=db_name, copy_mongo_from=copy_mongo_from,
                copy_mongo_to=copy_mongo_to)
        return

    extra_flags = []
    if args.port_dask:
        extra_flags.extend(['-p', f'{args.port_dask}:8787'])
    if port_mongo:
        extra_flags.extend(['-p', f'{port_mongo}:27017'])

    docker_bin_path = os.path.join(faw_dir, 'faw', 'docker-bin')
    if os.path.lexists(docker_bin_path):
        # Mount utilities for restarting the FAW.. can be handy.
        for f in os.listdir(docker_bin_path):
            ff = os.path.join(docker_bin_path, f)
            extra_flags.extend(['-v', f'{ff}:/usr/bin/{f}:ro'])

    if not development:
        extra_flags.append(IMAGE_TAG)
    else:
        # Mount various internal components
        extra_flags.extend(['-v', f'{faw_dir}/faw/pdf-etl-parse:/home/pdf-etl-parse'])
        extra_flags.extend(['-v', f'{faw_dir}/faw/pdf-observatory:/home/pdf-observatory'])

        # Mount distribution code
        extra_flags.extend(['-v', f'{ALL_FOLDER}:/home/all'])
        extra_flags.extend(['-v', f'{os.path.abspath(args.config_dir)}:/home/dist'])

        # Allow profiling via e.g. py-spy
        extra_flags.extend(['--cap-add', 'sys_ptrace'])

        extra_flags.append(IMAGE_TAG + '-dev')

    docker_id = get_faw_container_name(IMAGE_TAG, config, pdf_dir)
    if development:
        # Ensure that the necessary npm modules are installed to run the UI
        # locally. Notably, we do this from docker s.t. the node version used
        # to install packages is the same one used to run the FAW.
        #subprocess.check_call(['npm', 'install'],
        #        cwd=os.path.join(faw_dir, 'faw', 'pdf-observatory', 'ui'))
        subprocess.check_call(['docker', 'run', '--rm', '--entrypoint',
                '/bin/bash']
                + extra_flags
                + [
                    '-c', 'cd /home/pdf-observatory/ui && npm install'
                ])
                
        # We also install the npm modules required for the CI support nodejs app
        subprocess.check_call(['docker', 'run', '--rm', '--entrypoint',
                '/bin/bash']
                + extra_flags
                + [
                    '-c', 'cd /home/pdf-observatory/ci && npm install && npm run build'
                ])

        # Distribution folder is mounted in docker container, but workbench.py
        # holds the schema.
        def watch_for_config_changes(development=development, config_dir=config,
            build_dir=build_dir, build_faw_dir=build_faw_dir, docker_id=docker_id):
            # Where, in the docker container, to dump the new config
            cfg_dst = '/home/config.json'

            last_ts = None
            last_config = _check_config_file(config, build_dir)
            while True:
                try:
                    new_ts = {}
                    for config_spec in _plugin_folders(config=config, include_root=True):
                        cfg_path = os.path.join(config_spec.local_path, 'config.json5')
                        ts = os.path.getmtime(cfg_path)
                        new_ts[cfg_path] = ts

                    if last_ts is None:
                        # Initialization
                        last_ts = new_ts
                    elif last_ts != new_ts:
                        # Some config changed
                        last_ts = new_ts

                        logging.info('workbench: Updating /home/config.json')
                        new_config = _check_config_file(config, build_dir)
                        new_config_file = _export_config_json(new_config).encode('utf-8')

                        # Check if build stages have changed and handle if necessary
                        _check_build_stage_change_and_update(
                            new_config=new_config, old_config=last_config,
                            development=development, config_dir=config_dir,
                            build_dir=build_dir, build_faw_dir=build_faw_dir,
                            current_docker_id=docker_id
                        )

                        last_config = new_config

                        # Docker cp is weird... if stdin, it's a tarred
                        # directory
                        buf = io.BytesIO()
                        with tarfile.TarFile(fileobj=buf, mode='w') as tf:
                            finfo = tarfile.TarInfo(os.path.basename(cfg_dst))
                            finfo.size = len(new_config_file)
                            finfo.mtime = time.time()
                            tf.addfile(finfo, io.BytesIO(new_config_file))
                        buf.seek(0)
                        p_ts = subprocess.Popen(['docker', 'cp', '-',
                            f'{docker_id}:{os.path.dirname(cfg_dst)}'],
                            stdin=subprocess.PIPE)
                        p_ts.communicate(input=buf.read())
                        p_ts.wait()
                except:
                    traceback.print_exc()

                time.sleep(1)
        t_watch = threading.Thread(target=watch_for_config_changes)
        t_watch.daemon = True
        t_watch.start()

    if STATIC_BUILD:
        # Pop open the UI for the user
        def open_browser():
            time.sleep(1.5)
            import webbrowser
            try:
                webbrowser.open(f'http://localhost:{port}')
            except ex:
                traceback.print_exc()
        open_browser_thread = threading.Thread(target=open_browser)
        open_browser_thread.daemon = True
        open_browser_thread.start()

    # Start the webserver for the endpoint, passing along the
    # config directory for the distribution and the port at
    # which the FAW server would be accessible (from the host)
    server_thread = start_server(config, port)

    # To ensure that we kill the FAW container (which we are just
    # about to start) on exiting, including during abnormal exits,
    # we will register ourselves an atexit listener
    def on_exit(faw_docker_id=docker_id, server_thread=server_thread):
        logging.info("In at exit processing")
        # Because we are in an exit handler, let us not throw an exception
        r = subprocess.run(f"docker ps -a | grep {faw_docker_id}", shell=True)
        if r.returncode == 0:
            # We can just stop the container and it should be removed
            # since we will have started it with a "--rm"
            subprocess.run(['docker', 'stop', faw_docker_id])

        # Kill the webserver gracefully
        server_thread.stop_server()
        server_thread.join(3)

    atexit.register(on_exit)

    # Launch the FAW container with the appropriate command line
    volume_mount_params = [
        '-v', f'{pdf_dir}:/home/pdf-files',
        '-v', f'{IMAGE_TAG+VOLUME_SUFFIX}:/data/db'
    ]

    # In build mode, this variable will not be set:
    if 'FAW_HOST_CI_LOG_DIR' in os.environ:
        logdir = os.environ['FAW_HOST_CI_LOG_DIR']
        logfile_target_loc = '/var/log/ci-container'
        volume_mount_params.extend(['-v', f'{logdir}:{logfile_target_loc}'])

    faw_command_line = [
        'docker', 'run', '--rm', '--detach',
        '--log-driver', 'none',
        '--name', docker_id,
        '-e', f'DB={db_name}',
        '-p', f'{port}:8123'
    ] + volume_mount_params + extra_flags

    logging.info(f"FAW command line: {faw_command_line}")
    subprocess.check_call(faw_command_line)

    # If in an interactive mode, run the FAW cli
    # TODO: Not sure this would return a "good" exit code on Ctrl+C. Figure that out.
    # So currently we just don't check the exit status!
    if (not running_as_service):
        subprocess.run(['docker', 'exec', '-it', docker_id, 'faw-cli.py'])
    else:
        # Wait for a few seconds and write out a notification
        # TODO: Do this in a loop until FAW starts up, maybe?
        # But then you have to handle FAW refusing to start up.
        time.sleep(5)
        _service_write_ready()

        # Wait for ever
        while True:
            time.sleep(1)


def _check_config_file(config, build_dir):
    """For non-deployment editions, we must load config from a json5 file.

    This also gets run for deployments, as this function is responsible for
    merging multiple configs into the single root config.

    This function also contains all schema validation of json5 files, as
    the version stored in the observatory image is separate.
    """
    global CONFIG_FOLDER, IMAGE_TAG

    # Not a deployment -- requires CONFIG_FOLDER
    # if CONFIG_FOLDER is None:
        # assert config is not None
        # CONFIG_FOLDER = config
    # else:
        # assert config is None

    import pyjson5, schema as s
    # config_data = pyjson5.load(open(os.path.join(CONFIG_FOLDER,
            # 'config.json5')))
    config_data = pyjson5.load(open(os.path.join(config,
            'config.json5')))

    # Keys which get merged
    toplevel_config = [
        'file_detail_views',
        'decision_views',
        'parsers',
        'parser_parsers_shared',
        'pipelines',
        'tasks',
    ]
    # Use this instead of None for validation purposes
    default_cwd = '/home/dist'

    # Before applying schema, merge in child configs. Do this in order of
    # increasing modification time. This is important so that developers
    # don't have to keep rebuilding unnecessary stages.
    child_configs = []
    for child_folder in _plugin_folders(config=config):
        child_path = child_folder.local_path
        if not os.path.isdir(child_path):
            continue
        child_config_path = os.path.join(child_path, 'config.json5')
        if not os.path.lexists(child_config_path):
            continue

        child_config = pyjson5.load(open(child_config_path))
        if child_config.pop('disabled', None):
            # Do not integrate this child config
            continue

        modtime = os.path.getmtime(child_config_path)
        child_configs.append((modtime, child_folder, child_config))

    # Stable sort; mod time first, then child name
    child_configs.sort(key=lambda m: (m[0], m[1].name))
    for _, child_folder, child_config in child_configs:
        # First traversal -- patch keys and values
        nodes = [([], child_config)]
        while nodes:
            path, src = nodes.pop()
            for k, v in src.items():
                ## Rewrite v
                # Check executables; amend cwd so default matches plugin
                if path and path[-1] in toplevel_config:
                    if 'cwd' in v:
                        if not v['cwd'].startswith('/'):
                            v['cwd'] = f'{child_folder.prod_path}/' + v['cwd']
                    else:
                        v['cwd'] = child_folder.prod_path

                    # Note that, since config merge is recursive, changes to
                    # `cwd` are automatically propagated to lower-level
                    # `toplevel_config` keys. This primarily affects pipelines.

                # Docker build stage patch
                if path == ['build', 'stages']:
                    if 'commands' in v:
                        v['commands'] = [
                                vv
                                    .replace('{disttarg}', child_folder.prod_path)
                                    .replace('{dist}',
                                        os.path.relpath(child_folder.local_path, build_dir))
                                for vv in v['commands']]

                if isinstance(v, dict):
                    nodes.append((path + [k], v))

        # Second traversal -- merge
        nodes = [([], config_data, child_config)]
        while nodes:
            path, dst, src = nodes.pop()
            for k, v in src.items():
                ## Rewrite k
                # Docker build stage patch -- rewrites `k`
                if path == ['build', 'stages']:
                    # Adding a build stage. If not 'base' or 'final', then
                    # prepend config folder name
                    if k not in ['base', 'final']:
                        k = f'{child_folder.name}_{k}'

                # New entries only for these
                if len(path) == 1 and path[0] in toplevel_config:
                    # Amend these with the child's name, to allow for copying
                    k = f'{child_folder.name.replace("_", "-")}-{k}'
                    assert k not in dst, f'Cannot extend {path} {k}; must be new'

                ## Check for merge type
                # Check if new -- if so, assign and be done
                if k not in dst:
                    if isinstance(v, dict):
                        # Must merge, otherwise key rewriting (the above code
                        # that inserts folder names) won't work for features
                        # in an extension that weren't in the base config.
                        dst[k] = {}
                    else:
                        # Non-existent key; add it to the dictionary
                        dst[k] = v
                        continue

                # Do merge
                if isinstance(v, dict):
                    if not isinstance(dst[k], dict):
                        raise ValueError(f'{path} {k} type does not match base config')

                    # Dictionary merge
                    nodes.append((path + [k], dst[k], v))
                elif isinstance(v, list):
                    if not isinstance(dst[k], list):
                        raise ValueError(f'{path} {k} type does not match base config')

                    # Add to end.
                    dst[k].extend(v)
                elif v is None or isinstance(v, (int, float, str)):
                    # Scalar overwrite
                    dst[k] = v
                else:
                    raise ValueError(f'May not extend {path} {k}: base config type {dst[k]}')

    # Pull in parser-specific schema
    import importlib.util
    spec = importlib.util.spec_from_file_location('etl_parse',
            os.path.join(faw_dir, 'faw', 'pdf-etl-parse', 'parse_schema.py'))
    etl_parse = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(etl_parse)

    schema_views = {
            s.Optional('decision_views', default={}): s.Or({}, {
                str: {
                    'label': str,
                    'type': 'program',
                    'exec': [s.Or(
                        s.And(str, lambda x: not x.startswith('<')),
                        s.And(str, lambda x: x in [
                            '<filesPath>', '<apiInfo>', '<jsonArguments>', '<mongo>', '<outputHtml>',
                            '<workbenchApiUrl>']),
                        )],
                    s.Optional('cwd', default=default_cwd): str,
                    'execStdin': s.And(str, lambda x: all([
                        y.group(0) in ['<referenceDecisions>', '<statsbyfile>']
                        for y in re.finditer('<[^>]*>', x)]),
                        error="Must be string with any <'s being one of: "
                            "<referenceDecisions>, <statsbyfile>"),
                },
            }),
            s.Optional('file_detail_views', default={}): s.Or({}, {
                str: {
                    'label': str,
                    'type': 'program_to_html',
                    'exec': [str],
                    s.Optional('cwd', default=default_cwd): str,
                    s.Optional('outputMimeType', default='text/html'): str,
                },
            }),
    }

    # NOTE -- primary schema validation is here, but NOT for submodules such
    # as pdf-etl-parse.
    sch = s.Schema({
        'name': s.And(str, s.Regex(r'^[a-zA-Z0-9-]+$')),
        s.Optional('file_transform', default=None): s.Or(None, {
            'version': str,
            'exec': [str],
        }),
        # parsers validated by pdf-etl-parse
        s.Optional('parsers', default={}): etl_parse.schema_get(),
        s.Optional('parser_parsers_shared', default={}): s.Or({}, {
                s.And(str, lambda x: '_' not in x): {
                    s.Optional('cwd', default=default_cwd): str,
                    s.Optional('disabled', default=False): bool,
                    'parse': etl_parse.schema_get_parser_parser(),
                },
        }),
        s.Optional('parserDefaultTimeout', default=30): s.Or(float, int),
        'decision_default': s.Or(
            str,
            s.And(
                {'file': str},
                s.Use(lambda x: pathlib.Path(os.path.join(config, x['file'])).read_text())
            )
        ),
        s.Optional('pipelines', default={}): s.Or({}, {
            s.And(str, lambda x: '_' not in x and '.' not in x,
                    error='Must not have underscore or dot'): {
                s.Optional('label'): str,
                # ['pipelines']['cwd'] should not be used directly!
                s.Optional('cwd', default=default_cwd): str,
                s.Optional('disabled', default=False): s.Or(True, False),
                s.Optional('tasks', default={}): s.Or({},
                    s.And(
                        {
                            s.And(str, lambda x: '_' not in x and '.' not in x,
                                    error='Must not have underscore or dot'): {
                                s.Optional('disabled', default=False): s.Or(True, False),
                                'version': str,
                                'exec': [str],
                                s.Optional('cwd', default=default_cwd): str,
                                s.Optional('dependsOn', default=[]): [str],
                            },
                        },
                        lambda x: all([d in x for _, task in x.items() for d in task['dependsOn']]),
                        error="Task `dependsOn` had invalid name",
                    )),
                s.Optional('parsers', default={}): etl_parse.schema_get(),
                **schema_views,
            },
        }),
        'build': {
            'stages': {
                str: {
                    s.Optional('from'): str,
                    s.Optional('commands'): [str],
                    s.Optional('copy_output'): {
                        str: s.Or(str, bool),
                    },
                },
            },
        },
        **schema_views,
    })
    try:
        config_data = sch.validate(config_data)
    except Exception as e:
        traceback.print_exc()
        sys.stderr.write('\n'.join([str(v) for v in e.autos]) + '\n')
        sys.exit(1)
    IMAGE_TAG = config_data['name']
    return config_data


def _check_image(development, config,  config_data, build_dir, build_faw_dir):
    """Ensure that the docker image is loaded, if we are using a packaged
    version, or rebuild latest, if using a development version.

    Args:
        development (bool): True if image is a development image, which will
            have certain folders mounted.
        config_data (dict): The loaded config.json5 data.
        build_dir (str): The Docker build context folder.
        build_faw_dir (str): The path, relative to the docker build context,
            of the FAW code.
    """
    build_local = development or os.path.lexists(os.path.join(faw_dir,
            'faw', 'pdf-observatory'))
    if not build_local:
        # For e.g. version updates, load the image first.
        image_file = os.path.join(faw_dir, IMAGE_TAG + '.image')
        if os.path.lexists(image_file):
            logging.info('Loading docker image...')
            subprocess.check_call(['docker', 'load', '-i', image_file])
            os.unlink(image_file)

        o = subprocess.check_output(['docker', 'image', 'ls',
                IMAGE_TAG, '--format', '{{.ID}}'])
        if not o.strip():
            raise Exception("Image not correctly installed, and not "
                    "locally present.  Try re-extracting and running "
                    "again?")
        return o

    # Need to build the image
    suffix = '-dev' if development else ''
    assert config_data is not None, 'required --config?'

    # Generate the contents for the dockerfile
    dockerfile_contents = _create_dockerfile_contents(development, config, config_data, build_dir, build_faw_dir)
    logging.info('='*79)
    logging.info(dockerfile_contents)
    logging.info('='*79)

    # And build the docker image
    # TODO: Consider taking the image tag/name as input?
    img_tag = _build_docker_image(build_dir, config, suffix, dockerfile_contents)

    # Return the tag for the build
    return img_tag

def _create_dockerfile_contents(development, config, config_data, build_dir, build_faw_dir):
    dockerfile = []
    dockerfile_middle = []
    dockerfile_final = []
    dockerfile_final_postamble = []

    stages_written = set()

    # Hardcoded stages required for observatory.
    stages_hardcoded = {
        # For production builds, the mongodb server and the FAW utilities reside
        # in the same image. So, clone the latest mongodb from the official
        # image.
        'obs__mongo': {
            'from': 'mongo:4.4.12',
            'copy_output': {
                '/var/log/mongodb': True,
                #'/var/lib/dpkg/info': True,
                '/lib/systemd/system/mongod.service': '/lib/systemd/system/',
                #'/usr/share/doc': True,
                '/usr/share/lintian/overrides/mongodb-*': '/usr/share/lintian/overrides/',
                '/usr/bin/mongo*': '/usr/bin/',
                '/usr/local/bin/docker-entrypoint.sh': True,
                '/etc/apt/apt.conf.d/docker*': '/etc/apt/apt.conf.d/',
                '/etc/dpkg/dpkg.cfg.d/docker*': '/etc/dpkg/dpkg.cfg.d/',
                # COPY --from=mongo /.dockerenv /
                # COPY --from=mongo /docker-entrypoint-initdb.d /
                '/etc/apt/sources.list.d/mongodb-org.list': True,
                #'/etc/apt/trusted.gpg.d/mongodb.gpg': True,
                '/etc/mongod.conf.orig': True,
            },
        },
    }

    # Important! User changes should trigger as little rebuild as possible.
    # One way to accomplish this is by affected dockerfile_middle and dockerfile_final
    # with system code as early as possible.
    if development:
        # Development extensions... add not-compiled code directories.
        dockerfile_final.append(r'''
            # Install npm globally, so it's available for debug mode
            RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - && apt-get install -y nodejs
            # Install watchgod, which allows for live-reloading analysis sets
            # on file changes.
            RUN pip3 install watchgod
            ''')
    else:
        # Production extensions...
        dockerfile_middle.append(rf'''
            FROM base AS ui-builder
            # Install npm locally, only for the build.
            RUN curl -sL https://deb.nodesource.com/setup_16.x | bash - && apt-get install -y nodejs
            COPY {build_faw_dir}/faw/pdf-observatory/ui /home/pdf-observatory/ui
            RUN cd /home/pdf-observatory/ui \
                && npm install \
                && npm run build
            RUN cd /home/pdf-observatory/ci \
                && npm install \
                && npm run build
            ''')

    # Always install observatory component dependencies as first part of final
    # stage (to minimize rebuilds on user code changes)
    dockerfile_final.append(rf'''
            COPY {build_faw_dir}/faw/pdf-etl-parse/requirements.txt /home/pdf-etl-parse/requirements.txt
            RUN pip3 install -r /home/pdf-etl-parse/requirements.txt
            COPY {build_faw_dir}/faw/pdf-observatory/requirements.txt /home/pdf-observatory/requirements.txt
            RUN pip3 install -r /home/pdf-observatory/requirements.txt
    ''')

    # The below commands should be quick to run, as they will happen any time
    # the user changes their code.
    config_rel_dir = os.path.relpath(config, build_dir)
    if development:
        dockerfile_final_postamble.append(rf'''
            # Add not-compiled code directories
            COPY {build_faw_dir}/faw/pdf-etl-parse /home/pdf-etl-parse
            #COPY {build_faw_dir}/faw/pdf-observatory /home/pdf-observatory
            ''')

        # The CONFIG_FOLDER will be mounted as dist at runtime.
        # The ALL_FOLDER will be mounted as `all` at runtime.
    else:
        dockerfile_final_postamble.append(rf'''
            # Add not-compiled code directories; omit "ui" for observatory
            COPY {build_faw_dir}/faw/pdf-etl-parse /home/pdf-etl-parse
            COPY {build_faw_dir}/faw/pdf-observatory/*.py /home/pdf-observatory/
            COPY {build_faw_dir}/faw/pdf-observatory/dask-worker-runner /home/pdf-observatory/dask-worker-runner
            COPY {build_faw_dir}/faw/pdf-observatory/mongo_queue_helper /home/pdf-observatory/mongo_queue_helper
            COPY {build_faw_dir}/faw/docker-bin/* /usr/bin/
            COPY --from=ui-builder /home/pdf-observatory/ui/dist /home/pdf-observatory/ui/dist
            # Copy shared plugins
            COPY {shlex.quote(build_faw_dir + '/all')} /home/all
            # The final stage must always have the distribution folder available as
            # /home/dist; do this after copying the base material and building the
            # base UI, so user changes trigger those rebuilds infrequently.
            COPY {shlex.quote(config_rel_dir)} /home/dist
            ''')

    for stage, stage_def in {**config_data['build']['stages'],
            **stages_hardcoded}.items():
        if not stages_written and stage != 'base':
            raise ValueError(f'First stage must be "base", not {stage}')

        stage_commands = stage_def.get('commands', [])
        stage_commands_new = []
        for s in stage_commands:
            try:
                ss = s.format(dist=config_rel_dir, disttarg='/home/dist')
            except KeyError:
                raise KeyError(f'While formatting: {s}')
            else:
                stage_commands_new.append(ss)
        stage_commands = stage_commands_new

        if stage == 'final':
            assert stage_def.get('from') is None
            assert stage_def.get('copy_output') is None
            dockerfile_final.extend(stage_commands)
            continue

        base = stage_def.get('from', 'base')
        if base == 'base' and stage == 'base':
            raise ValueError("Stage 'base' must have a 'from' defined")

        stages_written.add(stage)
        dockerfile.append(f'FROM {base} AS {stage}')
        if stage == 'base':
            # Ensure that e.g. curl, python3, python3-pip, and wget all get installed
            stage_commands = [
                    'ENV DEBIAN_FRONTEND=noninteractive',
                    'RUN apt-get update && apt-get install -y curl python3 python3-pip wget',
                    ] + stage_commands
        dockerfile.extend(stage_commands)

        copy_commands = _generate_copy_commands(stage, stage_def)
        dockerfile_final_postamble.extend(copy_commands)

    # Regardless, there's some glue code to create the final image.
    dockerfile_middle.append(rf'''
            FROM base
            ''')
    # This all happens very quickly, so do it very last
    dockerfile_final_postamble.append(rf'''
            ## s6 overlay for running mongod and observatory side by side
            #ADD https://github.com/just-containers/s6-overlay/releases/download/v1.21.8.0/s6-overlay-amd64.tar.gz /tmp/
            COPY {build_faw_dir}/faw/s6-overlay-amd64.tar.gz /tmp/
            # Updated for ubuntu 20.04, for which /bin is a symlink
            # Still need old extract command for previous versions.
            RUN bash -c '\
                    ([ -L /bin ] \
                        && ( \
                            tar xzf /tmp/s6-overlay-amd64.tar.gz -C / --exclude="./bin" \
                            && tar xzf /tmp/s6-overlay-amd64.tar.gz -C /usr ./bin \
                        ) \
                        || ( \
                            tar xzf /tmp/s6-overlay-amd64.tar.gz -C / \
                        ) \
                    ) \
                    && rm /tmp/s6-overlay-amd64.tar.gz \
                    '
            # Setup service files to automatically run mongodb in the background
            # Note that logging for mongodb goes to /var/log/mongodb; see
            # https://github.com/just-containers/s6-overlay/issues/291
            # Tell S6 to pass environment variables on to child processes
            ENV S6_KEEP_ENV 1
            # Tell python to never buffer output. This is vital for preventing
            # some "write would block" messages.
            ENV PYTHONUNBUFFERED 1
            # Ensure any python code running (dask, user code) has access to
            # the faw_pipelines_util and user packages.
            ENV PYTHONPATH /home/dist:/home/all:/home/pdf-observatory
            # Always use 'bash' from this point forward, because 'echo' commands
            # are inconsistent between sh and bash.
            SHELL ["/bin/bash", "-c"]
            # Mongodb service
            RUN \
                mkdir -p /etc/cont-init.d \
                && echo -e '#! /bin/sh\nmkdir -p /var/log/mongodb\nchown -R nobody:nogroup /var/log/mongodb' > /etc/cont-init.d/mongod \
                && mkdir -p /etc/services.d/mongod \
                && echo -e '#! /bin/sh\nmongod --ipv6 --bind_ip_all' >> /etc/services.d/mongod/run \
                && chmod a+x /etc/services.d/mongod/run \
                && mkdir /etc/services.d/mongod/log \
                && echo -e '#! /usr/bin/execlineb -P\nlogutil-service /var/log/mongodb' >> /etc/services.d/mongod/log/run \
                && chmod a+x /etc/services.d/mongod/log/run
            # Observatory service (modifications must also change faw/teaming/pyinfra/deploy.py)
            RUN \
                mkdir -p /etc/cont-init.d \
                && echo -e '#! /bin/sh\nmkdir -p /var/log/observatory\nchown -R nobody:nogroup /var/log/observatory' > /etc/cont-init.d/observatory \
                && mkdir /etc/services.d/observatory \
                    && echo -e '#! /bin/bash\ncd /home/pdf-observatory\npython3 main.py /home/pdf-files "127.0.0.1:27017/${{DB}}" --in-docker --port 8123 ${{OBS_PRODUCTION}} --config ../config.json 2>&1' >> /etc/services.d/observatory/run \
                    && chmod a+x /etc/services.d/observatory/run \
                && mkdir /etc/services.d/observatory/log \
                    && echo -e '#! /usr/bin/execlineb -P\nlogutil-service /var/log/observatory' > /etc/services.d/observatory/log/run \
                    && chmod a+x /etc/services.d/observatory/log/run \
                && echo OK
            # Dask service (scheduler AND worker initially; teaming script fixes this)
            # Note -- listens to all IPv4 and IPv6 addresses by default.
            RUN \
                mkdir -p /etc/cont-init.d \
                && echo -e '#! /bin/sh\nmkdir -p /var/log/dask-scheduler\nchown -R nobody:nogroup /var/log/dask-scheduler' > /etc/cont-init.d/dask-scheduler \
                && mkdir /etc/services.d/dask-scheduler \
                    && echo -e '#! /bin/bash\ncd /home/dist\ndask-scheduler --port 8786 --dashboard-address :8787 2>&1' >> /etc/services.d/dask-scheduler/run \
                    && chmod a+x /etc/services.d/dask-scheduler/run \
                && mkdir /etc/services.d/dask-scheduler/log \
                    && echo -e '#! /usr/bin/execlineb -P\nlogutil-service /var/log/dask-scheduler' > /etc/services.d/dask-scheduler/log/run \
                    && chmod a+x /etc/services.d/dask-scheduler/log/run \
                && echo OK
            RUN \
                mkdir -p /etc/cont-init.d \
                && echo -e '#! /bin/sh\nmkdir -p /var/log/dask-worker\nchown -R nobody:nogroup /var/log/dask-worker' > /etc/cont-init.d/dask-worker \
                && mkdir /etc/services.d/dask-worker \
                    && echo -e '#! /bin/bash\ncd /home/dist\n/home/pdf-observatory/dask-worker-runner localhost:8786 2>&1' >> /etc/services.d/dask-worker/run \
                    && chmod a+x /etc/services.d/dask-worker/run \
                && mkdir /etc/services.d/dask-worker/log \
                    && echo -e '#! /usr/bin/execlineb -P\nlogutil-service /var/log/dask-worker' > /etc/services.d/dask-worker/log/run \
                    && chmod a+x /etc/services.d/dask-worker/log/run \
                && echo OK
            # Add 'timeout' script to /usr/bin, for collecting memory + CPU time
            # information.
            COPY {build_faw_dir}/faw/timeout-master /home/timeout
            RUN chmod a+x /home/timeout/timeout \
                && ln -s /home/timeout/timeout /usr/bin/timeout_pshved
            # Container runtime properties
            ENV LC_ALL C.UTF-8
            ENV LANG C.UTF-8
            ENV DB observatory-default-data
            ENV OBS_PRODUCTION "{'--production' if not development else ''}"
            ENTRYPOINT ["/init"]
            ''')

    # FIXME when not fully utilizing cores during re-parse:
    # change dask-worker to be wrapped in script which looks at os.cpu_count()
    # and makes 1 proc per 2-4 cores.

    config_json = _export_config_json(config_data)
    # Shell execution limit
    config_json = config_json.replace('\\', '\\\\')
    # We always process the deployment-specific json5 file into
    # /home/config.json in the repo.
    dockerfile_final_postamble.append(fr'''
            RUN echo -e {shlex.quote(config_json)} > /home/config.json
    ''')

    dockerfile = '\n'.join(dockerfile + dockerfile_middle
            + dockerfile_final + dockerfile_final_postamble)

    return dockerfile

def _build_docker_image(build_dir, config, suffix, dockerfile_contents):
    r = subprocess.run(['docker', 'build', '-t', get_faw_image_name(IMAGE_TAG, config, suffix),
        '-f', '-', '.'], cwd=build_dir, input=dockerfile_contents.encode())
    if r.returncode != 0:
        raise Exception("Docker build failed; see above")
    else:
        return IMAGE_TAG + suffix

def _export_config_json(config_data):
    import json
    config_data_client = config_data.copy()
    config_data_client.pop('build', None)
    config_json = json.dumps(config_data_client)
    return config_json

def _generate_copy_commands(stage, stage_def, *, force_prefix=None):
    commands = []
    for k, v in stage_def.get('copy_output', {}).items():
        if v is True:
            v = k

        # If force_prefix has been provided, the target path is forced to be
        # <force_prefix>/<v>. Otherwise it will simply be <v>
        if force_prefix:
            suffix = v[1:] if v[0] == '/' else v  # Required for os.path.join to work correctly
            new_path = os.path.join(force_prefix, suffix)
            commands.append(f'COPY --from={stage} {k} {new_path}')
        else:
            commands.append(f'COPY --from={stage} {k} {v}')

    return commands

def _mongo_copy(db_name, copy_mongo_from, copy_mongo_to):
    # Only spin up mongo -- the only reason we make it externally connectible is
    # to ensure that it's working OK. Otherwise, utilize services directly in
    # the image.

    import asyncio
    import bson
    import motor.motor_asyncio

    dummy_mongo_port = 27015

    async def run_and_dump():
        # print(f'Spinning up mongo inside FAW container...', file=sys.stderr)
        logging.info(f'Spinning up mongo inside FAW container...')
        container_name = 'faw-workbench-mongo-copy'
        p = await asyncio.create_subprocess_exec(
                'docker', 'run', '--rm', # '-it',
                '--name', container_name,
                '-v', f'{IMAGE_TAG+VOLUME_SUFFIX}:/data/db',
                '-p', f'{dummy_mongo_port}:27017',
                '--entrypoint', 'mongod',
                IMAGE_TAG + '-dev',
                '--bind_ip_all',
                stdin=asyncio.subprocess.DEVNULL,
                stdout=asyncio.subprocess.DEVNULL,
                stderr=asyncio.subprocess.DEVNULL,
        )

        try:
            # Wait for docker to spin up
            async def test_connection():
                p_exit = p.wait()
                try:
                    await asyncio.wait_for(asyncio.shield(p_exit), timeout=0.1)
                    raise ValueError('docker exited early')
                except asyncio.TimeoutError:
                    # OK, docker hasn't exited
                    pass

                client = motor.motor_asyncio.AsyncIOMotorClient(
                        f'mongodb://127.0.0.1:{dummy_mongo_port}')
                try:
                    await asyncio.wait_for(client.list_databases(),
                            timeout=10)
                    # OK, got names, connected
                    return True
                except asyncio.TimeoutError:
                    # Not yet.
                    return False
            while not await test_connection():
                pass

            # print('Docker w/ mongo spun up and ready.', file=sys.stderr)
            logging.info('Docker w/ mongo spun up and ready.')
            if copy_mongo_from is not None:
                if os.path.isabs(copy_mongo_from):
                    # Restore backup from file
                    # print('Restoring from file.', file=sys.stderr)
                    logging.info('Restoring from file.')
                    pr = await asyncio.create_subprocess_exec(
                            'docker', 'exec', '-i',
                            container_name,
                            'mongorestore', '--archive', '--drop', #'--oplogReplay',
                            '--host', 'localhost', '--port', '27017',
                            '--db', db_name,
                            stdin=open(copy_mongo_from, 'rb'),
                            stdout=asyncio.subprocess.PIPE,
                            stderr=asyncio.subprocess.PIPE)
                    stdout, stderr = await pr.communicate()
                    r_code = await pr.wait()
                    if r_code != 0:
                        logging.error('STDOUT')
                        logging.error(stdout.decode())
                        logging.error('STDERR')
                        logging.error(stderr.decode())
                        raise ValueError('Mongorestore crashed')
                else:
                    logging.info(f'Restoring from running mongod.')
                    raise NotImplementedError
                    # Restore backup from remote source
                    mongo_host, mongo_db = copy_mongo_from.split('/', 1)
                    client = motor.motor_asyncio.AsyncIOMotorClient(
                            'mongodb://' + mongo_host)
                    assert '/' not in mongo_db
                    client_db = client[mongo_db]

                    dest = motor.motor_asyncio.AsyncIOMotorClient(
                            'mongodb://127.0.0.1:27015')
                    await dest.drop_database(db_name)
                    dest_db = dest[db_name]

                    cols = [c['name'] for c in await client_db.list_collections()]
                    for col in cols:
                        logging.info(f'Copying {col}...')
                        client_col = client_db[col]
                        dest_col = dest_db[col]

                        copied = [0]
                        batch = []
                        async def dump_batch():
                            if not batch:
                                return

                            # Fix 2020-02-03: bson.errors.InvalidDocument: key 'Error opening PDF file.' must not contain '.'
                            if col == 'invocationsparsed':
                                for b in batch:
                                    to_fix = []
                                    for k in b['result'].keys():
                                        if '.' in k:
                                            to_fix.append(k)

                                    for k in to_fix:
                                        b['result'][k.replace('.', '')] = b['result'].pop(k)

                            try:
                                await dest_col.insert_many(batch)
                            except bson.errors.InvalidDocument:
                                #import pprint
                                #pprint.pprint(batch)
                                raise
                            copied[0] += len(batch)
                            batch.clear()
                        async for doc in client_col.find():
                            batch.append(doc)
                            if len(batch) >= 1024:
                                await dump_batch()
                        await dump_batch()
                        logging.info(f'...copied {copied[0]}')
            if copy_mongo_to is not None:
                # Restore to file
                logging.info('Backing up database.', file=sys.stderr)
                assert os.path.isabs(copy_mongo_to), copy_mongo_to
                pr = await asyncio.create_subprocess_exec(
                        'docker', 'exec', '-i',
                        container_name,
                        'mongodump', '--archive', #'--oplog',
                        '--host', 'localhost', '--port', '27017',
                        '--db', db_name,
                        stdin=asyncio.subprocess.DEVNULL,
                        stdout=open(copy_mongo_to, 'wb'),
                        stderr=asyncio.subprocess.PIPE)
                _stdout, stderr = await pr.communicate()
                r_code = await pr.wait()
                if r_code != 0:
                    logging.error('STDERR')
                    logging.error(stderr.decode())
                    raise ValueError('Mongodump crashed')
            logging.info('All done - OK')
        finally:
            # Stop docker
            # Kill stops the interface, but the container keeps going.
            # Instead, use terminate.
            p.terminate()
            await p.wait()
    asyncio.run(run_and_dump())


def _check_build_stage_change_and_update(
    *, new_config, old_config, development, config_dir, build_dir, build_faw_dir, current_docker_id
):
    logging.info(f"Checking for stage updates ...")

    # And we should be operating in development mode
    assert development, "Automatic rebuild on build stage configuration updates are only supported in development mode"

    # Check if any build stages in the new_config has changed, though we only
    # care about stages that specify things that can be copied
    updated_stages = {}
    for stage_name, stage_data in new_config['build']['stages'].items():
        old_stage_data = old_config['build']['stages'].get(stage_name)
        if old_stage_data is None or old_stage_data != stage_data:
            if 'copy_output' not in stage_data:
                logging.warning("=" * 80)
                logging.warning(f"Stage '{stage_name}' has been updated, but will have no effect on the running FAW as the stage lacks a 'copy_output' specification")
                logging.warning("=" * 80)
                continue

            updated_stages[stage_name] = stage_data
            logging.debug(f"Stage {stage_name} has been updated")
        else:
            logging.debug(f"Stage {stage_name} has no changes")

    # If there are any updated stages, build them
    if not updated_stages:
        logging.info("No updated stages. Returning")
        return

    # To build a new image, first create the dockerfile contents
    dockerfile_contents = _create_dockerfile_contents(
        development=development, config=config_dir, config_data=new_config,
        build_dir=build_dir, build_faw_dir=build_faw_dir
    )

    # We need to collect all the outputs from the updated stages in some place
    # where we can extract them out of the image (the container really) cleanly.
    # To this end, we insert extra COPY commands to the dockerfile that copy
    # the files under a specific temp directory, but retaining the full path.
    # We then create a tar file with all the copied files for extraction.
    temp_name = str(uuid.uuid4())
    temp_dir_name = os.path.join('/tmp', temp_name)
    commands = []
    for stage_name, stage_data in updated_stages.items():
        copy_cmds = _generate_copy_commands(stage_name, stage_data, force_prefix=temp_dir_name)
        commands.extend(copy_cmds)

    tar_file_name = f"/tmp/{temp_name}.tar"
    tar_command = f"RUN cd {temp_dir_name} && find . -type f -print0 | tar -cvf {tar_file_name} --null -T -"
    commands.append(tar_command)

    dockerfile_contents += ("\n" + "\n".join(commands))

    # Finally build the image
    # NOTE: Currently we are using the same image tag that is currently
    # running in FAW.
    logging.info("Rebuilding FAW ...")
    image_tag = _build_docker_image(
        build_dir=build_dir, config=config_dir, suffix="-dev", dockerfile_contents=dockerfile_contents
    )
    logging.info("Completed Rebuild")

    # Create a container with this image and copy files appropriately
    new_container_id = f"{image_tag}-rebuild"

    try:
        subprocess.check_call(['docker', 'create', f"--name={new_container_id}", image_tag])
        logging.info(f"Completed Container Creation - {new_container_id}")

        # Copy the tar file we created during the build process locally and then stream it out to the
        # destination container. NOTE: The explicit tar creation above ensures that the resultant tar
        # has the right hierarchy to allow us to do this.
        logging.info("Starting to copy files ...")
        subprocess.check_call(["docker", "cp", f"{new_container_id}:{tar_file_name}", tar_file_name])
        subprocess.check_call(f"docker cp - {current_docker_id}:/ < {tar_file_name}", shell=True)
        logging.info("Copying files completed")
    finally:
        subprocess.run(['docker', 'rm', new_container_id])
        logging.info(f"Removed Container - {new_container_id}")


def start_server(config_dir, faw_port):
    """
    Start a server and create an endpoint to upload config tars.
    """

    from aiohttp import web, request

    class ServerThread(threading.Thread):

        def __init__(self):
            super().__init__(daemon=True)

            self.app = web.Application(debug=True)
            self.app.add_routes([
                web.post('/configuration', self._update_config),
                web.put('/configuration', self._update_config),
                web.post('/decisions', self._get_decisions)
            ])
            self.runner = web.AppRunner(self.app)
            
            # NOTE: The FAW server is in a different container, BUT due to the way we run
            # that container, it should be accessible on the host of this container at a
            # configurable port. We can access the host machine using special hostname 
            # "host.docker.internal" as we are running *THIS* container with the appropriate
            # `add-host` invocation (for Linux)
            self.faw_url = f'http://host.docker.internal:{faw_port}'

            # logging.basicConfig(level=logging.DEBUG)

        def stop_server(self):
            self.stop_event.set_result(1)

        def run(self):
            loop = asyncio.new_event_loop()
            asyncio.set_event_loop(loop)

            loop.run_until_complete(self.runner.setup())

            site = web.TCPSite(self.runner, '0.0.0.0', 9001)
            loop.run_until_complete(site.start())

            logging.info("Site started")

            self.stop_event = loop.create_future()
            loop.run_until_complete(self.stop_event)

            logging.info("Site stopping")

            loop.run_until_complete(self.runner.cleanup())

        async def _update_config(self, request):
            data = await request.post()

            config_data = data['config']

            with tarfile.open(fileobj=config_data.file) as t:
                t.extractall(config_dir)

            return web.json_response({'status': 'success'})

        async def _get_decisions(self, req):
            # We are going to send the same request to FAW server and
            # stream out the response
            data = await req.post()
            async with request('POST', f"{self.faw_url}/decisions", data=data) as resp:
                out_resp = web.StreamResponse()
                out_resp.content_type = resp.content_type
                await out_resp.prepare(req)
                
                while content := await resp.content.read(4096):
                    await out_resp.write(content)
                await out_resp.write_eof()
                
                return out_resp


    # Run it in a thread
    t = ServerThread()
    t.start()

    return t


def get_db_name(pdf_dir):
    pdf_dir = os.path.abspath(pdf_dir)
    pdf_dir_name = re.sub(r'[ /\.]', '-', os.path.basename(pdf_dir))
    hash = hashlib.sha256(pdf_dir.encode('utf-8')).hexdigest()
    return f'gfaw-{pdf_dir_name}-{hash[:8]}'


def get_faw_container_name(image_tag, config_dir, pdf_dir):
    if not image_tag:
        import pyjson5
        config_data = pyjson5.load(open(os.path.join(config_dir, 'config.json5')))
        image_tag = config_data['name']

    return f'gfaw-{image_tag}-{get_db_name(pdf_dir)}'


def get_faw_image_name(image_tag, config_dir, suffix):
    tag = image_tag or get_default_image_tag(config_dir)
    return tag + suffix


def get_default_image_tag(config_dir):
    import pyjson5
    config_data = pyjson5.load(open(os.path.join(config_dir, 'config.json5')))
    image_tag = config_data['name']
    return image_tag



@dataclasses.dataclass
class _PluginFolderDesc:
    """Paths are absolute.

    local_path indicates on host OS.
    prod_path indicates within docker.
    """
    name: str
    local_path: str
    prod_path: str
def _plugin_folders(*, config, include_root=False):
    """Yields `[_PluginFolderDesc]` for folders which contain
    `config.json5` files. Optionally, also yields the base config path.
    """
    r = []
    seen = set()
    def add(d):
        if d.name in seen:
            raise ValueError(f'Plugin {d.name} found twice?')
        seen.add(d.name)
        r.append(d)

    if include_root:
        add(_PluginFolderDesc(name='<root>', local_path=config,
                prod_path='/home/dist'))
    for p in os.listdir(config):
        pp = os.path.abspath(os.path.join(config, p))
        if os.path.isdir(pp) and os.path.lexists(os.path.join(pp, 'config.json5')):
            add(_PluginFolderDesc(name=p, local_path=pp,
                    prod_path=f'/home/dist/{p}'))
    for p in os.listdir(ALL_FOLDER):
        pp = os.path.abspath(os.path.join(ALL_FOLDER, p))
        pname = f'all__{p}'
        if os.path.isdir(pp) and os.path.lexists(os.path.join(pp, 'config.json5')):
            add(_PluginFolderDesc(name=pname, local_path=pp,
                    prod_path=f'/home/all/{p}'))
    return r


def _service_write_ready():
    logging.info("Writing daemon ready notification")
    with os.fdopen(SERVICE_NOTIFICATION_FD, "w") as f:
        f.write('Service is up\n')


if __name__ == '__main__':
    main()

