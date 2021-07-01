#! /usr/bin/env python3

"""This is the convenience script for developing, building, distributing, and
running end-user copies of the Galois Format Analysis Workbench.
"""

# IMPORTANT - Only stdlib here! We want to minimize dependencies required by
# downstream users. Other imports are allowed in functions not used by the
# version published as part of a build.
import argparse
import hashlib
import io
import os
import re
import shlex
import shutil
import stat
import subprocess
import sys
import tarfile
import textwrap
import threading
import time
import traceback
import webbrowser

# These variables are a bit strange, but they exist to make distribution easier.
# Basically, this script is copied and these variables are overwritten
# automatically for deployments.
#
# Shorthand scripts will overwrite `CONFIG_FOLDER`, and deployments will
# overwrite `IMAGE_TAG`.
CONFIG_FOLDER = None
IMAGE_TAG = None
VOLUME_SUFFIX = '-data'

# Keep track of the directory containing the FAW
faw_dir = os.path.dirname(os.path.abspath(__file__))

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

    parser = argparse.ArgumentParser(description=textwrap.dedent(main.__doc__),
            formatter_class=argparse.RawTextHelpFormatter)
    def folder_exists(v):
        assert os.path.lexists(v), f'Path must exist: {v}'
        return v
    def folder_exists_or_build(v):
        assert os.path.lexists(v) or v.startswith('build'), f'Path must exist or start with build: {v}'
        return v
    if CONFIG_FOLDER is None and IMAGE_TAG is None:
        parser.add_argument('config_dir', type=folder_exists, help="Folder "
                "containing configuration for workbench to analyze a format.")
    parser.add_argument('file_dir', type=folder_exists_or_build, help="Folder containing "
            "files to investigate.")
    parser.add_argument('--port', default=8123, type=int,
            help="Port on which Galois Workbench is accessed.")
    parser.add_argument('--port-mongo', default=None, type=int,
            help="If specified, port on which to expose the mongo instance.")
    parser.add_argument('--copy-mongo-from', default=None, type=str,
            help="Replace the pdf-etl database used by the observatory with a "
            "copy of an existing database. Format: localhost:27019/120pdfs")
    parser.add_argument('--development', action='store_true',
            help="Developer option: mount source code over docker image, for "
            "Vue.js hot reloading. Also adds `sys_ptrace` capability to docker "
            "container for profiling purposes.")
    args = parser.parse_args()
    pdf_dir = args.file_dir
    port = args.port
    port_mongo = args.port_mongo
    copy_mongo_from = args.copy_mongo_from
    development = args.development

    config_data = None
    if IMAGE_TAG is None:
        # Not a deployment -- need to load spec so we can build the image.
        if CONFIG_FOLDER is None:
            config = args.config_dir
        else:
            config = CONFIG_FOLDER
        config_data = _check_config_file(config)

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

    # Check that observatory image is loaded / built
    _check_image(development=development, config_data=config_data,
            build_dir=build_dir, build_faw_dir=build_faw_dir)
    if os.path.split(os.path.relpath(pdf_dir, faw_dir))[0] == 'build':
        assert not development, "Build cannot use --development"

        # Populate the given directory with a built version of the workbench.
        try:
            shutil.rmtree(pdf_dir)
        except FileNotFoundError:
            pass
        os.makedirs(pdf_dir)

        # Export docker image
        assert IMAGE_TAG is not None
        # relpath() here used to strip trailing slash, which messes up basename
        dist_name = os.path.basename(os.path.relpath(CONFIG_FOLDER))
        image_file_name = f'{IMAGE_TAG}.image'
        subprocess.check_call(
                f'docker save {IMAGE_TAG} | gzip > {os.path.join(pdf_dir, image_file_name)}',
                shell=True)

        # Export readme
        with \
                open(os.path.join('common', 'README-dist.md')) as fin, \
                open(os.path.join(pdf_dir, 'README.md'), 'w') as fout:
            fout.write(
                    re.sub(r'{distribution}', dist_name,
                        re.sub(r'{imageName}', IMAGE_TAG,
                            fin.read()
                        )
                    )
            )

        # Build modified script
        script_name = os.path.join(pdf_dir, f'workbench-{dist_name}.py')
        with open(__file__) as fin, open(script_name, 'w') as fout:
            fout.write(re.sub('IMAGE_TAG = [N]one', f'IMAGE_TAG = {repr(IMAGE_TAG)}',
                    fin.read()))
            st = os.stat(script_name)
            os.chmod(script_name, st.st_mode | stat.S_IEXEC)

        if False:
            # Package up whole file
            # On second thought, don't. It takes up disk space and the user could
            # run this step on their own
            file_name = os.path.abspath(os.path.join(os.path.abspath(pdf_dir), '..',
                    f'{config_data["name"]}.tar.gz'))
            try:
                os.remove(file_name)
            except FileNotFoundError:
                pass
            subprocess.check_call(['tar', '-czvf', file_name] + os.listdir(pdf_dir),
                    cwd=pdf_dir)

        print(f'Package available as {pdf_dir}')
        return

    # Hash absolute path to folder to generate consistent DB name.
    pdf_dir = os.path.abspath(pdf_dir)
    pdf_dir_name = re.sub(r'[ /\.]', '-', os.path.basename(pdf_dir))
    hash = hashlib.sha256(pdf_dir.encode('utf-8')).hexdigest()
    db_name = f'gfaw-{pdf_dir_name}-{hash[:8]}'

    if copy_mongo_from is not None:
        # Auxiliary command for copying data from an existing mongo instance.
        # Used internally.
        _mongo_copy(db_name=db_name, copy_mongo_from=copy_mongo_from)
        return

    extra_flags = []
    if port_mongo:
        extra_flags.extend(['-p', f'{port_mongo}:27017'])

    if not development:
        extra_flags.append(IMAGE_TAG)
    else:
        extra_flags.extend(['-v', f'{faw_dir}/common/pdf-observatory:/home/pdf-observatory'])
        extra_flags.extend(['-v', f'{os.path.abspath(CONFIG_FOLDER)}:/home/dist'])
        # Allow profiling via e.g. py-spy
        extra_flags.extend(['--cap-add', 'sys_ptrace'])

        extra_flags.append(IMAGE_TAG + '-dev')

    docker_id = f'gfaw-{IMAGE_TAG}-{db_name}'
    if development:
        # Ensure that the necessary npm modules are installed to run the UI
        # locally. Notably, we do this from docker s.t. the node version used
        # to install packages is the same one used to run the FAW.
        #subprocess.check_call(['npm', 'install'],
        #        cwd=os.path.join(faw_dir, 'common', 'pdf-observatory', 'ui'))
        subprocess.check_call(['docker', 'run', '-it', '--rm', '--entrypoint',
                '/bin/bash']
                + extra_flags
                + [
                    '-c', 'cd /home/pdf-observatory/ui && npm install'
                ])

        # Distribution folder is mounted in docker container, but workbench.py
        # holds the schema.
        def watch_for_config_changes():
            # Where, in the docker container, to dump the new config
            cfg_dst = '/home/config.json'

            last_ts = None
            while True:
                try:
                    new_ts = {}
                    for configdir in [CONFIG_FOLDER] + [
                            os.path.join(CONFIG_FOLDER, p)
                            for p in os.listdir(CONFIG_FOLDER)
                            if os.path.isdir(os.path.join(CONFIG_FOLDER, p))]:
                        cfg_path = os.path.join(configdir, 'config.json5')
                        if not os.path.lexists(cfg_path):
                            continue

                        ts = os.path.getmtime(cfg_path)
                        new_ts[cfg_path] = ts

                    if last_ts is None:
                        # Initialization
                        last_ts = new_ts
                    elif last_ts != new_ts:
                        # Some config changed
                        last_ts = new_ts

                        print('workbench: Updating /home/config.json')
                        new_config = _export_config_json(_check_config_file(
                                None)).encode('utf-8')
                        # Docker cp is weird... if stdin, it's a tarred
                        # directory
                        buf = io.BytesIO()
                        with tarfile.TarFile(fileobj=buf, mode='w') as tf:
                            finfo = tarfile.TarInfo(os.path.basename(cfg_dst))
                            finfo.size = len(new_config)
                            finfo.mtime = time.time()
                            tf.addfile(finfo, io.BytesIO(new_config))
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

    def open_browser():
        time.sleep(1.5)
        try:
            webbrowser.open(f'http://localhost:{port}')
        except:
            traceback.print_exc()
    open_browser_thread = threading.Thread(target=open_browser)
    open_browser_thread.daemon = True
    open_browser_thread.start()
    subprocess.check_call(['docker', 'run', '-it', '--rm',
            '--log-driver', 'none',
            '--name', docker_id,
            '-v', f'{pdf_dir}:/home/pdf-files',
            '-v', f'{IMAGE_TAG+VOLUME_SUFFIX}:/data/db',
            '-e', f'DB={db_name}',
            '-p', f'{port}:8123',
            ] + extra_flags)


def _check_config_file(config):
    """For non-deployment editions, we must load config from a json5 file.

    This also gets run for deployments, as this function is responsible for
    merging multiple configs into the single root config.

    This function also contains all schema validation of json5 files, as
    the version stored in the observatory image is separate.
    """
    global CONFIG_FOLDER, IMAGE_TAG

    # Not a deployment -- requires CONFIG_FOLDER
    if CONFIG_FOLDER is None:
        assert config is not None
        CONFIG_FOLDER = config
    else:
        assert config is None

    import pyjson5, schema as s
    config_data = pyjson5.load(open(os.path.join(CONFIG_FOLDER,
            'config.json5')))

    # Before applying schema, merge in child configs
    for child_name in os.listdir(CONFIG_FOLDER):
        child_path = os.path.join(CONFIG_FOLDER, child_name)
        if not os.path.isdir(child_path):
            continue
        child_config_path = os.path.join(child_path, 'config.json5')
        if not os.path.lexists(child_config_path):
            continue

        child_config = pyjson5.load(open(child_config_path))

        # First traversal -- patch keys and values
        nodes = [([], child_config)]
        while nodes:
            path, src = nodes.pop()
            for k, v in src.items():
                ## Rewrite v
                # Check executables; amend cwd
                if path and (
                        'file_detail_views' == path[-1]
                        or 'decision_views' == path[-1]
                        or 'parsers' == path[-1]
                        or 'tasks' == path[-1]):
                    if 'cwd' in v:
                        v['cwd'] = f'{child_name}/' + v['cwd']
                    else:
                        v['cwd'] = child_name

                # Docker build stage patch
                if path == ['build', 'stages']:
                    if 'commands' in v:
                        v['commands'] = [
                                vv
                                    .replace('{disttarg}', f'{{disttarg}}/{child_name}')
                                    .replace('{dist}', f'{{dist}}/{child_name}')
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
                        k = f'{child_name}_{k}'

                # New entries only for these
                if path in [
                        ['pipelines'],
                        ['file_detail_views'],
                        ['decision_views'],
                        ['parsers'],
                        ]:
                    # Amend these with the child's name, to allow for copying
                    k = f'{child_name.replace("_", "-")}-{k}'
                    assert k not in dst, f'Cannot extend {path} {k}; must be new'

                ## Check for merge type
                # Check if new -- if so, assign and be done
                if k not in dst:
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
                else:
                    raise ValueError(f'May not extend {path} {k}: base config type {dst[k]}')

    # Pull in parser-specific schema
    import importlib.util
    spec = importlib.util.spec_from_file_location('etl_parse',
            os.path.join(faw_dir, 'common', 'pdf-etl-parse', 'parse_schema.py'))
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
                    s.Optional('cwd', default='.'): str,
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
                    s.Optional('cwd', default='.'): str,
                    s.Optional('outputMimeType', default='text/html'): str,
                },
            }),
    }

    # NOTE -- primary schema validation is here, but NOT for submodules such
    # as pdf-etl-parse.
    sch = s.Schema({
        'name': s.And(str, s.Regex(r'^[a-zA-Z0-9-]+$')),
        # parsers validated by pdf-etl-parse
        'parsers': etl_parse.schema_get(),
        s.Optional('parserDefaultTimeout', default=30): s.Or(float, int),
        'decision_default': str,
        s.Optional('pipelines', default={}): s.Or({}, {
            s.And(str, lambda x: '_' not in x and '.' not in x,
                    error='Must not have underscore or dot'): {
                s.Optional('label'): str,
                s.Optional('disabled', default=False): s.Or(True, False),
                s.Optional('tasks', default={}): s.Or({},
                    s.And(
                        {
                            s.And(str, lambda x: '_' not in x and '.' not in x,
                                    error='Must not have underscore or dot'): {
                                s.Optional('disabled', default=False): s.Or(True, False),
                                'version': str,
                                'exec': [str],
                                s.Optional('cwd', default='.'): str,
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


def _check_image(development, config_data, build_dir, build_faw_dir):
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
            'common', 'pdf-observatory'))
    if not build_local:
        # For e.g. version updates, load the image first.
        image_file = os.path.join(faw_dir, IMAGE_TAG + '.image')
        if os.path.lexists(image_file):
            print('Loading docker image...')
            subprocess.check_call(['docker', 'load', '-i', image_file])
            os.unlink(image_file)

        o = subprocess.check_output(['docker', 'image', 'ls',
                IMAGE_TAG, '--format', '{{.ID}}'])
        if not o.strip():
            raise Exception("Image not correctly installed, and not "
                    "locally present.  Try re-extracting and running "
                    "again?")
        return

    # Need to build the image
    suffix = '-dev' if development else ''
    assert config_data is not None, 'required --config?'

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
            'from': 'mongo:latest',
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
                '/etc/apt/trusted.gpg.d/mongodb.gpg': True,
                '/etc/mongod.conf.orig': True,
            },
        },

        'obs__pdf-etl-tools': {
            'from': 'ubuntu:18.04',
            'copy_output': {
                '/root/.local/bin/pdf-etl-tool': '/usr/local/bin/pdf-etl-tool',
            },
            'commands': [
                # Setup environment
                'RUN apt-get update && apt-get install -y wget && wget -qO- https://get.haskellstack.org/ | sh',
                # First, tell stack to download the compiler
                'RUN stack setup 8.6.5',
                # NOTE:
                #   8.6.5 is the ghc version number that is implicit in the the "resolver: "
                #   line in pdf-etl-tools/stack.yaml
                # ANALYSIS:
                #   - we could remove coupling at a big efficiency hit by removing this RUN command
                #     - not technically 'coupling' as this only affects efficiency
                #   - if the resolver requires a different version of ghc, nothing is broken, but
                #     we've downloaded 8.6.5 for nought.
                #   - no known solution to get the behavior we want from stack.
                #     - if we try to get stack to determine ghc version from stack.yaml, we have
                #       now lost our efficiency, because the ghc install would now be dependent
                #       upon the haskell package versions in stack.yaml
                #     - FIXME: discover a way to get cake and eat it too.

                # Next, download and build all packages (takes a long time)
                f'COPY {build_faw_dir}/common/stack-staging /home/stack-staging',
                f'COPY {build_faw_dir}/common/pdf-etl-tools/stack.yaml /home/stack-staging/stack.yaml',
                'WORKDIR /home/stack-staging',
                'RUN stack build',

                # Now, build our pdf-etl-tool
                f'COPY {build_faw_dir}/common/pdf-etl-tools /home/pdf-etl-tools',
                'RUN cd /home/pdf-etl-tools && stack --allow-different-user install pdf-etl-tools:pdf-etl-tool',
            ],
        },
    }

    # Important! User changes should trigger as little rebuild as possible.
    # One way to accomplish this is by affected dockerfile_middle and dockerfile_final
    # with system code as early as possible.
    if development:
        # Development extensions... add not-compiled code directories.
        dockerfile_final.append(r'''
            # Install npm globally, so it's available for debug mode
            RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && apt-get install -y nodejs
            ''')
    else:
        # Production extensions...
        dockerfile_middle.append(rf'''
            FROM base AS ui-builder
            # Install npm locally, only for the build.
            RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - && apt-get install -y nodejs

            COPY {build_faw_dir}/common/pdf-observatory/ui /home/pdf-observatory/ui

            RUN cd /home/pdf-observatory/ui \
                && npm install \
                && npm run build
            ''')

    # Always install observatory component dependencies as first part of final
    # stage (to minimize rebuilds on user code changes)
    dockerfile_final.append(rf'''
            COPY {build_faw_dir}/common/pdf-etl-parse/requirements.txt /home/pdf-etl-parse/requirements.txt
            RUN pip3 install -r /home/pdf-etl-parse/requirements.txt

            COPY {build_faw_dir}/common/pdf-observatory/requirements.txt /home/pdf-observatory/requirements.txt
            RUN pip3 install -r /home/pdf-observatory/requirements.txt
    ''')

    # The below commands should be quick to run, as they will happen any time
    # the user changes their code.
    config_rel_dir = os.path.relpath(CONFIG_FOLDER, build_dir)
    if development:
        dockerfile_final_postamble.append(rf'''
            # Add not-compiled code directories
            COPY {build_faw_dir}/common/pdf-etl-parse /home/pdf-etl-parse
            #COPY {build_faw_dir}/common/pdf-observatory /home/pdf-observatory
            ''')

        # The CONFIG_FOLDER will be mounted as dist at runtime.
    else:
        dockerfile_final_postamble.append(rf'''
            # Add not-compiled code directories; omit "ui" for observatory
            COPY {build_faw_dir}/common/pdf-etl-parse /home/pdf-etl-parse
            COPY {build_faw_dir}/common/pdf-observatory/*.py /home/pdf-observatory/
            COPY {build_faw_dir}/common/pdf-observatory/mongo_queue_helper /home/pdf-observatory/mongo_queue_helper
            COPY --from=ui-builder /home/pdf-observatory/ui/dist /home/pdf-observatory/ui/dist

            # The final stage must always have the distribution folder available as
            # /home/dist; do this after copying the base material and building the
            # base UI, so user changes trigger those rebuilds infrequently.
            COPY {shlex.quote(config_rel_dir)} /home/dist

            ENV OBS_PRODUCTION "--production"
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
                    'RUN apt-get update && apt-get install -y curl python3 python3-pip wget',
                    ] + stage_commands
        dockerfile.extend(stage_commands)
        for k, v in stage_def.get('copy_output', {}).items():
            if v is True:
                v = k
            dockerfile_final_postamble.append(f'COPY --from={stage} {k} {v}')

    # Regardless, there's some glue code to create the final image.
    dockerfile_middle.append(rf'''

            FROM base

            ## s6 overlay for running mongod and observatory side by side
            #ADD https://github.com/just-containers/s6-overlay/releases/download/v1.21.8.0/s6-overlay-amd64.tar.gz /tmp/
            COPY {build_faw_dir}/common/s6-overlay-amd64.tar.gz /tmp/

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
            ENV PYTHONPATH /home/dist:/home/pdf-observatory
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

            # Observatory service (modifications must also change common/teaming/pyinfra/deploy.py)
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
                    && echo -e '#! /bin/bash\ncd /home/dist\ndask-worker --local-directory /tmp localhost:8786 2>&1' >> /etc/services.d/dask-worker/run \
                    && chmod a+x /etc/services.d/dask-worker/run \
                && mkdir /etc/services.d/dask-worker/log \
                    && echo -e '#! /usr/bin/execlineb -P\nlogutil-service /var/log/dask-worker' > /etc/services.d/dask-worker/log/run \
                    && chmod a+x /etc/services.d/dask-worker/log/run \
                && echo OK

            # Add 'timeout' script to /usr/bin, for collecting memory + CPU time
            # information.
            COPY {build_faw_dir}/common/timeout-master /home/timeout
            RUN chmod a+x /home/timeout/timeout \
                && ln -s /home/timeout/timeout /usr/bin/timeout_pshved

            # Container runtime properties
            ENV LC_ALL C.UTF-8
            ENV LANG C.UTF-8
            ENV DB observatory-default-data
            ENV OBS_PRODUCTION ""
            ENTRYPOINT ["/init"]
            ''')

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
    print('='*79)
    print(dockerfile)
    print('='*79)

    r = subprocess.run(['docker', 'build', '-t', IMAGE_TAG + suffix,
        '-f', '-', '.'], cwd=build_dir, input=dockerfile.encode())
    if r.returncode != 0:
        raise Exception("Docker build failed; see above")


def _export_config_json(config_data):
    import json
    config_data_client = config_data.copy()
    config_data_client.pop('build', None)
    config_json = json.dumps(config_data_client)
    return config_json


def _mongo_copy(db_name, copy_mongo_from):
    # Only spin up mongo
    dummy_mongo_port = 27015

    import asyncio
    import bson
    import motor.motor_asyncio

    async def run_and_dump():
        p = await asyncio.create_subprocess_exec(
                'docker', 'run', '-it', '--rm',
                '-v', f'{IMAGE_TAG+VOLUME_SUFFIX}:/data/db',
                '-p', f'{dummy_mongo_port}:27017',
                '--entrypoint', 'mongod',
                IMAGE_TAG + '-dev',
                '--bind_ip_all',
        )
        p_exit = p.wait()

        # Wait for docker to spin up
        async def test_connection():
            try:
                await asyncio.wait_for(asyncio.shield(p_exit), timeout=0.1)
                raise ValueError('docker exited early')
            except asyncio.TimeoutError:
                # OK, docker hasn't exited
                pass

            client = motor.motor_asyncio.AsyncIOMotorClient(
                    'mongodb://127.0.0.1:27015')
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

        try:
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
                print(f'Copying {col}...')
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
                print(f'...copied {copied[0]}')
            print('All done - OK')
        finally:
            # Kill docker
            p.kill()
    asyncio.run(run_and_dump())


if __name__ == '__main__':
    main()

