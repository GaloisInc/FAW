#! /usr/bin/env python3

"""
After a build, one might want to run a multi-server distribution of the FAW.
This script assists with setting up a `pyinfra` deployment with the appropriate
configuration for the FAW.

This process requires that each machine(s) being
targeted has both passwordless `sudo` privileges and a passwordless SSH key on each
machine. A deployment may be bootstrapped as follows:

    python workbench-teaming-init.py [--port 8123] [--port-mongo 9123] build/pdf-dist-2021-02-10 team/pdf-dist-2021-02-10 web-host /path/to/files/on/web-host [worker1 worker2 ...]
    cd team/pdf-2021-02-10
    pyinfra inventory.py deploy.py

This will automatically set up docker on the remote machines, copy over the
built image, and set up a system service to run the FAW in the appropriate
configuration for each host, all under the same name as the shallow
directory `pdf-2021-02-10`.

The deployment is designed to be deployed to VMs which may be wiped when they
are no longer needed. To uninstall, simply wipe the whole machine. This was
chosen as a cleaner solution which has no chance of missing errant docker
volumes.

To update a deployment's FAW image, build, then copy e.g.
`build/pdf-dist-2021-02-10/*.image` over the image in `team/pdf-dist-2021-02-10`
and run `pyinfra inventory.py deploy.py`. No need to go through this script
again.

Arguments:
    BUILD_DIR: A directory containing a FAW build (see `workbench.py`).
    DEPLOY_DIR: A directory into which the new teaming deployment will be saved.
    WEB_HOST: A hostname for the server running the core FAW instance, including
        both the web server and the mongodb instance.
    WEB_HOST_FILE_DIR: A path on `WEB_HOST` containing the files to be
        investigated.
    WORKERS: An optional list of additional hostnames for worker nodes. This
        list may be subsequently expanded by modifying `inventory.py` in the
        deployment.

"""

import argparse
import os
import shutil

def dir_new(p):
    if os.path.lexists(p):
        raise ValueError(f"Target must not exist: {p}")
    return p


def dir_existing(p):
    if not os.path.isdir(p):
        raise ValueError(f"Build must already exist: {p}")
    return p


def main():
    ap = argparse.ArgumentParser(description=__doc__,
            formatter_class=argparse.RawTextHelpFormatter)
    ap.add_argument('build_dir', type=dir_existing)
    ap.add_argument('deploy_dir', type=dir_new)
    ap.add_argument('web_host', type=str)
    ap.add_argument('web_host_file_dir', type=str)
    ap.add_argument('workers', nargs='*')
    ap.add_argument('--port', default=8123, type=int,
            help="Port on which the FAW web interface will be accessed.")
    ap.add_argument('--port-mongo', default=9123, type=int,
            help="Port on which the FAW's mongodb will be accessed (UNSECURED!).")
    args = ap.parse_args()

    os.makedirs(args.deploy_dir)
    os.makedirs(os.path.join(args.deploy_dir, 'remote'))

    image_found = None
    for f in os.listdir(args.build_dir):
        if f.endswith('.image'):
            assert image_found is None
            image_found = os.path.join(args.build_dir, f)
            shutil.copy2(image_found, os.path.join(args.deploy_dir, 'remote', f))
    deploy_name = os.path.basename(image_found).rsplit('.', 1)[0]

    with open(os.path.join(args.deploy_dir, 'config.py'), 'w') as f:
        f.write(rf'''
deploy_name = {repr(deploy_name)}
web_host_file_dir = {repr(args.web_host_file_dir)}
port = {repr(args.port)}
port_mongo = {repr(args.port_mongo)}
''')

    with open(os.path.join(args.deploy_dir, 'inventory.py'), 'w') as f:
        f.write(rf'''
web_host = [{repr(args.web_host)}]
workers = {repr(args.workers)}
''')

    pyinfra_template_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)),
            'common', 'teaming', 'pyinfra')
    for fname in os.listdir(pyinfra_template_dir):
        if fname.startswith('.'):
            continue
        shutil.copy2(os.path.join(pyinfra_template_dir, fname), os.path.join(
                args.deploy_dir, fname))

    print('Pyinfra setup created.')


if __name__ == '__main__':
    main()

