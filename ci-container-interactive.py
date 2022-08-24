#!/usr/bin/env python3

import argparse
import subprocess

from workbench import get_faw_container_name


parser = argparse.ArgumentParser()
parser.add_argument('--image-tag', help="Image tag for the faw container")
parser.add_argument('--config-dir', required=True, help="Config directory for the distribution")
parser.add_argument('--file-dir', required=True, help="Directory containing test files")
parser.add_argument('--wait_time', default=0, type=int, help="Timeout (in seconds) to wait till the CI container is up. 0 (default) will wait for ever")
args = parser.parse_args()


print("=" * 80)
print("Waiting for CI container to be ready")
if not args.wait_time:
    r = subprocess.run(['s6-svwait', '-U', '/var/run/s6/services/ci-container/'])
else:
    r = subprocess.run(['s6-svwait', '-U', '-t', str(args.wait_time * 1000), '/var/run/s6/services/ci-container/'])

if r.returncode != 0:
    print("CI Container has not reached its steady state. Check the current logs at logs/ci-container/current")
else:
    docker_id = get_faw_container_name(args.image_tag, args.config_dir, args.file_dir)
    subprocess.run(['docker', 'exec', '-it', docker_id, 'faw-cli.py'])
