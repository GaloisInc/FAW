
import config
import json
import os
import shlex

from pyinfra import host, inventory
from pyinfra.operations import apt, files, server, yum
from pyinfra_docker import deploy_docker

SUDO = True
user = config.deploy_name
userhome = f'/home/{user}'
webhost = inventory.get_group('web_host')[0].name

use_apt = host.fact.deb_packages
pkg_man = apt if use_apt else yum

if use_apt:
    apt.update(cache_time=24*3600)

# This one doesn't allow containers to chat with external IPv6 servers
# UNLESS IPv6 NAT is enabled via the image robbertkl/ipv6nat
docker_ipv6_subnet = 'fd00:0:0:0:1::/80'
# This one does allow externa chatter, but is not supported by the NAT.
#docker_ipv6_subnet = '2001:db8:1::/64'

# Install docker
pkg_man.packages(packages=['curl', 'gnupg2'])
deploy_docker(config={
    'ipv6': True,
    'fixed-cidr-v6': docker_ipv6_subnet,
})
server.service('docker', running=True)
server.group(group='docker')

# Allow docker to work with IPv6 NAT enabled
# See https://github.com/robbertkl/docker-ipv6nat#usage
server.shell(name='Set up IPv6 NAT for docker', commands=[
        'if docker inspect ipv6nat ; then echo "ipv6nat already set up" ; else docker run -d --restart unless-stopped --name ipv6nat --cap-drop ALL --cap-add NET_ADMIN --cap-add NET_RAW --cap-add SYS_MODULE --network host -v /var/run/docker.sock:/var/run/docker.sock:ro -v /lib/modules:/lib/modules:ro robbertkl/ipv6nat ; fi',
])

# Disable Ubuntu's weird firewall
if use_apt:
    server.shell(name='Disable UFW firewall', commands=['ufw disable'])

# Set up user based on config
server.user(user=user, home=userhome, groups=['docker'])
# NOTE -- use rsync to allow for resume from partial transfers, and because it
# won't re-send bytes on a file which hasn't changed.
files.rsync(src='./remote', dest=userhome,
        flags=['-xaP', '--delete', f'--chown={user}:{user}'])

docker_container = f'{user}-faw'
docker_image_file = None
for f in os.listdir('./remote'):
    if f.endswith('.image'):
        assert docker_image_file is None, 'Do not put 2 images in remote folder'
        docker_image_file = f
assert docker_image_file is not None, 'No docker image in remote folder?'
docker_image_name = docker_image_file.rsplit('.')[0]
docker_flags = ['docker', 'run', '-d',
        '--restart', 'unless-stopped',
        '--log-driver', 'none',
        '--name', docker_container,
        '-e', f'DB={user}-faw-db',
]
docker_flags_cmd = []
if 'web_host' in host.groups:
    # Normal FAW instance, all ports exposed
    docker_flags.extend(['-v', f'{config.web_host_file_dir}:/home/pdf-files'])
    docker_flags.extend(['-v', f'{user}-data:/data/db'])
    docker_flags.extend(['-p', f'{config.port}:8123'])
    docker_flags.extend(['-p', f'{config.port_mongo}:27017'])
    docker_flags.extend(['-p', f'{config.port_dask}:8786'])
    docker_flags.extend(['-p', f'{config.port_dask_dashboard}:8787'])

    # Furthermore, extend our observatory run script to be self-hostname-aware
    host_script = rf'''
#! /bin/bash
set -e
echo -e '#! /bin/bash\ncd /home/pdf-observatory\npython3 main.py /home/pdf-files "{webhost}:{config.port_mongo}/${{DB}}" --in-docker --port 8123 ${{OBS_PRODUCTION}} --config ../config.json --hostname {webhost} 2>&1' > /etc/services.d/observatory/run
chmod a+x /etc/services.d/observatory/run
echo -e '#! /bin/bash\ncd /home/dist\n/home/pdf-observatory/dask-worker-runner --contact-host "{host.name}" --contact-first-port {config.port_dask_worker} localhost:8786 2>&1' > /etc/services.d/dask-worker/run
chmod a+x /etc/services.d/dask-worker/run
# Now, launch as normal
/init
'''
    server.shell(name="Writing host script", commands=[
            rf'echo {shlex.quote(host_script)} > {userhome}/host.sh',
            rf'chmod 755 {userhome}/host.sh',
    ])
    docker_flags.extend([
            '-v', f'{userhome}/host.sh:/home/host.sh:ro',
            '--entrypoint', '/bin/bash',
    ])
    docker_flags_cmd.extend(['-c', '/home/host.sh'])
else:
    # Worker FAW instance
    worker_script = rf'''
#! /bin/bash
set -e
# Fix up observatory (now removed from teaming deployments; main server only)
rm -rf /etc/services.d/observatory
# Fix up dask -- disable scheduler, change worker to connect to global scheduler
rm -rf /etc/services.d/dask-scheduler
echo -e '#! /bin/bash\ncd /home/dist\n/home/pdf-observatory/dask-worker-runner --contact-host "{host.name}" --contact-first-port {config.port_dask_worker} {webhost}:{config.port_dask} 2>&1' > /etc/services.d/dask-worker/run
chmod a+x /etc/services.d/dask-worker/run
# Disable mongo
rm -rf /etc/services.d/mongod
# Now, run as usual
/init
'''
    server.shell(name="Writing worker script", commands=[
            rf'echo {shlex.quote(worker_script)} > {userhome}/worker.sh',
            rf'chmod 755 {userhome}/worker.sh',
    ])
    docker_flags.extend([
            '-v', f'{userhome}/worker.sh:/home/worker.sh:ro',
            '--entrypoint', '/bin/bash',
    ])
    docker_flags_cmd.extend(['-c', '/home/worker.sh'])
# Forward both worker ports and dashboard
docker_flags.extend(['-p', f'{config.port_dask_worker}-{config.port_dask_worker+100}:8788-8888'])
docker_flags.append(docker_image_name)
docker_flags.extend(docker_flags_cmd)
server.shell(name="Kill old docker container, launch new",
        commands=[
            # Load new image, if needed (may take awhile; do before removing old)
            rf'''bash -c '\
                    if [[ "{userhome}/docker-last-update" -ot "{userhome}/remote/{docker_image_file}" ]]; then \
                        docker load -i "{userhome}/remote/{docker_image_file}" \
                        && touch -r "{userhome}/remote/{docker_image_file}" "{userhome}/docker-last-update"; \
                    else echo up to date; \
                    fi \
                    ' ''',
            # Stop / remove old
            rf'''bash -c '\
                    (docker stop {docker_container} || echo missing) \
                    && (docker rm {docker_container} || echo missing) \
                    ' ''',
            # Run new docker container as a service
            docker_flags[0] + ' ' + ' '.join([shlex.quote(q) for q in docker_flags[1:]]),
        ])

