#! /usr/bin/env python3

"""Helper script to print out logs.
"""

import os
import json
import signal
import subprocess


log_types = [
    'View /var/log/observatory',
    'View /var/log/dask-scheduler',
    'View /var/log/dask-worker'
] + ( ['View /var/log/ci-container'] if os.path.exists('/var/log/ci-container') else [] ) + [
    'Database REPL',
    'Bash shell',
    'Show Dev Mounts',
    'Restart FAW service',
]


def main():
    while True:        
        print('')  # Clear ctrl+C
        for i, loc in enumerate(log_types):
            print(f'{i+1}) {loc}')
        choice = input('Which action? (ctrl+c to exit / change log) ')
        try:
            li = int(choice) - 1
        except ValueError:
            continue

        if li < 0 or li >= len(log_types):
            continue

        if log_types[li].startswith('View'):
            try:
                subprocess.call(['s6-logwatch', log_types[li][5:]])
            except KeyboardInterrupt:
                pass
        elif log_types[li].startswith('Database'):
            _wrapped_call(['faw-db-console.py'])
        elif log_types[li].startswith('Bash shell'):
            _wrapped_call(['/usr/bin/bash'])
        elif log_types[li].startswith('Show Dev Mounts'):
            _show_devmounts()
        elif log_types[li].startswith('Restart'):
            subprocess.call(['faw-restart.sh'])
        else:
            raise NotImplementedError(log_types[li])


def _wrapped_call(cmd):
    # Must ignore CTRL+C since used by REPL
    old = signal.signal(signal.SIGINT, lambda signum, frame: None)
    try:
        subprocess.call(cmd)
    finally:
        signal.signal(signal.SIGINT, old)


def _show_devmounts():
    # we have tunnelled all valid environment variables
    # to this container, so we can get hold of the paths that way.
    devmount_env = {
        env: os.getenv(env)
        for env in os.environ.keys()
        if env.startswith('FAW_DEVMOUNTS-')
    }

    print('{0:<20}{1:<10}{2}'.format('DevMount', 'Status', 'Host Path'))
    print('{0:<20}{1:<10}{2}'.format('--------', '------', '---------'))
    for data in devmount_env.values():
        value = json.loads(data)
        active = True if value['host_path'] else False
        message = '{0:<20}{1:<10}{2}'.format(
            value['name'], "Active" if active else "Inactive",
            value['host_path'] or ''
        )
        print(message)


if __name__ == '__main__':
    main()
