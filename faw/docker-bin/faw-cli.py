#! /usr/bin/env python3

"""Helper script to print out logs.
"""

import os
import signal
import subprocess

def main():
    while True:
        log_types = [
                'View /var/log/observatory',
                'View /var/log/dask-scheduler',
                'View /var/log/dask-worker',
                'Database REPL',
                'Restart FAW service',
        ]
        print('')  # Clear ctrl+C
        for i, loc in enumerate(log_types):
            print(f'{i+1}) {loc}')
        choice = input('Which action? (ctrl+c to exit / change log) ')
        try:
            li = int(choice) - 1
        except ValueError:
            continue

        if log_types[li].startswith('View'):
            try:
                subprocess.call(['s6-logwatch', log_types[li][5:]])
            except KeyboardInterrupt:
                pass
        elif log_types[li].startswith('Database'):
            # Must ignore CTRL+C since used by REPL
            old = signal.signal(signal.SIGINT, lambda signum, frame: None)
            try:
                subprocess.call(['faw-db-console.py'])
            finally:
                signal.signal(signal.SIGINT, old)
        elif log_types[li].startswith('Restart'):
            subprocess.call(['faw-restart.sh'])
        else:
            raise NotImplementedError(log_types[li])


if __name__ == '__main__':
    main()
