#! /usr/bin/env python3
"""Parse a single request stream.

Note: This file just submits the requests to the nanny process,
which forwards them to the server. All of the server-specific logic
is contained in ``nanny.py``.
"""
import argparse
import json
import socket
import subprocess
import sys
import time
from typing import Any, Dict, List, TypedDict


class Response(TypedDict):
    parsed_requests: List[Dict[str, Any]]
    wrapper_messages: List[str]
    log_lines: List[str]


def receive_with_prepended_length(sock: socket.socket) -> bytes:
    response_bytes = sock.recv(4096)
    # First line of response is its total size
    expected_size_str, _, response_bytes = response_bytes.partition(b'\n')
    expected_size = int(expected_size_str)
    response_chunks = [response_bytes]
    bytes_read = len(response_bytes)
    while bytes_read < expected_size:
        response_bytes = sock.recv(4096)
        bytes_read += len(response_bytes)
        response_chunks.append(response_bytes)
    return b''.join(response_chunks)


def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument(
        '--request-stream', required=True, help='File containing HTTP requests'
    )
    argument_parser.add_argument(
        '--json-output', required=False, default=None,
        help='File in which to write parsed requests as JSON'
    )
    argument_parser.add_argument(
        '--load-balancer-port', required=True, type=int,
        help='Port where load balancer around apache server is running'
    )
    argument_parser.add_argument(
        '--server-port-range-start', required=True, type=int,
        help=(
            'First port where server runs; further instances run on '
            'subsequent ports'
        )
    )
    argument_parser.add_argument(
        '--server-instances', required=False, type=int, default=None,
        help=(
            'Number of server instances to run (default: use nanny CLI default)'
        )
    )
    args = argument_parser.parse_args()

    nanny_started = False
    while True:
        try:
            sock = socket.create_connection(('localhost', args.load_balancer_port), timeout=10)
        except OSError:
            # nanny not running
            if not nanny_started:
                p = subprocess.Popen(
                    [
                        './nanny.py', '--listen-port', str(args.load_balancer_port),
                        '--server-port-range-start', str(args.server_port_range_start),
                        '--stop-running-servers',
                        '--pid-file', '/var/run/apache-nanny/nanny.pid',
                        *(
                            ['--max-instances', str(args.server_instances)]
                            if args.server_instances is not None else []
                        ),
                    ],
                    start_new_session=True,
                    stdin=subprocess.DEVNULL,
                    stdout=subprocess.DEVNULL,
                    stderr=subprocess.DEVNULL,
                )
                print(f'Running nanny process at pid {p.pid}')
                time.sleep(1)
                nanny_started = True
            else:
                print(f'Could not launch or connect to nanny process at port {args.load_balancer_port}')
                exit(1)
        else:
            break
    request_stream = open(args.request_stream, 'rb').read()
    try:
        # First line of message is total size
        sock.sendall(str(len(request_stream)).encode() + b'\n')
        sock.sendall(request_stream)
        response_data = receive_with_prepended_length(sock)
    except socket.timeout:
        print('Request to nanny process timed out')
        exit(1)
    except OSError as e:
        print(f'Request to nanny process failed: {e}')
        exit(1)

    response: Response = json.loads(response_data)
    for message in response['wrapper_messages']:
        # Messages logged from nanny script
        print(message)
    for log_line in response['log_lines']:
        # Messages logged from server
        print(f'Server logging: {log_line}', file=sys.stderr)
    for parsed_request in response['parsed_requests']:
        if parsed_request.get('error', False):
            print('Error parsing a request.')
        elif parsed_request.get('bodyError', False):
            print('Error parsing a request body.')

    if args.json_output is not None:
        with open(args.json_output, 'w') as f:
            json.dump(response['parsed_requests'], f, indent=2)


if __name__ == '__main__':
    main()
