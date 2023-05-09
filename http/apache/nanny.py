#! /usr/bin/env python3
"""Load balancer around apache."""
import argparse
import asyncio
from contextlib import closing
import dataclasses
import functools
import http.client
import itertools
import json
import logging
import os
import pathlib
import socket
import shutil
import subprocess
from typing import Collection, Generic, List, TypeVar, AsyncContextManager

import psutil

T = TypeVar('T')

logger = logging.getLogger()

APACHE_RUN_DIR = '/var/run/apache2'
APACHE_LOG_DIR = '/var/log/apache2'
APACHE_LOCK_DIR = '/var/lock/apache2'


@dataclasses.dataclass
class ApacheInstance:
    port: int
    log_dir: pathlib.Path
    pid_file_path: pathlib.Path


class _ManagedResource(AsyncContextManager[T]):
    _queue: "asyncio.Queue[T]"
    _resource: T

    def __init__(self, queue: "asyncio.Queue[T]"):
        self._queue = queue

    async def __aenter__(self) -> T:
        self._resource = await self._queue.get()
        return self._resource

    async def __aexit__(self, *args, **kwargs):
        self._queue.put_nowait(self._resource)


class ResourcePool(Generic[T]):
    """Asynchronous resource pool."""

    _queue: "asyncio.Queue[T]"

    def __init__(self, resources: Collection[T]):
        self._queue = asyncio.Queue(len(resources))
        for resource in resources:
            self._queue.put_nowait(resource)

    def borrow(self) -> _ManagedResource[T]:
        """Return an async context manager allowing access to a resource."""
        return _ManagedResource(self._queue)


class NonClosingResponse(http.client.HTTPResponse):
    def _close_conn(self):
        # Normally this would close the file obj, but since we're using
        # pipelining, we need to read multiple responses from the same
        # connection.
        pass


class MockSocket:
    """Mocket.

    HTTPResponse only uses the ``makefile`` method of sockets.
    """
    def __init__(self, fp):
        self.fp = fp
    def makefile(self, *args, **kwargs):
        return self.fp


async def read_with_prepended_length(reader: asyncio.StreamReader) -> bytes:
    """Raises IncompleteReadError if not all data is found."""
    expected_size_str = await reader.readuntil(b'\n')
    expected_size = int(expected_size_str)
    return await reader.readexactly(expected_size)


async def handle_request_stream(
    reader: asyncio.StreamReader,
    writer: asyncio.StreamWriter,
    *,
    apache_instance_pool: ResourcePool[ApacheInstance],
) -> None:
    logger.debug(f'Request {id(reader):x}: Received')
    loop = asyncio.get_running_loop()
    async with apache_instance_pool.borrow() as apache_instance:
        logger.debug(f'Request {id(reader):x}: Claimed server instance on port {apache_instance.port}')
        ensure_instance_running(apache_instance)
        results_log_path = apache_instance.log_dir / 'results.log'
        error_log_path = apache_instance.log_dir / 'error.log'
        access_log_path = apache_instance.log_dir / 'access.log'
        # empty logs before sending request stream
        for log_path in [results_log_path, error_log_path, access_log_path]:
            open(log_path, 'w').close()

        # read the incoming data and forward to apache
        data = await read_with_prepended_length(reader)
        sock = socket.create_connection(('127.0.0.1', apache_instance.port), timeout=5)
        await loop.sock_sendall(sock, data)

        # Share the same file object between all responses (required for pipelining)
        responses_file = sock.makefile('rb')
        mock_sock = MockSocket(responses_file)

        responses: List[NonClosingResponse] = []
        wrapper_messages: List[str] = []
        log_lines: List[str] = []
        while True:
            response = NonClosingResponse(mock_sock)
            try:
                response.begin()
            except ConnectionError as e:
                # This probably just means that the server handled all requests,
                # but may be an actually meaningful error
                wrapper_messages.append(f'Connection Error: {e}')
                break
            except socket.timeout:
                wrapper_messages.append('Error: Connection to server timed out')
                logger.debug(f'Request {id(reader):x}: Connection to server timed out')
                break
            if response.will_close:
                break
            responses.append(response)
            response.read()  # Discard any http response body (not meaningful)
        sock.close()
        responses_file.close()

        # Get new log lines from apache
        with open(access_log_path) as access_log, open(error_log_path) as error_log, open(results_log_path) as results_log:
            log_lines = access_log.readlines() + error_log.readlines()
            results = results_log.read()  # concatenated JOSNified requests
        logger.debug(f'Request {id(reader):x}: Returning server instance on port {apache_instance.port}')

    wrapper_messages.append(f'Got {len(responses)} responses')

    # Convert the concatenated JSON objects into an array before loading
    try:
        parsed_requests = json.loads(f'[{results.replace("}{", "},{")}]')
    except json.JSONDecodeError as e:
        wrapper_messages.append(f'Malformed JSON written by server: {e}')
        parsed_requests = []

    for i, (response, parsed_request) in enumerate(itertools.zip_longest(responses, parsed_requests)):
        if response is None:
            wrapper_messages.append(f'Request {i}: Got no response for parsed request')
            continue
        wrapper_messages.append(f'Response: {response.status} {response.reason}')
        if parsed_request is None:
            wrapper_messages.append(
                f'Request {i}: Did not parse request (response status: {response.status})'
            )
            # it's safe to append here since parsed_requests is already exhausted
            parsed_requests.append({'error': True})

    data_to_send_back = json.dumps(
        {
            'parsed_requests': parsed_requests,
            'wrapper_messages': wrapper_messages,
            'log_lines': log_lines,
        },
        ensure_ascii=True,
        indent=2,
    ).encode()
    # First line of response is the size of the rest of the response
    writer.write(str(len(data_to_send_back)).encode() + b'\n')
    writer.write(data_to_send_back)
    logger.debug(f'Request {id(reader):x}: Done')


def start_apache_on_port(port: int) -> ApacheInstance:
    logger.debug(f'Starting apache on port {port}')
    log_dir = pathlib.Path(f'{APACHE_LOG_DIR}-{port}')
    run_dir = pathlib.Path(f'{APACHE_RUN_DIR}-{port}')
    lock_dir = pathlib.Path(f'{APACHE_LOCK_DIR}-{port}')
    for d in [log_dir, run_dir, lock_dir]:
        d.mkdir(exist_ok=True)
    subprocess.run(
        # Use restart instead of start in case it's already running
        ['/usr/sbin/apache2ctl', 'restart'],
        start_new_session=True,
        env={'PORT': str(port)},
        check=True,
    )
    # Ensure apache can write to results log file
    results_log_path = log_dir / 'results.log'
    results_log_path.touch()
    shutil.chown(results_log_path, user='www-data')
    return ApacheInstance(
        port=port,
        log_dir=log_dir,
        pid_file_path=run_dir / 'apache2.pid',
    )


def ensure_instance_running(instance: ApacheInstance) -> None:
    with open(instance.pid_file_path) as pid_file:
        pid = int(pid_file.read())
    if not psutil.pid_exists(pid):
        start_apache_on_port(instance.port)


async def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument(
        '--listen-port', required=True,
        help='Port on which to listen for incoming requests'
    )
    argument_parser.add_argument(
        '--server-port-range-start', required=True, type=int,
        help=(
            'First port where server runs; further instances run on '
            'subsequent ports'
        )
    )
    argument_parser.add_argument(
        '--max-instances', required=False, default=1, type=int,
        help='Number of server instances to run at a time'
    )
    argument_parser.add_argument(
        '--debug', action='store_true',
        help='Enable debug logging'
    )
    argument_parser.add_argument(
        '--stop-running-servers', action='store_true',
        help='Kill all running server instances outside of the port range given'
    )
    argument_parser.add_argument(
        '--pid-file', default=None, help=(
            'Path to file in which to write the PID of this process. '
            'If provided, replace currently running nanny process'
        ),
    )
    args = argument_parser.parse_args()
    logging.basicConfig()
    if args.debug:
        logger.setLevel(logging.DEBUG)

    if args.pid_file is not None:
        if os.path.isfile(args.pid_file):
            with open(args.pid_file, 'r') as pid_file:
                pid = int(pid_file.read().strip())
            if psutil.pid_exists(pid):
                logger.debug(f'Killing running nanny process at PID {pid}')
                os.kill(pid, 9)
        with open(args.pid_file, 'w') as pid_file:
            pid = os.getpid()
            logger.debug(f'Running at PID {pid} (saving to {args.pid_file})')
            pid_file.write(str(pid))

    if args.stop_running_servers:
        # First, kill running servers--only necessary when we've
        # changed the number of instances
        subprocess.run(['killall', 'apache2'])
    instances: List[ApacheInstance] = []
    for server_port in range(
        args.server_port_range_start,
        args.server_port_range_start + args.max_instances
    ):
        apache_instance = start_apache_on_port(server_port)
        instances.append(apache_instance)

    apache_instance_pool = ResourcePool(instances)

    try:
        async with await asyncio.start_server(
            client_connected_cb=functools.partial(
                handle_request_stream,
                apache_instance_pool=apache_instance_pool
            ),
            host='127.0.0.1',
            port=args.listen_port,
        ) as server:
            logger.debug(f'Started server on port {args.listen_port}')
            await server.serve_forever()
    except OSError as e:
        if e.errno == 98:
            # Probably we tried to launch multiple nannies at once. This is fine.
            logger.error(f'Nanny port in use; aborting')
        else:
            raise


if __name__ == '__main__':
    asyncio.run(main())
