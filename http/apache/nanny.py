#! /usr/bin/env python3
"""Load balancer around apache."""
import argparse
import asyncio
import concurrent.futures
import contextlib
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
from typing import Any, Generator, List, TypeVar

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


@contextlib.contextmanager
def reenqueueing(resource: T, queue: "asyncio.Queue[T]") -> Generator[T, Any, None]:
    try:
        yield resource
    finally:
        queue.put_nowait(resource)


async def handle_request_stream(
    reader: asyncio.StreamReader,
    writer: asyncio.StreamWriter,
    *,
    apache_instance_queue: "asyncio.Queue[ApacheInstance]",
) -> None:
    logger.debug(f'Request {id(reader):x}: Received')
    loop = asyncio.get_running_loop()
    with reenqueueing(await apache_instance_queue.get(), apache_instance_queue) as apache_instance:
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
        sock = socket.socket()
        sock.settimeout(10)
        await loop.sock_connect(sock, ('127.0.0.1', apache_instance.port))
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
                # Blocking I/O; run in executor to prevent blocking other
                # responses while waiting/reading
                await loop.run_in_executor(None, response.begin)
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
    try:
        with open(instance.pid_file_path) as pid_file:
            pid = int(pid_file.read())
    except FileNotFoundError:
        start_apache_on_port(instance.port)
        return
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
        '--version', required=True,
        help='Version identifier of the nanny process. Used to check if already running'
    )
    argument_parser.add_argument(
        '--debug', action='store_true',
        help='Enable debug logging'
    )
    argument_parser.add_argument(
        '--replace', action='store_true',
        help='Kill and replace nanny. If not passed, just ensure the nanny is running.'
    )
    argument_parser.add_argument(
        '--stop-running-servers', action='store_true',
        help=(
            'Kill all running server instances outside of the port range given'
        )
    )
    argument_parser.add_argument(
        '--run-dir', required=True, help=(
            'Path to dir containing pid and version files'
        ),
    )
    args = argument_parser.parse_args()
    logging.basicConfig()
    if args.debug:
        logger.setLevel(logging.DEBUG)

    version = args.version
    run_dir = pathlib.Path(args.run_dir)
    pid_path = run_dir / 'nanny.pid'
    version_path = run_dir / 'version'
    running_nanny_pid: int
    if pid_path.is_file():
        with open(pid_path, 'r') as pid_file:
            running_nanny_pid = int(pid_file.read().strip())
    else:
        running_nanny_pid = -1
    nanny_running = psutil.pid_exists(running_nanny_pid)
    if nanny_running and not args.replace and version_path.is_file():
        with open(version_path, 'r') as version_file:
            running_nanny_version = version_file.read()
        if running_nanny_version == version:
            logger.info(f'Nanny already running with same version')
            exit(0)
    with open(version_path, 'w') as version_file:
        # Save the version before killing the running nanny to prevent
        # race condition betwen multiples of this script
        version_file.write(version)
    if nanny_running:
        logger.info(f'Killing running nanny process at PID {running_nanny_pid}')
        os.kill(running_nanny_pid, 9)
    with open(pid_path, 'w') as pid_file:
        pid = os.getpid()
        logger.debug(f'Running at PID {pid} (saving to {pid_path})')
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

    apache_instance_queue = asyncio.Queue(len(instances))
    for instance in instances:
        apache_instance_queue.put_nowait(instance)
    # Ensure we have enough threads to handle all active requests)
    asyncio.get_running_loop().set_default_executor(
        concurrent.futures.ThreadPoolExecutor(args.max_instances)
    )

    try:
        async with await asyncio.start_server(
            client_connected_cb=functools.partial(
                handle_request_stream,
                apache_instance_queue=apache_instance_queue
            ),
            host='127.0.0.1',
            port=args.listen_port,
        ) as server:
            logger.info(f'Started server on port {args.listen_port}')
            await server.serve_forever()
    except OSError as e:
        if e.errno == 98:
            # Probably we tried to launch multiple nannies at once. This is fine.
            logger.error(f'Nanny port in use; aborting')
        else:
            raise


if __name__ == '__main__':
    asyncio.run(main())
