#! /usr/bin/env python3
"""Load balancer around apache."""
import argparse
import asyncio
import dataclasses
import functools
import http.client
import json
import logging
import os
import socket
import subprocess
from typing import Collection, Generic, List, TypeVar, AsyncContextManager

T = TypeVar('T')

logger = logging.getLogger()


@dataclasses.dataclass
class ApacheInstance:
    port: int
    error_log_path: "os.PathLike[str]"


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
        # catch up to current end of log file
        # TODO this might break when/if logs are rotated
        log_file = open(apache_instance.error_log_path, 'r')
        log_file.seek(0, os.SEEK_END)
        # read the incoming data and forward to apache
        data = await read_with_prepended_length(reader)
        sock = socket.create_connection(('127.0.0.1', apache_instance.port), timeout=5)
        await loop.sock_sendall(sock, data)

        # Share the same file object between all responses (required for pipelining)
        responses_file = sock.makefile('rb')
        mock_sock = MockSocket(responses_file)

        responses: List[NonClosingResponse] = []
        response_bodies: List[bytes] = []
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
            response_bodies.append(response.read())
        sock.close()
        responses_file.close()

        # Get new log lines from apache
        log_lines = log_file.readlines()
        logger.debug(f'Request {id(reader):x}: Returning server instance on port {apache_instance.port}')

    wrapper_messages.append(f'Got {len(responses)} responses')

    parsed_requests = []
    for response, response_body in zip(responses, response_bodies):
        wrapper_messages.append(f'Response: {response.status} {response.reason}')
        if 200 <= response.status < 400:
            # OK; response body is parse result
            try:
                response_content = json.loads(response_body)
                parsed_requests.append(response_content)
            except (json.JSONDecodeError, UnicodeDecodeError) as e:
                wrapper_messages.append(f'Error: Malformed JSON for allegedly OK request: {e}')
        else:
            # Errors; we could return them, but probably the status/reason are enough
            # for line in response_body.decode('utf-8', errors='replace').splitlines():
            #     print(line, file=sys.stderr)
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


async def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument(
        '--listen-port', required=True,
        help='Port on which to listen for incoming requests'
    )
    # TODO remove this, and dynamically run multiple apache instances
    argument_parser.add_argument(
        '--server-port', required=True, type=int,
        help='Port where server runs'
    )
    argument_parser.add_argument(
        '--max-instances', required=False, default=1, type=int,
        help='Number of server instances to run at a time'
    )
    argument_parser.add_argument(
        '--debug', action='store_true',
        help='Enable debug logging'
    )
    args = argument_parser.parse_args()
    logging.basicConfig()
    if args.debug:
        logger.setLevel(logging.DEBUG)
    # start up subprocess pool of apache servers
    # only allow one request through at a time
    # For now, just run one apache instance

    # start apache if it's not started yet
    # TODO specify port(s) here, as the env var PORT
    subprocess.run(
        'pidof apache2 > /dev/null || apache2ctl start',
        shell=True,
    )

    apache_instance_pool = ResourcePool([
        ApacheInstance(port=args.server_port, error_log_path='/var/log/apache2/error.log')
    ])

    try:
        async with await asyncio.start_server(
            client_connected_cb=functools.partial(
                handle_request_stream, apache_instance_pool=apache_instance_pool
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
