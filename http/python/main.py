#! /usr/bin/env python3
import argparse
import base64
import http
import http.server
import io
import json
import sys
from typing import Tuple, TypedDict, List


class Request(TypedDict):
    error: bool
    method: bytes
    path: bytes
    version: bytes
    headers: List[Tuple[bytes, bytes]]
    bodyError: bool
    body: bytes


def _base64_encode_string(s: str) -> bytes:
    # Note: python http.server interprets all binary data as latin-1
    return base64.b64encode(bytes(s, encoding='iso-8859-1')).decode()


class SerializingHandler(http.server.BaseHTTPRequestHandler):

    serialized_requests: List[Request]
    error: bool

    def __init__(self, *args, file: io.IOBase, **kwargs) -> None:
        self.serialized_requests = []
        self.wfile = io.BytesIO()  # Unused, but must be flushable in some error cases
        self.rfile = file
        self.error = False
        super().__init__(request=None, client_address=(None, None), server=None)

    def __serialize_request(self) -> None:
        """Serialize a request."""
        request = Request(
            error=False,
            method=_base64_encode_string(self.command),
            path=_base64_encode_string(self.path),
            version=_base64_encode_string(self.request_version),
            headers=[
                (_base64_encode_string(field_name), _base64_encode_string(value))
                for field_name, value in self.headers.items()
            ],
        )
        chunked_encoding = 'transfer-encoding' in self.headers and self.headers.get_all(
            'transfer-encoding', ['']
        )[-1].rsplit(maxsplit=1)[-1] == 'chunked'
        if (
            'content-length' not in self.headers
            and not chunked_encoding
        ):
            # No body
            request['bodyError'] = False
            self.serialized_requests.append(request)
            return

        try:
            if chunked_encoding:
                chunks = []
                chunk_length = -1
                while chunk_length != 0:
                    chunk_length = int(self.rfile.readline(), 16)
                    if chunk_length != 0:
                        chunks.append(self.rfile.read(chunk_length))
                    self.rfile.readline()
                request['body'] = base64.b64encode(b''.join(chunks)).decode()
                request['bodyError'] = False
            elif 'content-length' in self.headers:
                request['body'] = base64.b64encode(
                    self.rfile.read(int(self.headers['content-length']))
                ).decode()
                request['bodyError'] = False
            else:
                assert False
            if self.command == 'TRACE':
                # TRACE should never have a body. We should read it and either error or continue
                print("TRACE request had body", file=sys.stderr)
        except Exception as e:
            # Either the buffer was exhausted early, or a chunk size or
            # content length was malformed
            print(f'Exception while parsing body: {e}', file=sys.stderr)
            request['bodyError'] = True
        self.serialized_requests.append(request)

    # In later python versions this could be done more cleanly by
    # iterating over http.HTTPMethod
    do_CONNECT = __serialize_request
    do_DELETE = __serialize_request
    do_GET = __serialize_request
    do_HEAD = __serialize_request
    do_OPTIONS = __serialize_request
    do_PATCH = __serialize_request
    do_POST = __serialize_request
    do_PUT = __serialize_request
    do_TRACE = __serialize_request

    def send_error(
        self, code: http.HTTPStatus, message: str = None, explain: str = None
    ) -> None:
        self.serialized_requests.append(Request(error=True))
        self.error = True
        # logging from base `send_error` has too much cruft (e.g. timestamps),
        # and doesn't include the explanation, so we roll our own.
        print(f'Error {code}: {message}', file=sys.stderr)
        if explain is not None:
            for line in explain.split('\n'):
                # Prefix all explanation lines so they can be ignored
                print(f'Explanation: {line}', file=sys.stderr)

    def setup(self) -> None:
        # Override socketserver.StreamRequestHandler.setup
        pass

    def finish(self) -> None:
        # Override socketserver.StreamRequestHandler.finish
        self.rfile.close()


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
        '--protocol-version', required=False,
        default=SerializingHandler.protocol_version,
        help=(
            'The version of the HTTP protocol we support. Defaults to HTTP/1.0, '
            'which is Python\'s default. Set to HTTP/1.1 to enable automatic '
            'keepalive (we probably want this in the FAW)'
        )
    )
    args = argument_parser.parse_args()

    # Read whole file into memory to prevent race conditions if file is
    # modified while we're reading
    request_stream = io.BytesIO(open(args.request_stream, 'rb').read())

    SerializingHandler.protocol_version = args.protocol_version
    # Handlers munge their streams on initialization, and then close them.
    # If automatic keepalive is on, this handles multiple requests.
    request_handler = SerializingHandler(file=request_stream)

    if args.json_output is not None:
        with open(args.json_output, 'w') as f:
            json.dump(request_handler.serialized_requests, f, indent=2)


if __name__ == '__main__':
    main()
