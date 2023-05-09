#! /usr/bin/env python3
"""Script that submits request streams directly to apache.
Expects JSONified requests in response. Doesn't support HEAD or
CONNECT methods.

Legacy script; no longer used. Also, no longer works since
``index.php`` was changed.

Note: Doesn't yet use the "nanny" model that we want long-term!
Only runs a single apache instance, and doesn't get any info from
the server logs.
"""
import argparse
import http.client
import json
import socket
import subprocess
import sys


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
        '--apache-port', required=True, type=int,
        help='Port where apache server is running'
    )    
    args = argument_parser.parse_args()

    # start apache if it's not started yet
    subprocess.run('pidof apache2 > /dev/null || apache2ctl start', shell=True)

    data = open(args.request_stream, 'rb').read()
    sock = socket.create_connection(('127.0.0.1', args.apache_port), timeout=10)
    sock.sendall(data)

    # Share the same file object between all responses (required for pipelining)
    responses_file = sock.makefile('rb')
    mock_sock = MockSocket(responses_file)

    responses = []
    response_bodies = []
    while True:
        response = NonClosingResponse(mock_sock)
        try:
            response.begin()
        except ConnectionError as e:
            # This probably just means that the server handled all requests
            print(f'Connection Error: {e}', file=sys.stderr)
            break
        except socket.timeout:
            print('Connection timed out', file=sys.stderr)
            break
        if response.will_close:
            break
        responses.append(response)
        response_bodies.append(response.read())
    sock.close()
    responses_file.close()

    print(f'Got {len(responses)} responses')

    parsed_requests = []
    for response, response_body in zip(responses, response_bodies):
        if 200 <= response.status < 400:
            # OK; response is parse result
            try:
                response_content = json.loads(response_body)
                parsed_requests.append(response_content)
            except (json.JSONDecodeError, UnicodeDecodeError) as e:
                print(f'Malformed JSON for allegedly OK request: {e}', file=sys.stderr)
        else:
            # Errors; print 'em
            for line in response_body.decode('utf-8', errors='replace').splitlines():
                print(line, file=sys.stderr)
            parsed_requests.append({'error': True})

    if args.json_output is not None:
        with open(args.json_output, 'w') as f:
            json.dump(parsed_requests, f, indent=2)


if __name__ == '__main__':
    main()