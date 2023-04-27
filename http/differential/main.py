#! /usr/bin/env python3
import argparse
import base64
import binascii
import dataclasses
import http
import http.server
import itertools
import io
import json
import sys
import os
import pathlib
from typing import Optional, Sequence, Tuple, TypedDict, List, Dict, Type, TypeVar


class Request(TypedDict):
    error: bool
    method: bytes
    path: bytes
    version: bytes
    headers: List[Tuple[bytes, bytes]]
    bodyError: bool
    body: bytes


def _b64_decode(s: str) -> str:
    return base64.b64decode(bytes(s, encoding='utf-8')).decode(errors='replace')


@dataclasses.dataclass
class DeserializedRequest:
    error: bool
    method: Optional[str]
    path: Optional[str]
    version: Optional[str]
    headers: Sequence[Tuple[Optional[str], Optional[str]]]
    bodyError: bool
    body: Optional[str]

    @staticmethod
    def from_dict(d: Request) -> 'DeserializedRequest':
        return DeserializedRequest(
            error=d.get('error', True),
            method=_b64_decode(d['method']) if 'method' in d else None,
            path=_b64_decode(d['path']) if 'path' in d else None,
            version=_b64_decode(d['version']) if 'version' in d else None,
            headers=[
                (_b64_decode(field), _b64_decode(value))
                for field, value in d.get('headers', [])
            ],
            bodyError=d.get('bodyError', False),
            body=_b64_decode(d['body']) if 'body' in d else None,
        )


def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument(
        'artifact_dir',
        help='Dir containing per-parser dirs of JSON parsed requests'
    )
    argument_parser.add_argument(
        '--html-diff', action='store_true',
        help='View diffs between all parsed requests (as file detail plugin)'
    )
    argument_parser.add_argument(
        '--html-view', action='store_true',
        help='View full parsed requests (as file detail plugin)'
    )
    args = argument_parser.parse_args()

    artifact_dir = pathlib.Path(args.artifact_dir)
    html_diff: bool = args.html_diff
    html_view: bool = args.html_view

    tool_names = sorted(os.listdir(artifact_dir))
    requests_by_tool: Dict[str, Sequence[DeserializedRequest]] = {}
    for tool_name in tool_names:
        files = list((artifact_dir / tool_name).iterdir())
        if len(files) == 0:
            print(f'No output from tool {tool_name}')
        elif len(files) == 1:
            try:
                with open(files[0], 'r') as f:
                    requests_by_tool[tool_name] = list(map(DeserializedRequest.from_dict, json.load(f)))
            except (json.JSONDecodeError, OSError, binascii.Error) as e:
                print(f'Error reading artifact from tool {tool_name}: {e}')
        else:
            print(f'Multiple fiels from tool {tool_name}')

    max_request_count = max(len(requests) for requests in requests_by_tool.values())
    for request_index in range(max_request_count):
        maybe_request_by_tool_name = {
            tool_name: requests[request_index] if request_index < len(requests) else None
            for tool_name, requests in requests_by_tool.items()
        }
        if html_view:
            # TODO: Switch output to HTML. For now it's plaintext
            # TODO merge with the diff view
            indent = '  '
            print(f'\nRequest {request_index}:\n')
            for tool_name, maybe_request in maybe_request_by_tool_name.items():
                print(f'{indent}Output from tool {tool_name}:')
                if maybe_request is None:
                    print(f'{indent}None\n')
                    continue
                if maybe_request.error:
                    print(f'{indent}Error parsing request')
                if maybe_request.bodyError:
                    print(f'{indent}Error parsing request body')
                print(f'{indent}Method: {maybe_request.method}')
                print(f'{indent}Path: {maybe_request.path}')
                print(f'{indent}Version: {maybe_request.version}')
                print(f'{indent}Headers:{"" if maybe_request.headers else " None"}')
                for field, value in maybe_request.headers:
                    print(f'{indent}  {field}: {value}')
                print(f'{indent}Body:{" None" if maybe_request.body is None else ""}')
                if maybe_request.body is not None:
                    print(f'{indent}{maybe_request.body}')
                print()


if __name__ == '__main__':
    main()
