#! /usr/bin/env python3
import argparse
import base64
import binascii
import collections
import dataclasses
import json
import os
import pathlib
import html
from typing import DefaultDict, Iterable, Optional, Sequence, Set, Tuple, TypedDict, List, Dict, Union


CSS_PRELUDE = """
<style>
table {
    border-collapse: collapse;
}
th, td {
    border: solid black 1px;
    padding: 0.2rem;
}
tbody th {
    text-align: right;
}
.highlight {
    background-color: yellow;
}
</style>
"""


class SerializedRequest(TypedDict):
    error: bool
    method: bytes
    path: bytes
    version: bytes
    headers: List[Tuple[bytes, bytes]]
    bodyError: bool
    body: bytes


def _b64_decode(s: str) -> bytes:
    return base64.b64decode(bytes(s, encoding='utf-8'))


@dataclasses.dataclass
class Request:
    error: bool
    method: Optional[bytes]
    path: Optional[bytes]
    version: Optional[bytes]
    headers: Sequence[Tuple[bytes, bytes]]
    body_error: bool
    body: Optional[bytes]

    @staticmethod
    def from_dict(d: SerializedRequest) -> 'Request':
        return Request(
            error=d.get('error', True),
            method=_b64_decode(d['method']) if 'method' in d else None,
            path=_b64_decode(d['path']) if 'path' in d else None,
            version=_b64_decode(d['version']) if 'version' in d else None,
            headers=[
                (_b64_decode(field), _b64_decode(value))
                for field, value in d.get('headers', [])
            ],
            body_error=d.get('bodyError', False),
            body=_b64_decode(d['body']) if 'body' in d else None,
        )


@dataclasses.dataclass
class Cell:
    content: Union[str, Iterable[str]]
    """If non-string, split onto multiple lines."""
    rowspan: int = 1
    header: bool = False
    collapsible_with_summary: Optional[str] = None
    highlight: bool = False
    preformatted: bool = False

    def to_html(self) -> str:
        element = 'th' if self.header else 'td'
        attributes = []
        if self.rowspan > 1:
            attributes.append(f'rowspan="{self.rowspan}"')
        if self.highlight:
            attributes.append('class="highlight"')
        raw_content_chunks = [self.content] if isinstance(self.content, str) else self.content
        if self.preformatted:
            content_chunks = [
                f'<pre>{html.escape(chunk)}</pre>' for chunk in raw_content_chunks
            ]
        else:
            content_chunks = [html.escape(chunk) for chunk in raw_content_chunks]
        if self.collapsible_with_summary is None:
            content = '<br />'.join(content_chunks)
        else:
            content = (
                f'<details><summary>{html.escape(self.collapsible_with_summary)}'
                f'</summary>{"<br />".join(content_chunks)}</details>'
            )
        return f'<{element} {" ".join(attributes)}>{content}</{element}>'


def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument(
        'artifact_dir',
        help='Dir containing per-parser dirs of JSON parsed requests'
    )
    argument_parser.add_argument(
        '--print-html', action='store_true',
        help='Run as file detail plugin'
    )
    args = argument_parser.parse_args()

    artifact_dir = pathlib.Path(args.artifact_dir)
    print_html: bool = args.print_html

    write_message = print
    if print_html:
        print(CSS_PRELUDE)
        def write_message(text: str) -> None:
            print(f'<p>{html.escape(text)}</p>')

    tool_names = sorted(os.listdir(artifact_dir))
    requests_by_tool: Dict[str, Sequence[Request]] = {tool_name: [] for tool_name in tool_names}
    for tool_name in tool_names:
        files = list((artifact_dir / tool_name).iterdir())
        if len(files) == 0:
            write_message(f'No output from tool {tool_name}')
        elif len(files) == 1:
            try:
                with open(files[0], 'r') as f:
                    requests_by_tool[tool_name] = list(map(Request.from_dict, json.load(f)))
            except (json.JSONDecodeError, OSError, binascii.Error) as e:
                write_message(f'Error reading artifact from tool {tool_name}: {e}')
        else:
            write_message(f'Multiple files from tool {tool_name}')
    num_parsers = len(tool_names)

    table_rows: List[List[Cell]] = []
    max_request_count = max(len(requests) for requests in requests_by_tool.values())
    for request_index in range(max_request_count):
        maybe_request_by_tool = {
            tool_name: requests[request_index] if request_index < len(requests) else None
            for tool_name, requests in requests_by_tool.items()
        }

        no_output_count = 0
        # Treat all errors the same when diffing
        error_count = 0
        method_values : Set[bytes] = set()
        path_values : Set[bytes] = set()
        version_values : Set[bytes] = set()
        # Ignore header order (except for repeated fields) when diffing,
        # since some tools may expose them in an unordered map
        header_counts: Set[int] = set()
        header_values_by_field : DefaultDict[bytes, Set[Tuple[bytes]]] = collections.defaultdict(set)
        header_value_list_by_field_by_tool: DefaultDict[str, Dict[bytes, Sequence[bytes]]] = collections.defaultdict(dict)
        body_values : Set[Optional[bytes]] = set()
        for tool_name, maybe_request in maybe_request_by_tool.items():
            if maybe_request is None:
                no_output_count += 1
                continue
            if maybe_request.error:
                error_count += 1
                continue
            method_values.add(maybe_request.method)
            path_values.add(maybe_request.error)
            version_values.add(maybe_request.error)

            header_counts.add(len(maybe_request.headers))
            header_value_list_by_field: DefaultDict[bytes, List[bytes]] = collections.defaultdict(list)
            for field, value in maybe_request.headers:
                header_value_list_by_field[field].append(value)
            for field, values in header_value_list_by_field.items():
                header_values_by_field[field].add(tuple(values))
            header_value_list_by_field_by_tool[tool_name] = header_value_list_by_field

            if maybe_request.body_error:
                error_count += 1
                continue
            body_values.add(maybe_request.body if maybe_request.body is not None else b'')

        if print_html:
            status_differs = (
                0 < no_output_count < num_parsers
                or 0 < error_count < num_parsers
            )
            all_bad = no_output_count + error_count == num_parsers
            num_header_rows = len(header_values_by_field.keys())
            status_row = [
                Cell(
                    str(request_index),
                    header=True,
                    rowspan=1 if all_bad else 6 + num_header_rows,
                ),
                Cell('Status', header=True, highlight=status_differs),
                *(
                    Cell(
                        'no parse attempt' if maybe_request is None
                        else 'error' if maybe_request.error
                        else 'body-parsing error' if maybe_request.body_error
                        else 'parsed',
                        highlight=status_differs,
                        preformatted=True,
                    )
                    for maybe_request in maybe_request_by_tool.values()
                ),
            ]
            if all_bad:
                table_rows.append(status_row)
                continue

            method_row = [
                Cell('Method', header=True, highlight=len(method_values) > 1),
                *(
                    Cell(
                        '' if maybe_request is None
                        else '' if maybe_request.method is None
                        else maybe_request.method.decode(errors='replace'),
                        highlight=len(method_values) > 1,
                        preformatted=True,
                    )
                    for maybe_request in maybe_request_by_tool.values()
                ),
            ]
            path_row = [
                Cell('Path', header=True, highlight=len(path_values) > 1),
                *(
                    Cell(
                        '' if maybe_request is None
                        else '' if maybe_request.path is None
                        else maybe_request.path.decode(errors='replace'),
                        highlight=len(path_values) > 1,
                        preformatted=True,
                    )
                    for maybe_request in maybe_request_by_tool.values()
                ),
            ]
            version_row = [
                Cell('Version', header=True, highlight=len(version_values) > 1),
                *(
                    Cell(
                        '' if maybe_request is None
                        else '' if maybe_request.version is None
                        else maybe_request.version.decode(errors='replace'),
                        highlight=len(version_values) > 1,
                        preformatted=True,
                    )
                    for maybe_request in maybe_request_by_tool.values()
                ),
            ]
            headers_row = [
                Cell('Headers', header=True, highlight=len(header_counts) > 1),
                *(
                    Cell(
                        '' if maybe_request is None
                        else f'{len(maybe_request.headers)} headers',
                        highlight=len(header_counts) > 1,
                        preformatted=True,
                    )
                    for maybe_request in maybe_request_by_tool.values()
                ),
            ]
            header_field_rows = [
                [
                    Cell(
                        header_field.decode(errors='replace'),
                        header=True,
                        highlight=len(header_values) > 1,
                        preformatted=True,
                    ),
                    *(
                        Cell(
                            [
                                value.decode(errors='replace')
                                for value in header_value_list_by_field_by_tool[tool_name].get(
                                    header_field, []
                                )
                            ],
                            highlight=len(header_values) > 1,
                            preformatted=True,
                            collapsible_with_summary=(
                                None if sum(map( len, header_value_list_by_field_by_tool[tool_name].get(
                                    header_field, []
                                ))) <= 32
                                else 'Long Value'
                            )
                        )
                        for tool_name in tool_names
                    )
                ]
                for header_field, header_values in header_values_by_field.items()
            ]
            body_row = [
                Cell('Body', header=True, highlight=len(body_values) > 1),
                *(
                    Cell(
                        '' if maybe_request is None
                        else '' if maybe_request.body is None
                        else maybe_request.body.decode(errors='replace'),
                        highlight=len(body_values) > 1,
                        preformatted=True,
                        collapsible_with_summary=(
                            'Nonempty Body' if maybe_request and maybe_request.body else None
                        )
                    )
                    for maybe_request in maybe_request_by_tool.values()
                ),
            ]
            table_rows.extend(
                [
                    status_row,
                    method_row,
                    path_row,
                    version_row,
                    headers_row,
                    *header_field_rows,
                    body_row
                ]
            )
        else:
            if 0 < no_output_count < num_parsers:
                print(f'Request {request_index}: {no_output_count}/{num_parsers} parsers did not attempt to parse')
            if 0 < error_count < num_parsers:
                print(f'Request {request_index}: {error_count}/{num_parsers} parsers had errors')
            if len(method_values) > 1:
                print(f'Request {request_index}: {len(method_values)} distinct methods')
            if len(path_values) > 1:
                print(f'Request {request_index}: {len(path_values)} distinct paths')
            if len(version_values) > 1:
                print(f'Request {request_index}: {len(version_values)} distinct versions')
            if len(header_counts) > 1:
                print(f'Request {request_index}: {len(header_counts)} distinct header counts')
            for field, values in header_values_by_field.items():
                if len(values) > 1:
                    print(f'Request {request_index}: {len(values)} distinct values (including repeated entries) for header: {field}')
            if len(body_values) > 1:
                print(f'Request {request_index}: {len(body_values)} distinct bodies')
    
    if print_html:
        # Actually print the table here
        print('<table><thead><tr>')
        print('<th>Request Index</th><th></th>')
        for tool_name in tool_names:
            print(f'<th>{tool_name}</th>')
        print('</tr></thead><tbody>')
        for row in table_rows:
            print('<tr>', *[cell.to_html() for cell in row], '</tr>')
        print('</tbody></table>')


if __name__ == '__main__':
    main()
