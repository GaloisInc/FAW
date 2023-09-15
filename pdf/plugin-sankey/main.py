#! /usr/bin/env python3
import argparse
import json
import urllib.parse

import jinja2

import sankey


parser = argparse.ArgumentParser()
parser.add_argument('filename')
parser.add_argument('--api-url', required=True)
parser.add_argument('--debug', help="Verbose debugging output of internal Python data", action='store_true', default=False)
parser.add_argument('--keep-debug-markers',  help="Keep all data markers in debug output", action='store_true')
parser.add_argument('--force', help="Force processing by ignoring possible data issues", action='store_true')


def main():
    args = parser.parse_args()
    # Extract the root URI from the workbench API URL.
    # Static resources can't be retrieved from local paths from inside an iframe,
    # so we need an absolute path that will work for the client.
    root_uri = urllib.parse.urlunparse(
        urllib.parse.urlparse(args.api_url)._replace(path='')
    )
    sankey_data = sankey.pdf_sankey_data(
        args.filename,
        verbose_debug=args.debug,
        force=args.force,
        keep_debug_markers=args.keep_debug_markers,
    )

    with open('index.html.jinja') as f:
        html_template = jinja2.Template(f.read())
    with open('main.js') as f:
        main_script = f.read()
    print(
        html_template.render(
            DEBUG_LINES=sankey_data.debug_lines,
            CSV_TEXT=json.dumps('\n'.join(','.join(row) for row in sankey_data.sankey_data)),
            MAIN_SCRIPT=main_script,
            ABSOLUTE_ROOT_URI=root_uri,
        )
    )


if __name__ == '__main__':
    main()
