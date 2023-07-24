#! /usr/bin/env python3
import argparse
import json

import jinja2

import sankey


parser = argparse.ArgumentParser()
parser.add_argument('filename')
parser.add_argument('--debug', help="Verbose debugging output of internal Python data", action='store_true', default=False)
parser.add_argument('--keep-debug-markers',  help="Keep all data markers in debug output", action='store_true')
parser.add_argument('--force', help="Force processing by ignoring possible data issues", action='store_true')


def main():
    args = parser.parse_args()
    sankey_data = sankey.pdf_sankey_data(
        args.filename,
        verbose_debug=args.debug,
        force=args.force,
        keep_debug_markers=args.keep_debug_markers,
    )

    with open('index.html') as f:
        html_template = jinja2.Template(f.read())
    with open('main.js') as f:
        main_script = f.read()
    print(
        html_template.render(
            DEBUG_LINES=sankey_data.debug_lines,
            CSV_TEXT=json.dumps('\n'.join(','.join(row) for row in sankey_data.sankey_data)),
            MAIN_SCRIPT=main_script,
        )
    )


if __name__ == '__main__':
    main()
