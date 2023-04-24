#! /usr/bin/env python3

import argparse
import html.parser
import os
import sys
import requests


def main():
    """Write FF-delimited page text to stdout and output_file"""
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument('file')
    argument_parser.add_argument('-o', '--output-file', required=True)
    args = argument_parser.parse_args()

    r = requests.put('http://localhost:9998/tika', data=None,
        headers={
            'Content-Type': 'application/pdf',
            'fileUrl': 'file://' + os.path.abspath(args.file),
            'Accept': 'text/html'
        }
    )
    stderr = requests.status_codes._codes[r.status_code][0]
    pages = [[]]
    if r.status_code == requests.codes.ok:
        # Parse out pages. We use HTML here because otherwise, Tika doesn't
        # include page boundaries.
        class HtmlByPage(html.parser.HTMLParser):
            def __init__(self):
                super().__init__()
                self.stack = ['root']
            def handle_starttag(self, tag, attrs):
                self.stack.append(tag)
                if tag == 'div':
                    a = [v for v in attrs if v[0] == 'class']
                    if a and a[0][1] == 'page':
                        pages.append([])
            def handle_endtag(self, tag):
                assert self.stack[-1] == tag, tag
                self.stack.pop()
            def handle_data(self, data):
                if self.stack[-1] == 'a':
                    # Do not include link content... other parsers seem to
                    # ignore these too
                    return
                pages[-1].append(data)
        parser = HtmlByPage()
        parser.feed(r.text)
        assert parser.stack == ['root'], parser.stack

    print(stderr, file=sys.stderr)
    with open(args.output_file, 'w') as f:
        # First page was catchall, and doesn't contain useful information
        text = '\f'.join(''.join(p) for p in pages[1:])
        f.write(text)
        print(text)


if __name__ == '__main__':
    main()
