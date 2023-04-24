#! /usr/bin/env python3
import argparse
import base64
import http
import http.server
import io
import json
import sys
from typing import Tuple, TypedDict, List

def main():
    argument_parser = argparse.ArgumentParser()
    argument_parser.add_argument(
        'artifact_dir', required=True,
        help='Dir containing per-parser dirs of JSON parsed requests'
    )
    argument_parser.add_argument(
        '--html', action='store_true',
        help='Write HTML to standard output (run as file detail plugin)'
    )
    args = argument_parser.parse_args()

    



if __name__ == '__main__':
    main()
