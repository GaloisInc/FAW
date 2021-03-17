"""Show parsing result on given file.
"""

import argparse
import faw_pipelines_util
import json

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('input_file')
    ap.add_argument('api_info')
    args = ap.parse_args()

    api = faw_pipelines_util.Api(json.loads(args.api_info))

    print('Parse result')

if __name__ == '__main__':
    main()