#! /usr/bin/env python3

import argparse
import json
import os
import requests

def is_number(v):
    try:
        float(v)
    except ValueError:
        return False
    else:
        return True


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('file')
    args = ap.parse_args()

    headers = {'Content-Type': 'application/pdf',
            'fileUrl': 'file://' + os.path.abspath(args.file)}
    r = requests.put('http://localhost:9998/rmeta', data=None, headers=headers)

    statusMsg = requests.status_codes._codes[r.status_code][0]
    print("tika.status: {}".format(statusMsg))

    # Decide whether parsing was successful: look for Tika exceptions in JSON response
    if r.status_code == requests.codes.ok:
        data = json.loads(r.content)
        if len(data) != 1:
            print(f'ERROR: more than one result? {len(data)}')

        for k, v in data[0].items():
            if k in ['resourceName', 'X-TIKA:content']:
                continue

            if '\n' in v:
                print(k)
            elif isinstance(v, list) and v and is_number(v[0]):
                # E.g., charsPerPage or unmappedUnicodeCharsPerPage
                print(f'{k} -- {max(v)}')
            else:
                print(f'{k} -- {v}')


if __name__ == '__main__':
    main()
