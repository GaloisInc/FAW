"""Processes an image with a given ICC profile via the given BIN, which is a
path to an iccApplyProfile executable.
"""

import argparse
import base64
import html
import os
import subprocess
import sys

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('bin')
    ap.add_argument('file_in')
    ap.add_argument('file_out')
    ap.add_argument('others', nargs='*')
    args = ap.parse_args()

    cmd = [args.bin, args.file_in, args.file_out] + args.others
    print(f'Calling {" ".join(cmd)}', file=sys.stderr)
    p = subprocess.Popen(
            [args.bin, args.file_in, args.file_out] + args.others,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT)
    stdout, _stderr = p.communicate()
    stdout = stdout.decode('utf-8')
    retcode = p.wait()
    print(stdout, file=sys.stderr)
    print(f'Made file of size {len(open(args.file_out, "rb").read())}', file=sys.stderr)
    jpg_file = args.file_out + '.jpg'
    try:
        print(f'Converting output to JPG for browser...', file=sys.stderr)
        subprocess.call(['convert', args.file_out, jpg_file])

        with open(jpg_file, 'rb') as f:
            b64 = base64.b64encode(f.read())
            b64 = b64.decode('latin1')
            print(f'<!DOCTYPE html><html><body>')
            print(f'<img src="data:image/jpg;base64,{b64}" />')
            print(f'<div>Return code: {retcode}</div>')
            print(f'<div style="white-space: pre-wrap">Output: {html.escape(stdout)}</div>')
            print(f'</body></html>')
    finally:
        os.unlink(jpg_file)


if __name__ == '__main__':
    main()

