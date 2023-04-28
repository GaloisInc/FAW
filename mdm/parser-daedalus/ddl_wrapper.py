#! /usr/bin/env python3

"""Wraps a DDL-compiled parser to pass the `--json` option and to turn the
results into a FAW-friendly format.
"""

import argparse
import json
import subprocess
import sys

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('parser')
    ap.add_argument('file')
    args = ap.parse_args()

    p = subprocess.Popen([args.parser, '--json', args.file],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = p.communicate()
    ret = p.wait()

    print(err.decode(), file=sys.stderr)

    out = out.decode()
    out = json.loads(out)
    assert 'results' in out, out.keys()
    out = out['results']

    if isinstance(out, dict) and 'error' in out:
        # Note that we use `json.dumps` to collapse newlines
        print(f'Error at {out["offset"]}: {json.dumps(out["error"])}')
        # Per Iavor, schema is:
        # CallStack := [ Funs ]
        # Funs        := [ Fun ]
        # Fun          := STRING | [  STRING, NUMBER ]
        for cfuns in out['context']:
            for cfun in cfuns:
                if isinstance(cfun, list):
                    print(f'Error context: {cfun[0]}::{cfun[1]}')
                else:
                    print(f'Error context: {cfun}')
    else:
        def dump_obj(prefix, o):
            if isinstance(o, list):
                for ooi, oo in enumerate(o):
                    dump_obj(f'{prefix}[{ooi}]', oo)
                return
            elif not isinstance(o, dict):
                return
            for k, v in o.items():
                print(f'Result path: {prefix}.{k}')
                dump_obj(f'{prefix}.{k}', v)
        dump_obj('', out)

    # Forward ret code
    sys.exit(ret)


if __name__ == '__main__':
    main()

