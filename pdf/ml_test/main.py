"""Entry point for all ml_test scripts. Unifies finding the python module in a
way that is dask-friendly.
"""

import argparse
import importlib
import json
import os

def main():
    # Find our module
    path = os.path.dirname(os.path.abspath(__file__))
    path_prefix = '/home/dist/'
    assert path.startswith(path_prefix)
    module_name = path[len(path_prefix):]
    assert '/' not in module_name, module_name

    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('api_info', type=json.loads)
    ap.add_argument('command', type=str)
    ap.add_argument('args', nargs=argparse.REMAINDER)
    args = ap.parse_args()

    module = importlib.import_module(module_name + '.cmd_' + args.command)
    module.main(args.api_info, args.args)


if __name__ == '__main__':
    main()

