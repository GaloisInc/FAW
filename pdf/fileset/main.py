
import argparse
import importlib
import json
import os

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('api_info', type=json.loads)
    ap.add_argument('fname', type=str)
    args = ap.parse_args()

    # Path fixing for dask
    path = os.path.dirname(os.path.abspath(__file__))
    path_prefix = '/home/dist/'
    assert path.startswith(path_prefix)
    module_name = path[len(path_prefix):]
    assert '/' not in module_name, module_name

    module = importlib.import_module(module_name + '.cmd_lookup')
    module.main(args.api_info, args.fname)


if __name__ == '__main__':
    main()

