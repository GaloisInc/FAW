
import argparse
import json
import pprint
import subprocess
import tempfile

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('fpath')
    args = ap.parse_args()

    stdout = subprocess.run(['polyfile', '--format', 'json', args.fpath],
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL).stdout

    data = json.loads(stdout)
    for file_desc in data['struc']:
        print(f'Found file of type: {file_desc["name"]}')
        if file_desc['name'] != 'application/pdf':
            continue

        for obj in file_desc['subEls']:
            if obj['type'] != 'PDFObject':
                continue

            for obj_el in obj['subEls']:
                if obj_el['type'] != 'PDFDictionary':
                    continue

                _print_dict_vals(obj_el)


def _print_dict_vals(obj_el):
    for maybe_kvp in obj_el['subEls']:
        if maybe_kvp['type'] != 'KeyValuePair':
            continue

        try:
            key, value = maybe_kvp['subEls']
            # For searching, treat keys as distinct feature from values
            print(f'Key {key["value"]}')
            if value['type'] == 'PDFDictionary':
                print(f'Value {key["value"]}: <<PDFDictionary>>')
                _print_dict_vals(value)
            elif value['type'] == 'Value':
                print(f'Value {key["value"]}: {json.dumps(value["value"])}')
            else:
                print(f'Error: Unknown value type: {value["type"]}')
        except:
            raise ValueError(f'Handling {pprint.pformat(maybe_kvp)}')


if __name__ == '__main__':
    main()

