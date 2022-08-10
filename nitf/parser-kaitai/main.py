
import nitf

import argparse

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('input_file')
    args = ap.parse_args()

    data = nitf.Nitf.from_file(args.input_file)
    print(data.header)

if __name__ == '__main__':
    main()

