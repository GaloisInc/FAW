
import argparse
import os

if __name__ == '__main__':
    ap = argparse.ArgumentParser()
    ap.add_argument('file')
    args = ap.parse_args()

    fold = args.file
    assert fold.startswith('/home/pdf-files/'), fold
    fold = fold[len('/home/pdf-files/'):]
    while True:
        fold, _ = os.path.split(fold)
        if not fold:
            break
        print(f'Folder: {os.path.basename(fold)}')

