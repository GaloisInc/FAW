"""Given a folder with subfolders, run the schizo test and report on the min,
max, etc statistics of each subfolder.
"""

import argparse
import collections
import multiprocessing
import os
import re
import subprocess

progdir = os.path.dirname(os.path.abspath(__file__))
mainscript = os.path.join(progdir, '../main.py')

def dir_arg(s):
    if os.path.isdir(s):
        return s
    raise ArgumentError(f'{s} is not a directory')


def get_rmse(fpath):
    p = subprocess.Popen(['python3', mainscript, fpath],
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = p.communicate()
    r = p.wait()

    vals = {}
    for m in re.finditer(rb'^Max RMSE, (.*) to (.*): ([^\n]+)', stderr, flags=re.M):
        vals[(m.group(1), m.group(2))] = float(m.group(3))
    return vals


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('folder', type=dir_arg)
    args = ap.parse_args()

    pool = multiprocessing.Pool()
    all_files = []
    for path, dirs, files in os.walk(args.folder):
        for f in files:
            fpath = os.path.join(path, f)
            if os.path.isfile(fpath):
                all_files.append(fpath)

    results = pool.map(get_rmse, all_files)
    rmses = sorted(zip(all_files, results), key=lambda k: k[0])
    folders = []
    class Tracker:
        def __init__(self):
            self.items = collections.defaultdict(list)
        def __str__(self):
            result = []
            for (t1, t2), vals in self.items.items():
                result.append(f'{t1} to {t2}: {sum(vals) / len(vals):.4f} ({min(vals):.4f} / {max(vals):.4f})')
            return ', '.join(result)
        def add(self, rmse_vals):
            for k, v in rmse_vals.items():
                self.items[k].append(v)
    def cap(parts, rmse):
        for fi in reversed(range(len(folders))):
            if len(parts) <= fi or parts[fi] != folders[fi][0]:
                fname = '/'.join([ff[0] for ff in folders])
                fstr = "\n  ".join(str(folders[fi][1]).split(", "))
                print(f'{fname}\n  {fstr}')
                folders.pop()
            else:
                break
        for fi in range(len(folders), len(parts)):
            folders.append([parts[fi], Tracker()])
        for f in folders:
            f[1].add(rmse)
    for f, r in rmses:
        remainder = f
        parts = []
        while True:
            remainder, tail = os.path.split(remainder)
            if not tail:
                break
            parts.insert(0, tail)
            if not remainder:
                break
        cap(parts, r)
    cap([], None)


if __name__ == '__main__':
    main()

