#! /usr/bin/env python3

"""FAW plugin for running Michael Robinson's Bernoulli Test, part of work with
BAE systems.
"""

from bernoulli_test import run_bernoulli_test

import argparse
import json
import numpy as np
import os
import pypugjs
import sys

_path = os.path.dirname(os.path.abspath(__file__))

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('api_url')
    ap.add_argument('json_arguments')
    ap.add_argument('html_out')
    args = ap.parse_args()

    dec_args = json.loads(args.json_arguments)
    threshold = dec_args.get('threshold', 0.98)
    use_refs = dec_args.get('use_refs', False)
    topk_shown = dec_args.get('topk_shown', 10)
    file_features = dec_args.get('file_features', False)
    html_vars = {
            'threshold': threshold,
            'use_refs': use_refs,
            'topk_shown': topk_shown,
            'file_features': file_features,
            'old_args': json.dumps(dec_args),
            'api_url': args.api_url,
    }

    file_names = []
    file_names_backward = {}
    ft_names = []
    ft_lookup = {}
    matrix = []
    set1 = set()
    set2 = set()
    mode = 'files'
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        if line == 'REFERENCES':
            mode = 'refs'
        elif mode == 'files':
            obj = json.loads(line)
            file_names.append(obj.pop('_id'))
            file_names_backward[file_names[-1]] = len(file_names) - 1
            matrix.append({})
            for k, v in obj.items():
                ft_i = ft_lookup.get(k)
                if ft_i is None:
                    ft_i = ft_lookup[k] = len(ft_names)
                    ft_names.append(k)
                matrix[-1][ft_i] = 1.
        elif mode == 'refs':
            # Ensure UI maintains its old decision
            print(line)

            if not use_refs:
                continue

            obj = json.loads(line)
            if obj['status'] == 'valid':
                set1.add(obj['testfile'])
            else:
                set2.add(obj['testfile'])
        else:
            raise NotImplementedError(mode)

    # Having consumed all data, sort messages for user-friendliness
    mat = np.zeros((len(matrix), len(ft_names)))
    ft_i_sorted = sorted(range(len(ft_names)), key=lambda m: ft_names[m].lower())
    ft_i_backward = [0 for _ in ft_i_sorted]
    for i_back, i in enumerate(ft_i_sorted):
        ft_i_backward[i] = i_back
    for m_i, m in enumerate(matrix):
        for k, v in m.items():
            mat[m_i, ft_i_backward[k]] = v

    mat1 = mat
    mat2 = None

    if use_refs:
        # mat1 is "good" files, mat2 is "bad" files, the Bernoulli test
        # implemented by Michael Robinson

        id_good = []
        id_bad = []
        for i, fname in enumerate(file_names):
            if fname in set1:
                id_good.append(i)
            else:
                id_bad.append(i)

        mat2 = mat1[id_bad]
        mat1 = mat1[id_good]
        file_names = [file_names[i] for i in id_good]

        mat1 = mat1.T
        mat2 = mat2.T
    else:
        mat1 = mat1.T

    # Pass to bernoulli
    interesting, interesting_fts = run_bernoulli_test(threshold, mat1, mat2)
    html_vars['files'] = []
    for i, i_fts in zip(interesting, interesting_fts):
        if topk_shown > len(i_fts):
            fts = range(len(i_fts))
        elif topk_shown > 0:
            fts = np.argpartition(i_fts, -topk_shown)[-topk_shown:]
        else:
            fts = []
        html_vars['files'].append({
                'name': file_names[i],
                'topk': (
                    sorted([(ft_names[j], i_fts[j]) for j in fts],
                        key=lambda m: -m[1])
                    if file_features
                    else []),
        })

    # Compute most relevant features across all interesting files.
    ft_weights = interesting_fts.mean(0)
    ft_order = np.argpartition(ft_weights, -topk_shown)[-topk_shown:]
    html_vars['features'] = sorted(
            [(ft_names[i], ft_weights[i]) for i in ft_order],
            key=lambda m: -m[1])

    write_html(os.path.join(_path, 'index.pug'), html_vars, args.html_out)


def write_html(template_file, vars, html_out):
    """Given a template in `template_file`, format it in the Pug language
    using `vars` as context variables, printing the resulting HTML to stdout.
    """
    with open(template_file) as f:
        template = f.read()

    parser = pypugjs.parser.Parser(template)
    block = parser.parse()
    compiler = pypugjs.ext.html.Compiler(block)
    # Fix 5.9.4 and before
    def interpolate_replacement(self, text, escape=True):
        esc = lambda x: x
        if escape:
            esc = lambda x: x.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
        return self._interpolate(text, lambda x: esc(str(self._do_eval(x))))
    pypugjs.ext.html.Compiler.interpolate = interpolate_replacement
    compiler.global_context = vars
    with open(html_out, 'w') as f:
        f.write(compiler.compile())


if __name__ == '__main__':
    main()

