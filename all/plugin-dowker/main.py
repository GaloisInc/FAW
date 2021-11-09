#! /usr/bin/env python3

"""FAW plugin for running Michael Robinson's Bernoulli Test, part of work with
BAE systems.
"""


import argparse
import json
import numpy as np
import os
import pypugjs
import re
import sys

from errorMatrixToDowker import errorMatrixToDowker
_path = os.path.dirname(os.path.abspath(__file__))

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('api_url')
    ap.add_argument('json_arguments')
    ap.add_argument('html_out')
    args = ap.parse_args()

    dec_args = json.loads(args.json_arguments)
    dec_args.setdefault('feature_regex', ': exit code (?!0)')

    file_names = []
    file_names_backward = {}
    ft_names = []
    ft_lookup = {}
    ft_regex = re.compile(dec_args['feature_regex'], flags=re.I)

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
                if ft_regex.search(k) is None:
                    continue
                ft_i = ft_lookup.get(k)
                if ft_i is None:
                    ft_i = ft_lookup[k] = len(ft_names)
                    ft_names.append(k)
                matrix[-1][ft_i] = 1.
        elif mode == 'refs':
            # Ensure UI maintains its old decision (this was a regression; the
            # new code does not have this flaw)
            #print(line)
            obj = json.loads(line)
            if obj['status'] == 'valid':
                set1.add(obj['testfile'])
            else:
                set2.add(obj['testfile'])
        else:
            raise NotImplementedError(mode)

    # From sparse to dense
    mat = np.zeros((len(matrix), len(ft_names)))
    ft_i_sorted = sorted(range(len(ft_names)), key=lambda m: ft_names[m].lower())
    ft_i_backward = [0 for _ in ft_i_sorted]  # goes from original to sorted
    for i_back, i in enumerate(ft_i_sorted):
        ft_i_backward[i] = i_back
    for m_i, m in enumerate(matrix):
        for k, v in m.items():
            mat[m_i, ft_i_backward[k]] = v

    #Create Dowker from matrix
    plotly_div = errorMatrixToDowker(mat, file_names,
            [ft_names[i] for i in ft_i_sorted])

    parser = pypugjs.parser.Parser(r'''
!!! 5
html
    head
        title Dowker plot
        script!= "let api_url = '" + api_url + "'; let old_args = JSON.parse('" + old_args_str.replace('\\', '\\\\').replace("'", "\\'") + "');"
        script.
            function callUrl(fn, args) {
                let req = new XMLHttpRequest();
                let url = api_url + fn;
                req.open('post', url, true);
                req.setRequestHeader('Content-Type', 'application/json');
                req.send(JSON.stringify(args));
            }

            function reprocess(new_args) {
                let args = Object.assign({}, old_args, new_args);
                callUrl('redecide', args);
            }
    body
        p Feature regex:
            input(id="featureField" type="text" value=old_args_obj['feature_regex'])
            input(type="button" value="Change" onclick="reprocess({feature_regex: document.getElementById('featureField').value})")
        div!= plotly_div
            ''')
    block = parser.parse()
    compiler = pypugjs.ext.html.Compiler(block)
    with open(args.html_out, 'w') as f:
        compiler.global_context = {
                'api_url': args.api_url,
                'old_args_obj': dec_args,
                'old_args_str': json.dumps(dec_args),
                'plotly_div': plotly_div,
        }
        f.write(compiler.compile())


if __name__ == '__main__':
    main()

