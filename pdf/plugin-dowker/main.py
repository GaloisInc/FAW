#! /usr/bin/env python3

"""FAW plugin for running Michael Robinson's Bernoulli Test, part of work with
BAE systems.
"""


import argparse
import json
import numpy as np
import os
import packaging.version
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
    threshold = dec_args.get('threshold', 0.98)
    use_refs = dec_args.get('use_refs', False)
    topk_shown = dec_args.get('topk_shown', 10)
    file_features = dec_args.get('file_features', False)
    feature_regex = dec_args.get('feature_regex', '')
    feature_regex_insensitive = dec_args.get('feature_regex_insensitive', True)
    html_vars = {
            'debug': '',
            'threshold': threshold,
            'use_refs': use_refs,
            'topk_shown': topk_shown,
            'file_features': file_features,
            'feature_regex': feature_regex,
            'feature_regex_insensitive': feature_regex_insensitive,
            'old_args': json.dumps(dec_args),
            'api_url': args.api_url,
    }

   # try:
   #     ft_re = re.compile(feature_regex, flags=re.I if feature_regex_insensitive else 0)
   # except re.error:
    #    raise ValueError(feature_regex)
   
    file_names = []
    file_names_backward = {}
    ft_names=list()
    f =  open('keys.txt', 'r')
    allKeys = f.read()
    for key in allKeys.split("\n"):
        ft_names.append(key)    
   # ft_names = ['pdfinfo_<<workbench: Exit code 0>>', 'pdfid_<<workbench: Exit code 0>>', 'pdffonts_<<workbench: Exit code 0>>', 'pdftocairo-pdf_<<workbench: Exit code 0>>', 'pdftops_<<workbench: Exit code 0>>', 'pdftotext_<<workbench: Exit code 0>>' 'polyfile_<<workbench: Exit code 0>>', 'qpdf-check_<<workbench: Exit code 2>>', 'pdfid_<<workbench: Exit code 0>>', 'caradoc-stats_<<workbench: Exit code 2>>', 'caradoc-strict_<<workbench: Exit code 2>>', 'mutool-clean_<<workbench: Exit code 0>>', 'pdfium_<<workbench: Exit status: RuntimeError>>']
    ft_lookup = {}
    matrix = []
    curr=0
    errorMatrix = np.asmatrix(np.zeros(shape=(64,len(ft_names)+1)))
    set1 = set()
    set2 = set()
    mode = 'files'
    lines=list()
    file1 = open("MyFile.txt", "w") 
    for line in sys.stdin:
        line = line.strip()
        file1.write(line)

        if not line:
            continue
        
        if line == 'REFERENCES':
            mode = 'refs'
        elif mode == 'files':
            obj = json.loads(line)
            
            file_names.append(obj.pop('_id'))
            file_names_backward[file_names[-1]] = len(file_names) - 1
            matrix.append({})
            #if "1253.pdf" in line:
#                lines.append(line)
             #   allvals = line.split(",")
              #  with open('names.txt', 'w') as f:
             #       for val in allvals:
              #          f.write(val+"\n")

            i=1
            #a="pdfinfo_<>"
            
            #matrix[-1][0]=file_names[-1]
            curr+=1
          #  errorMatrix.resize(len(ft_names),curr, False)
            
            for name in ft_names:
                matrix[-1][i]=0
                errorMatrix[curr,i]=1
                matched =False
                for key in obj.keys():
                    if name.split("\\<\\<")[0]==key.split("\\<\\<")[0]:
                        matrix[-1][i]=obj[key]
                        errorMatrix[curr,i]=1-int(obj[key])
                        matched=True
                        break
                if not(matched):
                    errorMatrix[curr, i]=1
                i+=1

        elif mode == 'refs':
            # Ensure UI maintains its old decision (this was a regression; the
            # new code does not have this flaw)
            #print(line)
           # lines.append(line)
            if not use_refs:
                continue

            obj = json.loads(line)
            if obj['status'] == 'valid':
                set1.add(obj['testfile'])
            else:
                set2.add(obj['testfile'])
        else:
            raise NotImplementedError(mode)
    file1.close()
    #Create Dowker from matrix
    errorMatrixToDowker(errorMatrix, file_names, args.html_out)
    #Return plotly args
  #  html_vars['file_names']=str(curr)
    # Sort files based on likelihood of being 'odd'
   # html_vars['lines']=lines[0]
   # html_vars['lines']=str(errorMatrix)
 #   write_html(os.path.join(_path, 'index.pug'), html_vars, args.html_out)


def write_html(template_file, vars, html_out):
    """Given a template in `template_file`, format it in the Pug language
    using `vars` as context variables, printing the resulting HTML to stdout.
    """
    with open(template_file) as f:
        template = f.read()

    parser = pypugjs.parser.Parser(template)
    block = parser.parse()
    compiler = pypugjs.ext.html.Compiler(block)
    if True:#packaging.version.parse(pypugjs.__version__) < packaging.version.parse('5.9.9'):
        # Fix 5.9.4 and before
        def interpolate_replacement(self, text, escape=True):
            esc = lambda x: x
            if escape:
                esc = lambda x: x.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
            return self._interpolate(text, lambda x: esc(str(self._do_eval(x))))
        pypugjs.ext.html.Compiler.interpolate = interpolate_replacement
    compiler.global_context = vars
    with open("dowker_faw.html", 'r') as f1:
        with open(html_out, 'w') as f:
           # f.write("TEXT\n")
            #f.write(vars['file_names']);
            for line in f1:
                f.write(line)
            #f.write(vars['lines'])
#            f.write(compiler.compile())


if __name__ == '__main__':
    main()

