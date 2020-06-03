"""Uses pypugjs to format `index.pug` based on lines passed to standard input.
"""

import pypugjs

import json, os, re, sys

_path = os.path.dirname(os.path.abspath(__file__))

def main():
    _, output_html, api_url, options = sys.argv
    options = json.loads(options)

    v1, v2 = 'valid', 'rejected'
    flip = 'flip'
    if options.get('flip'):
        v1, v2 = v2, v1
        flip = ''

    html_context = {}
    html_context['api_url'] = api_url
    html_context['flip'] = flip
    html_context['v1'] = v1
    html_context['files'] = files = []

    for l in sys.stdin:
        f = json.loads(l)
        files.append({'id': f['_id']})
        print(json.dumps({
            'testfile': f['_id'],
            'status': v1 if 'qpdf-check_<<workbench: Exit code 0>>' in f else v2,
        }))

    write_html(os.path.join(_path, 'index.pug'), output_html, html_context)


def write_html(template_file, html_out, vars):
    """Given a template in `template_file`, format it in the Pug language using
    `vars` as context variables, and write the resulting HTML to `html_out`.
    """
    with open(template_file) as f:
        template = f.read()
    
    parser = pypugjs.parser.Parser(template)
    block = parser.parse()
    compiler = pypugjs.ext.html.Compiler(block)
    compiler.global_context = vars
    with open(html_out, 'w') as f:
        f.write(compiler.compile())


if __name__ == '__main__':
    main()
