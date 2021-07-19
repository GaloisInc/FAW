#! /usr/bin/env python3

import collections
import html.parser
import io
import os
import re
import requests
import shutil
from spacy.lang.en import English
import subprocess
import sys
import tempfile
import time

## Setup NLP
_nlp = English()
_tokenizer = _nlp.tokenizer

## Setup specific parsers
class ParserBase:
    disabled = False

    def run(self, fpath):
        """Return a list of [tokens found in fpath, pre-stemming
            for each page in document].

        May also return a dict of {page: tokens...}

        Should never fail. Not all parsers must return the same size array,
        though.
        """
        raise NotImplementedError()


    def util_get_stdout(self, cmd):
        """Returns (binary output, stderr)"""
        p = subprocess.Popen(cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        r = p.wait()
        return stdout, stderr


class GhostscriptToText(ParserBase):
    disabled = True

    def run(self, fpath):
        pages = {}

        d = tempfile.mkdtemp()
        try:
            stdout, stderr = self.util_get_stdout(['gs', '-sDEVICE=txtwrite',
                    f'-sOutputFile={os.path.join(d, "output%d.txt")}',
                    '-q', '-dNOPAUSE', '-dBATCH', fpath])

            for f in os.listdir(d):
                assert f.startswith('output') and f.endswith('.txt'), f
                page_num = int(f[6:-4])
                pages[page_num-1] = open(os.path.join(d, f)).read()
        finally:
            shutil.rmtree(d)

        return pages, stderr.decode(errors='replace')
class MupdfToText(ParserBase):
    def run(self, fpath):
        stdout, stderr = self.util_get_stdout(['mutool', 'draw', '-F', 'text',
                '-o', '-', fpath])
        return [t.decode(errors='replace') for t in stdout.split(b'\x0c')[:-1]], stderr.decode(errors='replace')
class PdfBox(ParserBase):
    def run(self, fpath):
        pages = [[]]

        r = requests.put('http://localhost:9998/tika', data=None,
                headers={
                    'Content-Type': 'application/pdf',
                    'fileUrl': 'file://' + os.path.abspath(fpath),
                    'Accept': 'text/html'})
        stderr = requests.status_codes._codes[r.status_code][0]
        if r.status_code == requests.codes.ok:
            # Parse out pages. We use HTML here because otherwise, Tika doesn't
            # include page boundaries.
            class HtmlByPage(html.parser.HTMLParser):
                def __init__(self):
                    super().__init__()
                    self.stack = ['root']
                def handle_starttag(self, tag, attrs):
                    self.stack.append(tag)
                    if tag == 'div':
                        a = [v for v in attrs if v[0] == 'class']
                        if a and a[0][1] == 'page':
                            pages.append([])
                def handle_endtag(self, tag):
                    assert self.stack[-1] == tag, tag
                    self.stack.pop()
                def handle_data(self, data):
                    if self.stack[-1] == 'a':
                        # Do not include link content... other parsers seem to
                        # ignore these too
                        return
                    pages[-1].append(data)
            parser = HtmlByPage()
            parser.feed(r.text)
            assert parser.stack == ['root'], parser.stack

        # First page was catchall, and doesn't contain useful information
        return [''.join(p) for p in pages[1:]], stderr

        # OLD METHOD -- slow due to jar call
        pages = []
        # pdfbox has no page delimiters, and does not fail on an empty page.
        # So, look for N blank pages in a row
        n_blank = 0
        for page in range(1, 1000):
            stdout, stderr = self.util_get_stdout(['java', '-jar',
                    '/opt/pdfbox/pdfbox-app-2.0.17.jar',
                    'ExtractText',
                    '-encoding', 'UTF-8',
                    '-startPage', str(page),
                    '-endPage', str(page),
                    '-console',
                    fpath,
            ])
            stdout = stdout.decode(errors='replace')

            if not stdout:
                pages.append('')
                n_blank += 1
            else:
                pages.append(stdout)

            if n_blank >= 3:
                break

        return pages[:-n_blank], stderr.decode(errors='replace')
class PdfMinerToText(ParserBase):
    def run(self, fpath):
        # pdf2txt.py seems broken on a lot of files, oops
        stdout, stderr = self.util_get_stdout(['pdf2txt.py', fpath])
        # But dumppdf doesn't output the right format
        #stdout = self.util_get_stdout(['/usr/local/bin/dumppdf.py',
        #        '-a', '-t', fpath])
        return [t.decode(errors='replace') for t in stdout.split(b'\x0c')[:-1]], stderr.decode(errors='replace')
class PopplerPdfToText(ParserBase):
    def run(self, fpath):
        stdout, stderr = self.util_get_stdout(['bash', '-c',
                'pdftotext -enc UTF-8 $0 >(cat)', fpath])
        return [t.decode(errors='replace') for t in stdout.split(b'\x0c')[:-1]], stderr.decode(errors='replace')
class XpdfToText(ParserBase):
    def run(self, fpath):
        stdout, stderr = self.util_get_stdout(['bash', '-c',
                '/opt/xpdf/bin64/pdftotext -enc UTF-8 $0 >(cat)', fpath])
        return [t.decode(errors='replace') for t in stdout.split(b'\x0c')[:-1]], stderr.decode(errors='replace')

tools = [cls() for cls in ParserBase.__subclasses__() if not cls.disabled]

def run_parser(cls_instance, fpath):
    """"Run the given parser class and process output per page into proper,
    uniform buckets.
    """
    stime = time.monotonic()
    pages, stderr = cls_instance.run(fpath)
    if isinstance(pages, dict):
        # 0-based page index
        pass
    elif isinstance(pages, list):
        pages = {i: pages[i] for i in range(len(pages)) if pages[i] is not None}
    else:
        raise ValueError(pages)

    ## For each page....
    r = {'<stats>': {'runtime': time.monotonic() - stime, 'stderr': stderr}}
    for k, v in pages.items():
        if v is None:
            continue

        v = v.strip()

        # This doesn't appear to be what Kudu does (https://github.com/kududyn/safedocs/blob/master/pdftools/sparclur/sparclur/_text_extractor.py)
        # , but spacy interprets differing newlines as different characters.
        v = re.sub(r'\s+', ' ', v)
        # Also, some PDFs interpret as e.g. 4| vs |4, which is annoying. So...
        v = re.sub(r'\W+', ' ', v)
        # Tokensize / stem maybe
        v = [str(t) for t in _tokenizer(v)]
        # Shingle / make a set
        shingle_size = 1
        if shingle_size > 1 and len(v) >= shingle_size:
            v = [', '.join(g for g in v[i:i + shingle_size])
                    for i in range(len(v) - shingle_size + 1)]
        # Assign output
        r[k] = set(v)
    return r


def main():
    fname = sys.argv[1]
    html_out = len(sys.argv) > 2 and sys.argv[2] == '--html'

    if html_out:
        print(f'<!DOCTYPE html><html><title>Schizo Test</title><body>')

    tool_to_pages = {t.__class__.__name__: run_parser(t, fname) for t in tools}
    tool_names = sorted(tool_to_pages.keys())

    if html_out:
        print('<table>')
        for tn in tool_names:
            print(f'<tr><td>{tn}</td>')
            print(f'<td>{tool_to_pages[tn]["<stats>"]["runtime"]:.3f}s</td>')
            print(f'<td>{html.escape(tool_to_pages[tn]["<stats>"]["stderr"])}</td>')
            print(f'</tr>')
        print('</table>')

    min_similarity = {}

    page_index = -1
    while True:
        page_index += 1
        pages = [tool_to_pages[k].get(page_index) for k in tool_names]
        if all([p is None for p in pages]):
            break

        eps = 1  # Some bias for div zero

        if html_out:
            print(f'<p>Page {page_index}<table>')
            print(f'<tr><td></td>')
            for ti, t in enumerate(tool_names):
                print(f'<td>')
                #print(t)
                if pages[ti] is not None:
                    print(f'<details><summary>{t}</summary>{html.escape(str(sorted(pages[ti])))}</details>')
                else:
                    print(f'{t} (not rendered)')
                print(f'</td>')
            print('</tr>')
        else:
            print(f'Page {page_index}')

        for i in range(len(tool_names)-1):
            if html_out:
                print(f'<tr><td>{tool_names[i]}</td>')
                for j in range(0, i+1):
                    print(f'<td></td>')

            for j in range(i+1, len(tool_names)):
                if pages[i] is None or pages[j] is None:
                    # Automatic schizophrenia -- missing page
                    # Signify with value less than 0
                    jacard = -1
                else:
                    jacard = (eps + len(pages[i].intersection(pages[j]))) / (
                            eps + len(pages[i].union(pages[j])))

                desc = f'{tool_names[i]} to {tool_names[j]}'
                if jacard < min_similarity.get(desc, 1e30):
                    min_similarity[desc] = jacard

                if html_out:
                    print(f'<td>{jacard:.3f}</td>')
                else:
                    print(f'{desc}: {jacard:.3f}')

            if html_out:
                print(f'</tr>')

        if html_out:
            print(f'</table></p>')

    if html_out:
        print(f'</body></html>')
    else:
        for k, v in sorted(min_similarity.items(), key=lambda m: m[0]):
            print(f'Min {k}: {v:.3f}')


if __name__ == '__main__':
    main()

