#! /usr/bin/env python3

import collections
import html
import io
import os
import re
import shutil
from spacy.lang.en import English
import subprocess
import sys
import tempfile

## Setup NLP
_nlp = English()
_tokenizer = _nlp.tokenizer

## Setup specific parsers
class ParserBase:
    disabled = False

    def run(self, fpath):
        """Return a dict of {page: tokens found in fpath, pre-stemming}.

        Page numbers are 0-based."""
        raise NotImplementedError()


    def util_get_stdout(self, cmd):
        """Returns binary output"""
        p = subprocess.Popen(cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        r = p.wait()
        return stdout


class GhostscriptToText(ParserBase):
    disabled = True

    def run(self, fpath):
        pages = {}

        d = tempfile.mkdtemp()
        try:
            stdout = self.util_get_stdout(['gs', '-sDEVICE=txtwrite',
                    f'-sOutputFile={os.path.join(d, "output%d.txt")}',
                    '-q', '-dNOPAUSE', '-dBATCH', fpath])

            for f in os.listdir(d):
                assert f.startswith('output') and f.endswith('.txt'), f
                page_num = int(f[6:-4])
                pages[page_num-1] = open(os.path.join(d, f)).read()
        finally:
            shutil.rmtree(d)

        return pages
class MupdfToText(ParserBase):
    def run(self, fpath):
        stdout = self.util_get_stdout(['mutool', 'draw', '-F', 'text',
                '-o', '-', fpath])
        return [t.decode() for t in stdout.split(b'\x0c')]
class PdfMinerToText(ParserBase):
    def run(self, fpath):
        stdout = self.util_get_stdout(['pdf2txt.py', fpath])
        return [t.decode() for t in stdout.split(b'\x0c')]
class PopplerPdfToText(ParserBase):
    def run(self, fpath):
        stdout = self.util_get_stdout(['bash', '-c',
                'pdftotext -enc UTF-8 $0 >(cat)', fpath])
        return [t.decode() for t in stdout.split(b'\x0c')]
class XpdfToText(ParserBase):
    def run(self, fpath):
        stdout = self.util_get_stdout(['bash', '-c',
                '/opt/xpdf/bin64/pdftotext -enc UTF-8 $0 >(cat)', fpath])
        return [t.decode() for t in stdout.split(b'\x0c')]

tools = [cls() for cls in ParserBase.__subclasses__() if not cls.disabled]

def run_parser(cls_instance, fpath):
    """"Run the given parser class and process output per page into proper,
    uniform buckets.
    """
    pages = cls_instance.run(fpath)
    if isinstance(pages, dict):
        # 0-based page index
        pass
    elif isinstance(pages, list):
        pages = {i: pages[i] for i in range(len(pages)) if pages[i].strip()}
    else:
        raise ValueError(pages)

    ## For each page....
    r = {}
    for k, v in pages.items():
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
                    # Automatic schizophrenia
                    jacard = 0.
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

