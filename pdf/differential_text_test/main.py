#! /usr/bin/env python3

import html.parser
import os
import re
from spacy.lang.en import English
import sys
from typing import AbstractSet, Optional

## Setup NLP
_nlp = English()
_tokenizer = _nlp.tokenizer


def normalize_text(b: Optional[bytes]) -> Optional[AbstractSet[str]]:
    if b is None:
        return None
    v = b.decode(encoding='utf-8', errors='replace')
    v = v.strip()

    # This doesn't appear to be what Kudu does (https://github.com/kududyn/safedocs/blob/master/pdftools/sparclur/sparclur/_text_extractor.py)
    # , but spacy interprets differing newlines as different characters.
    v = re.sub(r'\s+', ' ', v)
    # Also, some PDFs interpret as e.g. 4| vs |4, which is annoying. So...
    v = re.sub(r'\W+', ' ', v)
    # Tokensize / stem maybe
    list_v = [str(t) for t in _tokenizer(v)]
    # Shingle / make a set (skipped when shingle_size is 1)
    shingle_size = 1
    if shingle_size > 1 and len(v) >= shingle_size:
        list_v = [
            ', '.join(g for g in v[i:i + shingle_size])
            for i in range(len(v) - shingle_size + 1)
        ]
    return frozenset(list_v)


def main():
    artifacts_root_dir = sys.argv[1]
    html_out = len(sys.argv) > 2 and sys.argv[2] == '--html'

    if html_out:
        print(f'<!DOCTYPE html><html><title>Text Differential</title><body>')

    tool_names = []
    page_text_by_tool = {}
    for t in os.listdir(artifacts_root_dir):
        tool_names.append(t)
        # The artifact dir has a single file with all pages separated by FF
        page_files = os.listdir(os.path.join(artifacts_root_dir, t))
        if not page_files:
            print(f'{t} produced no output')
            if html_out:
                print('<br />')
        elif len(page_files) == 1:
            # combined output
            with open(os.path.join(artifacts_root_dir, t, page_files[0]), 'rb') as file:
                pages = file.read().split(b'\f')
                if pages and not pages[-1].strip():  # trailing FF
                    pages.pop()
                for pageno, page in enumerate(pages):
                    page_text_by_tool[(t, pageno)] = normalize_text(page)
        else:
            print(f'{t} produced multiple output files: {page_files}')
    tool_names.sort()
    if not tool_names:
        print(f'No upstream tools produce page text')

    min_similarity = {}

    page_index = 0
    while True:
        pages = [page_text_by_tool.get((k, page_index)) for k in tool_names]
        if all([p is None for p in pages]):
            break

        eps = 1  # Some bias for div zero

        if html_out:
            print(f'<p>Page {page_index}<table><thead><tr><td></td>')
            for ti, t in enumerate(tool_names):
                print(f'<td>')
                #print(t)
                if pages[ti] is not None:
                    print(f'<details><summary>{t}</summary>{html.escape(str(sorted(pages[ti])))}</details>')
                else:
                    print(f'{t} (not rendered)')
                print(f'</td>')
            print('</tr></thead><tbody>')
        else:
            print(f'Page {page_index}')

        for i in range(len(tool_names)-1):
            if html_out:
                print(f'<tr><td>{tool_names[i]}</td>')
                for j in range(0, i+1):
                    print(f'<td></td>')

            for j in range(i+1, len(tool_names)):
                if pages[i] is None or pages[j] is None:
                    # Automatic differential -- missing page
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
            print(f'</tbody></table></p>')
        page_index += 1

    if html_out:
        print(f'</body></html>')
    else:
        for k, v in sorted(min_similarity.items(), key=lambda m: m[0]):
            print(f'Min {k}: {v:.3f}')


if __name__ == '__main__':
    main()

