#! /usr/bin/env python3

import base64
import imageio
import io
import numpy as np
import os
import scipy.ndimage
import shutil
import subprocess
import sys
import tempfile

RMSE_FOR_SCHIZO = 2.  # Out of 255

def main():
    fname = sys.argv[1]
    html_out = len(sys.argv) > 2 and sys.argv[2] == '--html'

    res = '100'  # DPI
    img_attrs = 'width="400"'
    with TempDir() as dname:
        tools = ['mutool', 'pdftocairo']

        subprocess.check_call(
                ['mutool', 'draw', '-r', res, '-o',
                    os.path.join(dname, 'mutool-%d.png'), fname],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                )
        subprocess.check_call(
                ['pdftocairo', '-png', '-r', res, fname,
                    os.path.join(dname, 'pdftocairo')],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                )

        if html_out:
            print('<!DOCTYPE html><html><title>Schizo Test</title><body><table>')

        pages_by_tool = {}
        for f in os.listdir(dname):
            for t in tools:
                prefix = f'{t}-'
                if not f.startswith(prefix):
                    continue
                pageno = int(f[len(prefix):].split('.')[0])
                assert (t, pageno) not in pages_by_tool, f'{t} / {pageno} / {pages_by_tool}'
                pages_by_tool[(t, pageno)] = f

        pageno = 1
        while True:
            existed = set()
            base = None
            base_tool = None

            if html_out:
                print(f'<tr><td>Page {pageno}</td>')

            for t in tools:
                pagename = pages_by_tool.get((t, pageno))
                if pagename is None:
                    if html_out:
                        print(f'<td>{t} did not render this page</td>')
                    continue
                existed.add(pagename)

                pagepath = os.path.join(dname, pagename)
                if html_out:
                    b64 = base64.b64encode(open(pagepath, 'rb').read())
                    b64 = b64.decode('latin1')
                    print(f'<td><img src="data:image/png;base64,{b64}" {img_attrs} />')

                try:
                    img = imageio.imread(pagepath)
                except ValueError:
                    if not html_out:
                        # Report this error.
                        print(f'Unable to read image!', file=sys.stderr)
                        raise
                    print(f'Unable to read image!</td>')
                    continue
                img = img.astype(float)

                diff = None
                if base is not None:
                    filt_sz = 3
                    if False:
                        # Old, erode filter
                        # "diff" only exists if base has been set!
                        diff = abs(img - base)

                        # Text aliasing effects are annoying -- be optimistic about
                        # local diffs.
                        diff = scipy.ndimage.filters.minimum_filter(diff,
                                size=(filt_sz, filt_sz, 1))
                    else:
                        # New, box-difference filter. Handles aliasing better.
                        img_mn = scipy.ndimage.filters.minimum_filter(img,
                                size=(filt_sz, filt_sz, 1))
                        img_mx = scipy.ndimage.filters.maximum_filter(img,
                                size=(filt_sz, filt_sz, 1))
                        base_mn = scipy.ndimage.filters.minimum_filter(base,
                                size=(filt_sz, filt_sz, 1))
                        base_mx = scipy.ndimage.filters.maximum_filter(base,
                                size=(filt_sz, filt_sz, 1))

                        if True:
                            # Post-processing. Box-difference with filt_sz 2
                            # missed some alignment errors, but filt_sz 3
                            # is overly forgiving, as some letters are less than
                            # 2px wide. Therefore, do a soft min/max based on
                            # current pixel value with variance explained by
                            # local min/max.
                            u = 0.85
                            img_mn *= u
                            img_mx *= u
                            base_mn *= u
                            base_mx *= u

                            uu = 1 - u
                            img_mn += uu * img
                            img_mx += uu * img
                            base_mn += uu * base
                            base_mx += uu * base

                        diff = np.maximum(
                                img_mn - base_mx,
                                base_mn - img_mx)
                        diff = np.clip(diff, 0., None)

                if html_out:
                    if diff is not None:
                        buf = io.BytesIO()
                        imgdat = imageio.imwrite(buf, 255 - diff, 'png')
                        buf.seek(0)
                        b64 = base64.b64encode(buf.read()).decode('latin1')
                        print(f'<img src="data:image/png;base64,{b64}" {img_attrs} />')
                    print(f'<br />{t}')
                if base is None:
                    base = img
                    base_tool = t
                else:
                    rmse = (diff ** 2).mean() ** 0.5

                    # RMSE's units are pixels difference; remember, black is 0
                    # and white is 255.
                    print(f'Page {pageno} RMSE from {t} to {base_tool} was {rmse:.4f}',
                            file=sys.stderr)
                    if rmse > RMSE_FOR_SCHIZO:
                        print(f'Schizophrenic: page {pageno} from {t} to {base_tool}',
                                file=sys.stderr)

                    if html_out:
                        print(f', RMSE {rmse:.4f}')
                if html_out:
                    print('</td>')

            if html_out:
                print('</tr>')

            if not existed:
                break

            if len(tools) != len(existed):
                print(f'Schizophrenic: page {pageno} only existed in some tools: {existed}',
                        file=sys.stderr)
            pageno += 1

        if html_out:
            print('</table></body></html>')


class TempDir:
    def __enter__(self):
        self.d = tempfile.mkdtemp()
        return self.d
    def __exit__(self, exc_type, exc_val, tb):
        shutil.rmtree(self.d)


if __name__ == '__main__':
    main()

