#! /usr/bin/env python3

import base64
import imageio
import io
import os
import scipy.ndimage
import shutil
import subprocess
import sys
import tempfile

RMSE_FOR_SCHIZO = 15.  # Out of 255

def main():
    fname = sys.argv[1]
    html_out = len(sys.argv) > 2 and sys.argv[2] == '--html'

    res = '100'  # DPI
    with TempDir() as dname:
        tools = ['mutool', 'pdftocairo']

        subprocess.call(
                ['mutool', 'draw', '-r', res, '-o',
                    os.path.join(dname, 'mutool-%d.png'), fname],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                )
        subprocess.call(
                ['pdftocairo', '-png', '-r', res, fname,
                    os.path.join(dname, 'pdftocairo')],
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                )

        if html_out:
            print('<!DOCTYPE html><html><title>Schizo Test</title><body><table>')

        pageno = 1
        while True:
            existed = set()
            base = None
            base_tool = None

            if html_out:
                print(f'<tr><td>Page {pageno}</td>')

            for t in tools:
                pagename = f'{t}-{pageno}.png'
                pagepath = os.path.join(dname, pagename)
                if not os.path.lexists(pagepath):
                    if html_out:
                        print(f'<td>{t} did not render this page</td>')
                    continue
                existed.add(pagename)

                if html_out:
                    b64 = base64.b64encode(open(pagepath, 'rb').read())
                    b64 = b64.decode('latin1')
                    print(f'<td><img src="data:image/png;base64,{b64}" />')

                img = imageio.imread(pagepath)
                img = img.astype(float)

                diff = None
                if base is not None:
                    # "diff" only exists if base has been set!
                    diff = abs(img - base)

                    # Text aliasing effects are annoying -- be optimistic about
                    # local diffs.
                    filt_sz = 2
                    diff = scipy.ndimage.filters.minimum_filter(diff,
                            size=(filt_sz, filt_sz, 1))

                if html_out:
                    if diff is not None:
                        buf = io.BytesIO()
                        imgdat = imageio.imwrite(buf, 255 - diff, 'png')
                        buf.seek(0)
                        b64 = base64.b64encode(buf.read()).decode('latin1')
                        print(f'<img src="data:image/png;base64,{b64}" />')
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
                print('Schizophrenic: page {pageno} only existed in some tools: {existed}',
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

