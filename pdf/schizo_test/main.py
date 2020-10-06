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

DPI = 100
RMSE_FOR_SCHIZO = 0.4  # Out of 255

def main():
    fname = sys.argv[1]
    html_out = len(sys.argv) > 2 and sys.argv[2] == '--html'

    res = str(DPI)
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
        rmse_max = 0.
        any_schizo = False
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

                    # Ok, a blur filter captures gross differences, whereas
                    # a box-difference filter captures high-frequency
                    # differences while ignoring aliasing.

                    # The box-difference on its own is very susceptible to
                    # font kerning differences. Modulating the box-difference
                    # filter by the blur filter addresses this shortcoming.

                    # (Cannot just use blur, as modified text wouldn't show,
                    # and we still want the high-pass filter for minor color
                    # space differences)
                    blur_sz = 40
                    img_blur = scipy.ndimage.filters.gaussian_filter(img,
                            blur_sz * 0.5)
                    base_blur = scipy.ndimage.filters.gaussian_filter(base,
                            blur_sz * 0.5)
                    diff_blur = abs(img_blur - base_blur)

                    # Box difference filter
                    img_mn = scipy.ndimage.filters.minimum_filter(img,
                            size=(filt_sz, filt_sz, 1))
                    img_mx = scipy.ndimage.filters.maximum_filter(img,
                            size=(filt_sz, filt_sz, 1))
                    base_mn = scipy.ndimage.filters.minimum_filter(base,
                            size=(filt_sz, filt_sz, 1))
                    base_mx = scipy.ndimage.filters.maximum_filter(base,
                            size=(filt_sz, filt_sz, 1))
                    diff_box = np.maximum(
                            img_mn - base_mx,
                            base_mn - img_mx)
                    diff_box = np.clip(diff_box, 0., None)
                    # High-pass filter
                    diff_box[:-1, :-1] -= diff_box[1:, 1:]
                    diff_box[-1, :] = diff_box[:, -1] = 0.
                    diff_box = np.clip(diff_box, 0., None)

                    diff = diff_box * diff_blur / 255.
                    #diff = diff_blur

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
                    rmse_max = max(rmse, rmse_max)

                    # RMSE's units are pixels difference; remember, black is 0
                    # and white is 255.
                    print(f'Page {pageno} RMSE from {t} to {base_tool} was {rmse:.4f}',
                            file=sys.stderr)
                    if rmse > RMSE_FOR_SCHIZO:
                        print(f'Schizophrenic: page {pageno} from {t} to {base_tool}',
                                file=sys.stderr)
                        any_schizo = True

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
                any_schizo = True
            pageno += 1

        if html_out:
            print('</table>')
            if any_schizo:
                print('Schizophrenic pages detected; see HTML report above.')
            print('</body></html>')

        print(f'Max RMSE: {rmse_max:.4f}', file=sys.stderr)


class TempDir:
    def __enter__(self):
        self.d = tempfile.mkdtemp()
        return self.d
    def __exit__(self, exc_type, exc_val, tb):
        shutil.rmtree(self.d)


if __name__ == '__main__':
    main()

