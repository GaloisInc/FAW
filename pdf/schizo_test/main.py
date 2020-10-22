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
RMSE_WINDOW_SIZE = 50
RMSE_FOR_SCHIZO = 30.  # Out of 255
DIFF_FOR_SCHIZO = -12.  # Negative means disabled

tools = [
        # File prefix must always be f'{name}-{page}.ext'
        # Can optionally take 'env'
        # Lambda function takes output directory
        # '<inputFile>' replaced with input file name.
        {
            'name': 'mutool',
            'exec': ['mutool', 'draw', '-r', str(DPI), '-o',
                lambda dname: os.path.join(dname, 'mutool-%d.png'),
                '<inputFile>'],
        },
        {
            'name': 'pdftoppm',
            'exec': ['pdftoppm', '-png', '-r', str(DPI), '<inputFile>',
                lambda dname: os.path.join(dname, 'pdftoppm')],
        },
]

def main():
    fname = sys.argv[1]
    html_out = len(sys.argv) > 2 and sys.argv[2] == '--html'

    tool_names = [tool['name'] for tool in tools]

    res = str(DPI)
    img_attrs = 'width="400"'
    with TempDir() as dname:
        for t in tools:
            ex = t['exec']
            ex = [e if e != '<inputFile>' else fname for e in ex]
            ex = [e if not callable(e) else e(dname) for e in ex]
            env = os.environ.copy()
            env.update(t.get('env', {}))
            subprocess.check_call(ex, env=env, stdout=subprocess.PIPE,
                    stderr=subprocess.PIPE)

        if html_out:
            print('<!DOCTYPE html><html><title>Schizo Test</title><body><table>')

        pages_by_tool = {}
        for f in os.listdir(dname):
            for t in tool_names:
                prefix = f'{t}-'
                if not f.startswith(prefix):
                    continue
                pageno = int(f[len(prefix):].split('.')[0])
                assert (t, pageno) not in pages_by_tool, f'{t} / {pageno} / {pages_by_tool}'
                pages_by_tool[(t, pageno)] = f

        pageno = 1
        all_rmse_max = 0.
        all_diff_max = 0.
        any_schizo = False
        while True:
            existed = set()
            base = None
            base_tool = None

            if html_out:
                print(f'<tr><td>Page {pageno}</td>')

            for t in tool_names:
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
                img = img.astype(np.float32)

                # Can we fix rounding differences?
                if base is not None and img.shape != base.shape and (
                        abs(img.shape[0] - base.shape[0]) < 2
                        and abs(img.shape[1] - base.shape[1]) < 2
                        and img.shape[2] == base.shape[2]):
                    if img.shape[0] < base.shape[0]:
                        img = np.concat((img, np.zeros(base.shape[0] - img.shape[0], img.shape[1], img.shape[2])), 0)
                    elif img.shape[0] > base.shape[0]:
                        img = img[:base.shape[0]]
                    if img.shape[1] < base.shape[1]:
                        img = np.concat((img, np.zeros(img.shape[0], base.shape[1] - img.shape[1], img.shape[2])), 1)
                    elif img.shape[1] > base.shape[1]:
                        img = img[:, :base.shape[1]]

                diff = None
                if base is not None and base.shape != img.shape:
                    print(f'Schizophrenic: page {pageno} was size '
                            f'{base.shape} in {base_tool}, and '
                            f'{img.shape} in {t}', file=sys.stderr)
                    any_schizo = True

                    if html_out:
                        print(f'<br />Size mismatch: {base.shape} != {img.shape}')
                elif base is not None and False:
                    # Experimental filter, archived here. Production one is
                    # below.

                    # For both images, do a high pass filter.
                    def preproc(i):
                        r = i.copy()
                        r[:-1, :-1] -= r[1:, 1:]
                        return r
                    bb, ii = [base, img]

                    def grayscale(i):
                        return i[:, :, 0]

                    # Align via cv2, per https://alexanderpacha.com/2018/01/29/aligning-images-an-engineers-solution/
                    # Doesn't really seem to help, compared to box difference filter
                    if False:
                        import cv2
                        warp_mode = cv2.MOTION_AFFINE
                        warp_matrix = np.eye(2, 3, dtype=np.float32)
                        num_iter = 100
                        threshold_eps = 1e-7
                        criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT,
                                num_iter, threshold_eps)
                        try:
                            (cc, warp_matrix) = cv2.findTransformECC(grayscale(bb), grayscale(ii),
                                    warp_matrix, warp_mode, criteria, inputMask=None,
                                    gaussFiltSize=5)
                        except cv2.error:
                            if html_out:
                                print(f'No page alignment: cv2.error <br/>')
                        iii = cv2.warpAffine(ii, warp_matrix, (img.shape[1], img.shape[0]),
                                flags=cv2.INTER_LINEAR | cv2.WARP_INVERSE_MAP,
                                borderMode=cv2.BORDER_REPLICATE)
                    else:
                        iii = ii

                    # Box difference filter, bloom iii toward bb
                    filt_sz = 9
                    img_mn = scipy.ndimage.filters.minimum_filter(iii,
                            size=(filt_sz, filt_sz, 1))
                    img_mx = scipy.ndimage.filters.maximum_filter(iii,
                            size=(filt_sz, filt_sz, 1))

                    diff_box = np.maximum(
                            img_mn - bb,
                            bb - img_mx)
                    diff_box = np.clip(diff_box, 0., None)

                    diff_box = abs(preproc(diff_box))

                    diff = diff_box  # abs(bb - iii)
                elif base is not None:
                    filt_sz = 2

                    # Ok, a blur filter captures gross differences, whereas
                    # a box-difference filter captures high-frequency
                    # differences while ignoring aliasing.

                    # The box-difference on its own is very susceptible to
                    # font kerning differences. Modulating the box-difference
                    # filter by the blur filter addresses this shortcoming.

                    # (Cannot just use blur, as modified text wouldn't show,
                    # and we still want the high-pass filter for minor color
                    # space differences)
                    blur_sz = DPI
                    if False:
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

                    u = 0.8
                    for arr in [img_mn, img_mx]:
                        arr *= u
                        arr += (1 - u) * img
                    for arr in [base_mn, base_mx]:
                        arr *= u
                        arr += (1 - u) * base

                    diff_box = np.maximum(
                            img_mn - base_mx,
                            base_mn - img_mx)
                    diff_box = np.clip(diff_box, 0., None)

                    # High-pass filter
                    diff_box[:-1, :-1] -= diff_box[1:, 1:]
                    diff_box[-1, :] = diff_box[:, -1] = 0.
                    diff_box = abs(diff_box)

                    dw = 1
                    bw = 1
                    #diff = diff_box ** dw * diff_blur ** bw * 255 / 255 ** dw / 255. ** bw
                    diff = diff_box
                    #diff = diff_blur

                if html_out:
                    if diff is not None:
                        buf = io.BytesIO()
                        imgdat = imageio.imwrite(buf, 255 - diff,
                                'png')
                        buf.seek(0)
                        b64 = base64.b64encode(buf.read()).decode('latin1')
                        print(f'<img src="data:image/png;base64,{b64}" {img_attrs} />')
                    print(f'<br />{t}')

                if diff is None:
                    base = img
                    base_tool = t
                else:
                    # Abs needed to prevent rounding errors
                    rmse = abs(scipy.ndimage.filters.uniform_filter(
                            (diff ** 2).mean(2),
                            size=(RMSE_WINDOW_SIZE, RMSE_WINDOW_SIZE))) ** 0.5
                    rmse = rmse.max()
                    all_rmse_max = max(rmse, all_rmse_max)

                    diff_max = diff.max()
                    all_diff_max = max(diff_max, all_diff_max)

                    # RMSE's units are pixels difference; remember, black is 0
                    # and white is 255.
                    print(f'Page {pageno} RMSE from {t} to {base_tool} was {rmse:.4f}',
                            file=sys.stderr)
                    print(f'Page {pageno} max diff from {t} to {base_tool} was {diff_max:.4f}',
                            file=sys.stderr)
                    if rmse > RMSE_FOR_SCHIZO and diff_max > DIFF_FOR_SCHIZO:
                        print(f'Schizophrenic: page {pageno} from {t} to {base_tool}',
                                file=sys.stderr)
                        any_schizo = True

                    if html_out:
                        print(f', RMSE {rmse:.4f}')
                        print(f', max diff {diff.max():.4f}')
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

        print(f'Max RMSE: {all_rmse_max:.4f}', file=sys.stderr)
        print(f'Max diff: {all_diff_max:.4f}', file=sys.stderr)


class TempDir:
    def __enter__(self):
        self.d = tempfile.mkdtemp()
        return self.d
    def __exit__(self, exc_type, exc_val, tb):
        shutil.rmtree(self.d)


if __name__ == '__main__':
    main()

