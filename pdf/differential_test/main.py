#! /usr/bin/env python3

import base64
import collections
import imageio
import io
import numpy as np
import os
import scipy
import shutil
import subprocess
import sys
import tempfile

RMSE_WINDOW_SIZE = 50
RMSE_FOR_DIFFERENTIAL = 30.  # Out of 255
DIFF_FOR_DIFFERENTIAL = -12.  # Negative means disabled


def main():
    artifacts_root_dir = sys.argv[1]
    temp_dir = sys.argv[2]
    html_out = len(sys.argv) > 3 and sys.argv[3] == '--html'

    img_attrs = 'width="400"'

    tool_names = []
    pages_by_tool = {}
    for t in sorted(os.listdir(artifacts_root_dir)):
        tool_names.append(t)
        for f in os.listdir(os.path.join(artifacts_root_dir, t)):
            # Filename format is `page-%d.ext`
            pageno = int(f.rsplit('.')[0].rsplit('-')[-1])
            assert (t, pageno) not in pages_by_tool, f'{t} / {pageno} / {pages_by_tool}'
            pages_by_tool[(t, pageno)] = f
    if not tool_names:
        print(f'No upstream tools produce page images')
    if tool_names and not pages_by_tool:
        print(f'All upstream tools produced no output', file=sys.stderr)

    if html_out:
        print(
            '<!DOCTYPE html><html><title>Visual Differential</title><body><table border="1">'
            '<thead><tr><th>Page</th>'
        )
        for t in tool_names:
            print(f'<th>{t}</th>')
        print('</thead><tbody>')

    pageno = 1
    rmse_max = collections.defaultdict(lambda: collections.defaultdict(float))
    diff_max = collections.defaultdict(lambda: collections.defaultdict(float))
    any_differential = False
    while True:
        existed = set()

        if html_out:
            print(f'<tr><td>Page&nbsp;{pageno}</td>')

        page_images = []
        for ti, t in enumerate(tool_names):
            page_images.append(None)  # append placeholder

            pagename = pages_by_tool.get((t, pageno))
            if pagename is None:
                if html_out:
                    print(f'<td>{t} did not render this page</td>')
                continue
            existed.add(pagename)

            pagepath = os.path.join(artifacts_root_dir, t, pagename)
            # TODO requires conversion (imagemagick installed on machine)
            # // testing before commit.
            if not pagepath.endswith('.png'):
                new_pagepath = os.path.join(temp_dir, f'{t}-{pageno}.png')
                subprocess.check_call(['convert', pagepath, new_pagepath])
                pagepath = new_pagepath

            if html_out:
                b64 = base64.b64encode(open(pagepath, 'rb').read())
                b64 = b64.decode('latin1')
                print(f'<td><img src="data:image/png;base64,{b64}" {img_attrs} />')

            try:
                img = imageio.v2.imread(pagepath)
            except ValueError:
                if not html_out:
                    # Report this error.
                    print(f'Unable to read image!', file=sys.stderr)
                    raise
                print(f'Unable to read image!</td>')
                continue
            img = img.astype(np.float32)
            if len(img.shape) < 3:
                # Image was interpreted by imageio as monochrome
                img = np.atleast_3d(img)
            elif img.shape[2] == 4:
                # Image is RGBA; discard alpha channel.
                print(f'Discarded transparency from {t} page {pageno}')
                # alpha to white; equivalent to `img = img[:, :, :3]` for opaque images
                img = img[:, :, :3] * np.atleast_3d(img[:, :, 3]) / 255
            if img.shape[2] == 1:
                # Image was grayscale; convert to RGB for visual comparison
                img = np.broadcast_to(img, (*img.shape[:2], 3))
            page_images[-1] = img  # replace placeholder (None)

            if html_out and ti > 0:
                print(f'<table><tr>')

            for bt, base_tool in enumerate(tool_names[:ti]):
                base_img = page_images[bt]
                if base_img is None:
                    if html_out:
                        print(f'<td>No image for {base_tool}</td>')
                    continue
                diff, definitely_differential = img_diff(pageno, base_img,
                        base_tool, img, t, html_out=html_out)
                if definitely_differential:
                    any_differential = True

                if html_out:
                    print(f'<td>')
                    if diff is not None:
                        buf = io.BytesIO()
                        diff_rgb = np.broadcast_to(255 - diff * 255, (*diff.shape[:2], 3)).astype(np.uint8)
                        imgdat = imageio.v2.imwrite(buf, diff_rgb, 'png')
                        buf.seek(0)
                        b64 = base64.b64encode(buf.read()).decode('latin1')
                        print(f'<img src="data:image/png;base64,{b64}" {img_attrs} />')
                    print(f'<br />vs {base_tool}')

                if diff is not None:
                    # Abs needed to prevent rounding errors
                    rmse = abs(scipy.ndimage.uniform_filter(
                            (diff ** 2).mean(2),
                            size=(RMSE_WINDOW_SIZE, RMSE_WINDOW_SIZE))) ** 0.5
                    rmse = rmse.max()
                    rmse_max[base_tool][t] = max(rmse, rmse_max[base_tool][t])

                    diff_max_inst = diff.max()
                    diff_max[base_tool][t] = max(diff_max_inst, diff_max[base_tool][t])

                    # RMSE's units are pixels difference; remember, black is 0
                    # and white is 255.
                    print(f'Page {pageno} RMSE from {t} to {base_tool} was {rmse:.4f}',
                            file=sys.stderr)
                    print(f'Page {pageno} max diff from {t} to {base_tool} was {diff_max_inst:.4f}',
                            file=sys.stderr)
                    if rmse > RMSE_FOR_DIFFERENTIAL and diff_max > DIFF_FOR_DIFFERENTIAL:
                        print(f'Differential: page {pageno} from {t} to {base_tool}',
                                file=sys.stderr)
                        any_differential = True

                    if html_out:
                        print(f'<br/>RMSE {rmse:.4f}, max diff {diff.max():.4f}')

                if html_out:
                    print(f'</td>')
            if html_out:
                if ti > 0:
                    print(f'</tr></table>')
                print('</td>')

        if html_out:
            print('</tr>')

        if not existed:
            break

        if len(tool_names) != len(existed):
            print(f'Differential: page {pageno} only existed in some tools: {existed}',
                    file=sys.stderr)
            any_differential = True
        pageno += 1

    if html_out:
        print('</tbody></table>')
        if any_differential:
            print('Differential pages detected; see HTML report above.')
        print('</body></html>')

    for bt, bt_tools in rmse_max.items():
        for ot, rmse in bt_tools.items():
            print(f'Max RMSE, {bt} to {ot}: {rmse:.4f}', file=sys.stderr)
    for bt, bt_tools in diff_max.items():
        for ot, rmse in bt_tools.items():
            print(f'Max diff, {bt} to {ot}: {rmse:.4f}', file=sys.stderr)


def img_diff(pageno, base, base_tool, img, img_tool, html_out):
    """Returns: diff img, definitely differential
    """

    # Can we fix rounding differences?
    if img.shape != base.shape and (
            abs(img.shape[0] - base.shape[0]) < 2
            and abs(img.shape[1] - base.shape[1]) < 2
            and img.shape[2] == base.shape[2]):
        if img.shape[0] < base.shape[0]:
            img = np.concatenate((img, np.zeros((base.shape[0] - img.shape[0], img.shape[1], img.shape[2]))), 0)
        elif img.shape[0] > base.shape[0]:
            img = img[:base.shape[0]]
        if img.shape[1] < base.shape[1]:
            img = np.concatenate((img, np.zeros((img.shape[0], base.shape[1] - img.shape[1], img.shape[2]))), 1)
        elif img.shape[1] > base.shape[1]:
            img = img[:, :base.shape[1]]

    diff = None
    if base.shape != img.shape:
        print(f'Differential: page {pageno} was size '
                f'{base.shape} in {base_tool}, and '
                f'{img.shape} in {img_tool}', file=sys.stderr)

        if html_out:
            print(f'<br />Size mismatch: {base.shape} != {img.shape}')
        return None, True

    def hipass(i):
        r = i.copy()
        r[:-1, :-1] -= r[1:, 1:]
        # Hipass has invalid borders
        return r[:-1, :-1]
    bb, ii = [base, img]

    # pdftoppm sometimes renders a border wrong. Other renderers
    # probably do as well. So, take off border pixels.
    bb = bb[1:-1, 1:-1]
    ii = ii[1:-1, 1:-1]

    if True:
        # Align via cv2, per https://alexanderpacha.com/2018/01/29/aligning-images-an-engineers-solution/
        # Some PDFs do have alignment issues, particularly when
        # the whole page is an image. So, this is important,
        # even though it inflates processing time.
        import cv2
        warp_mode = cv2.MOTION_AFFINE
        warp_matrix = np.eye(2, 3, dtype=np.float32)
        num_iter = 10
        threshold_eps = 1e-7
        criteria = (cv2.TERM_CRITERIA_EPS | cv2.TERM_CRITERIA_COUNT,
                num_iter, threshold_eps)
        def grayscale(i):
            if i.shape[2] == 1:
                # image already grayscale
                return i
            return i[:, :, 0] * .3 + i[:, :, 1] * .59 + i[:, :, 2] * .11
        try:
            (cc, warp_matrix) = cv2.findTransformECC(grayscale(bb), grayscale(ii),
                    warp_matrix, warp_mode, criteria, inputMask=None,
                    gaussFiltSize=5)
        except cv2.error:
            # Usually happens when page is all-white
            print(f'Page {pageno} failed to find alignment: cv2.error',
                    file=sys.stderr)
            if html_out:
                print(f'No page alignment: cv2.error <br/>')
        else:
            ii = np.atleast_3d(  # cv2 drops the last dimension if it's 1
                cv2.warpAffine(
                    ii, warp_matrix, (ii.shape[1], ii.shape[0]),
                    flags=cv2.INTER_LINEAR | cv2.WARP_INVERSE_MAP,
                    borderMode=cv2.BORDER_REPLICATE
                )
            )

    if True:
        sz_blur = 2.
        sz_med = 5
        sz_var_max = 9

        iig = ii
        bbg = bb

        iig = hipass(iig)
        bbg = hipass(bbg)

        iig = scipy.ndimage.gaussian_filter(iig, sigma=(sz_blur, sz_blur, 0))
        bbg = scipy.ndimage.gaussian_filter(bbg, sigma=(sz_blur, sz_blur, 0))

        if False:
            # Median filter -- from before hipass preceding other filters
            if False:
                med_ii = scipy.ndimage.median_filter(iig, size=(sz_med, sz_med, 1))
                med_bb = scipy.ndimage.median_filter(bbg, size=(sz_med, sz_med, 1))
            else:
                # A true median filter is ridiculously expensive for large sizes.
                # So, try a cheap one which only gets a bead on the page color.
                med_ii = np.median(iig, axis=(0, 1)).reshape((1, 1, ii.shape[2]))
                med_bb = np.median(bbg, axis=(0, 1)).reshape((1, 1, ii.shape[2]))

            iig -= med_ii
            bbg -= med_bb

        iig_max = scipy.ndimage.maximum_filter(abs(iig), size=(sz_var_max, sz_var_max, 1))
        bbg_max = scipy.ndimage.maximum_filter(abs(bbg), size=(sz_var_max, sz_var_max, 1))

        s = lambda a, b: (10 + a) / (10 + b)

        diff = s(iig, iig_max) - s(bbg, bbg_max)

        #diff = hipass(diff)
        diff = abs(diff)
        return diff, False
    elif base is not None and True:
        # Experimental covariance filter

        if False:
            # Use box test to bloom one image into the other, to match
            # as closely as possible. Fixes antialiasing differences.
            if True:
                filt_sz = (3, 3, 1)
                ii_mn = scipy.ndimage.minimum_filter(ii,
                        size=filt_sz)
                ii_mx = scipy.ndimage.maximum_filter(ii,
                        size=filt_sz)
            else:
                ii_gauss = scipy.ndimage.gaussian_filter(ii,
                        sigma=2.)
                ii_mn = np.minimum(ii_gauss, ii)
                ii_mx = np.maximum(ii_gauss, ii)
            ii = np.minimum(ii_mx, np.maximum(ii_mn, bb))

        filt_kw = {'sigma': 2.5}
        def covar(img):
            #mn = scipy.ndimage.uniform_filter(img,
            #        size=filt_sz)
            # Reading is all about high contrast -- therefore, use
            # a median rather than gaussian filter to determine the
            # "background color" for each covariance.
            mn = scipy.ndimage.median_filter(img,
                    size=(21, 21, 1))

            # Clean up covariance due to aliasing via a blur filter
            img = scipy.ndimage.gaussian_filter(img,
                    sigma=2.)
            dev = img - mn

            mag = dev * dev
            return mn, dev, mag

            mag = np.clip(scipy.ndimage.gaussian_filter(dev * dev,
                    **filt_kw), 0, None) ** 0.5
            return mn, dev, mag

        bb_mn, bb_dev, bb_mag = covar(bb)
        ii_mn, ii_dev, ii_mag = covar(ii)

        dev_eps = 1.  # Out of 255 * 255

        #diff = (dev_eps + bb_dev * ii_dev) / (dev_eps + np.maximum(bb_mag, ii_mag))
        mix = bb_dev * ii_dev
        mix_max = scipy.ndimage.maximum_filter(mix, size=(21, 21, 1))
        diff = (dev_eps + mix) / (dev_eps + mix_max)
        diff = abs(1. - diff)

        diff = abs(hipass(diff))
        # Hipass has invalid borders
        diff = diff[:-1, :-1]
        return diff, False

        diff = scipy.ndimage.gaussian_filter(bb_dev * ii_dev,
                **filt_kw)
        dev_eps = 1000.00 # Max is 255*255.
        diff /= dev_eps + bb_mag * ii_mag

        diff = abs(1. - diff) * np.maximum(bb_mag, ii_mag) + abs(bb_mn - ii_mn) / 255.
        diff = abs(hipass(diff))

        # hipass' far borders are invalid.
        diff = diff[:-1, :-1]

        return diff, False


    elif base is not None and True:
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

        if False:
            # Align via cv2, per https://alexanderpacha.com/2018/01/29/aligning-images-an-engineers-solution/
            # Doesn't really seem to help, compared to box difference filter
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

        # Box difference filter, bloom iii toward bb, and vice-versa
        # for a symmetrical filter.
        # This is a pretty large-scale feature, and will certainly
        # miss text replacement. Would need OCR for that.
        filt_sz = 5
        img_mn = scipy.ndimage.minimum_filter(iii,
                size=(filt_sz, filt_sz, 1))
        img_mx = scipy.ndimage.maximum_filter(iii,
                size=(filt_sz, filt_sz, 1))
        b_mn = scipy.ndimage.minimum_filter(bb,
                size=(filt_sz, filt_sz, 1))
        b_mx = scipy.ndimage.maximum_filter(bb,
                size=(filt_sz, filt_sz, 1))

        # Important! Aliasing and DPI mean that not all renderers
        # will reach the same color for e.g. a black border on a
        # white background. So, scale up the variance of each range
        # a bit.
        for mn, mx in [(img_mn, img_mx), (b_mn, b_mx)]:
            mean = (mn + mx) * 0.5
            beta = 3.
            mn += (mn - mean) * beta
            mx += (mx - mean) * beta

        diff_box = np.maximum(
                np.maximum(
                    img_mn - bb,
                    bb - img_mx),
                np.maximum(
                    b_mn - iii,
                    iii - b_mx))
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
            img_blur = scipy.ndimage.gaussian_filter(img,
                    blur_sz * 0.5)
            base_blur = scipy.ndimage.gaussian_filter(base,
                    blur_sz * 0.5)
            diff_blur = abs(img_blur - base_blur)

        # Box difference filter
        img_mn = scipy.ndimage.minimum_filter(img,
                size=(filt_sz, filt_sz, 1))
        img_mx = scipy.ndimage.maximum_filter(img,
                size=(filt_sz, filt_sz, 1))
        base_mn = scipy.ndimage.minimum_filter(base,
                size=(filt_sz, filt_sz, 1))
        base_mx = scipy.ndimage.maximum_filter(base,
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


class TempDir:
    def __enter__(self):
        self.d = tempfile.mkdtemp()
        return self.d
    def __exit__(self, exc_type, exc_val, tb):
        shutil.rmtree(self.d)


if __name__ == '__main__':
    main()

