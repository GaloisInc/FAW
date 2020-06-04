"""
This is a more complicated plugin, which plots results using a Vue-based
interface.

The Vue project was re-configured to produce a single output file, s.t.
vue.config.js contains:

    module.exports = {
      configureWebpack: {
        optimization: {
          splitChunks: false
        }
      },
      css: {
        extract: false,
      }
    }

Test data was inserted directly into vue-ui/public/index.html, and gets
overwritten by this file. This was done so that the Vue UI (run `npm start` in
vue-ui) may still be used to develop the app.

Additionally, for convenience, the Vue template was switched around a bit.
"""

import json, os, re, sys

_path = os.path.dirname(os.path.abspath(__file__))

def main():
    _, output_html, api_url, args = sys.argv
    args = json.loads(args)

    vue_files = []
    vue_fts = {'files': vue_files, 'apiUrl': api_url}

    for line in sys.stdin:
        obj = json.loads(line)
        exit_codes = [k for k in obj.keys() if '<<workbench: Exit code' in k]
        vue_files.append({'testfile': obj['_id'], 'fts': exit_codes})

        # See if all returned 0
        all_ok = True
        for e in exit_codes:
            if 'Exit code 0' not in e:
                all_ok = False

        # Make a decision about this file
        print(json.dumps({
            'testfile': obj['_id'],
            'status': 'valid' if all_ok else 'rejected',
        }))


    with open(output_html, 'w') as f:
        f.write(_collect_vue(vue_fts))


def _collect_vue(args):
    vue_dist = os.path.join(_path, 'vue-ui', 'dist')
    if not os.path.lexists(vue_dist):
        raise ValueError("To use this plugin, the developer putting the workbench "
                "image together needs to install node.js and run 'npm run build' "
                "in pdf/vue_plugin/vue-ui.")

    fname = None
    for f in os.listdir(os.path.join(vue_dist, 'js')):
        if f.endswith('.js'):
            fname = os.path.join(vue_dist, 'js', f)
            break
    else:
        raise ValueError('Could not find built vue .js file?')

    # Import the index.html file, and splice in the javascript file
    html = open(os.path.join(vue_dist, 'index.html')).read()
    js = open(fname).read()

    re_html = re.sub(
            r'<script src=.+?/script>',
            # Lambda important to avoid interpreting escapes in js
            lambda m: ''.join(['\n<script type="text/javascript">\n',
                f'window.workbenchArgs={json.dumps(args)};\n',
                js,
                '\n</script>']),
            html)

    if False:
        # Debugging output on stderr will be printed in workbench's console.
        # Otherwise, something like "docker exec -it <container> /bin/bash" can
        # be used to read temp files.

        # FIXME something fails about long debug prints inside docker,
        # which is probably related to https://github.com/travis-ci/travis-ci/issues/4704#issuecomment-348435959
        # but for now just write to a temp file
        with open('/tmp/example.html', 'w') as f:
            f.write(re_html)
    return re_html


if __name__ == '__main__':
    main()
