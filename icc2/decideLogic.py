
import html
import json
import sys

def main():
    print(sys.argv)
    _, output_html = sys.argv

    files = []
    prop_counts = {}

    for l in sys.stdin:
        f = json.loads(l)
        for k in f.keys():
            prop_counts[k] = prop_counts.get(k, 0) + 1
        files.append(f)

    for f in files:
        print(json.dumps({
                'testfile': f['_id'],
        }))

    pk = sorted(prop_counts.items(), key=lambda m: -m[-1])

    with open(output_html, 'w') as f:
        f.write('<!DOCTYPE html>\n<html><body>\n')
        f.write('Done. "Elbow" messages: <br/>')
        j = None
        jn = 0
        for p in pk:
            if j != p[-1]:
                jn = 0
                j = p[-1]
                f.write(f'<h1>Seen in {p[-1]} files</h1><br />')
            if jn < 5:
                f.write(f'{html.escape(p[0])} <br/>')
                jn += 1
        f.write('\n</body></html>')


if __name__ == '__main__':
    main()

