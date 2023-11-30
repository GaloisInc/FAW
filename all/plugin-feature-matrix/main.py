import heapq
import html
from typing import Dict, List, DefaultDict, Any
import collections
import ujson as json
import numpy as np
import numpy.typing as npt
import sys
import typer


MAX_FEATURES_TO_SHOW = 512

HTML_TEMPLATE = """
<!DOCTYPE html>\n<html>
<head><title>FAW Clustering plugin</title>
<style lang="text/css">
{css}
</style>
</head>
<body>
{body_html}
</body>
</html>
"""

CSS = """
thead th {
    writing-mode: sideways-lr;
    width: 1.25em;
    vertical-align: bottom;
}
th {
    text-align: start;
}
td {
    text-align: center;
    width: 1.25em;
}
"""


def main(output_html: str):

    filename_to_index: Dict[str, int] = {}
    filenames: List[str] = []
    file_indices_by_feature: DefaultDict[str, List[int]] = collections.defaultdict(list)
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj: Dict[str, Any] = json.loads(line)
        filename = obj.pop('_id')
        file_index = len(filename_to_index)
        filenames.append(filename)
        filename_to_index[filename] = file_index

        for feature, feature_value in obj.items():
            file_indices_by_feature[feature].append(file_index)

    # Drop very uncommon or common features. Only keep the most interesting 1000 features
    num_features = len(file_indices_by_feature)
    num_files = len(filenames)
    if num_features > MAX_FEATURES_TO_SHOW:
        file_indices_by_feature = dict(
            heapq.nlargest(
                MAX_FEATURES_TO_SHOW,
                file_indices_by_feature.items(),
                key=lambda p: num_files - abs(len(p[1]) - num_files),
            )
        )

    features = list(file_indices_by_feature.keys())
    feature_to_index: Dict[str, int] = {
        feature: i for i, feature in enumerate(features)
    }
    feature_files: npt.NDArray[np.bool_] = np.zeros(
        (len(features), len(filename_to_index)), dtype=np.bool_
    )
    for feature, file_indices in file_indices_by_feature.items():
        feature_files[feature_to_index[feature], file_indices] = 1

    body_html = f'''
    <p>Dropped the {num_features - MAX_FEATURES_TO_SHOW} most common and uncommon features.</p>
    <p>Showing {len(features)} features over {num_files} files.</p>
    <table><thead>
    <tr>{''.join(f'<th>{html.escape(filename)}</th>' for filename in filename_to_index)}</tr>
    </thead>
    <tbody>
    {''.join(
        f"""<tr>{"".join(
                f'<td style="background-color:{"#666" if x else "#bbb"}">{x:d}</td>'
                for x in row
            )
        }<th>{html.escape(feature)}</th></tr>""" for feature, row in sorted(zip(features, feature_files)))
    }
    </tbody>
    </table>
    '''

    with open(output_html, 'w') as f:
        f.write(HTML_TEMPLATE.format(
            body_html=body_html,
            css=CSS,
        ))


if __name__ == '__main__':
    typer.run(main)

