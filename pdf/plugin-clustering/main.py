import base64
import json
import numpy as np
from sklearn.cluster import AgglomerativeClustering
import sys
import typer

def main(workbench_api_url: str, json_arguments: str, output_html: str):

    # Load parameters
    dec_args = json.loads(json_arguments)

    # Compute sparse features
    file_to_idx = {}
    ft_to_idx = {}
    file_ft = []
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj = json.loads(line)
        file_name = obj.pop('_id')
        file_idx = len(file_to_idx)
        file_to_idx[file_name] = file_idx

        for k, v in obj.items():
            if k.startswith('ml-test'):
                # Filter out these for now... need to change to user-specified
                # filter.
                continue

            ft_idx = ft_to_idx.get(k)
            if ft_idx is None:
                ft_idx = len(ft_to_idx)
                ft_to_idx[k] = ft_idx

            file_ft.append((file_idx, ft_idx))

    X = np.zeros((len(file_to_idx), len(ft_to_idx)))
    X_nonzero = np.asarray(file_ft, dtype=int)
    X[X_nonzero[:, 0], X_nonzero[:, 1]] = 1.

    # X is (file, ft) at the moment. We actually want to find features which
    # are the same.
    X = X.T

    # And we want each dim to be zero mean, unit std s.t. euclidean distance
    # takes rarity into account.
    X -= X.mean(1, keepdims=True)
    X /= X.std(1, keepdims=True) + 1e-30

    model = AgglomerativeClustering(
            distance_threshold=0.,
            n_clusters=None
            ).fit(X)

    def plot_dendrogram(model, labels=None, **kwargs):
        import io
        from scipy.cluster.hierarchy import dendrogram

        counts = np.zeros(model.children_.shape[0])
        n_samples = len(model.labels_)
        for i, merge in enumerate(model.children_):
            current_count = 0
            for child_idx in merge:
                if child_idx < n_samples:
                    current_count += 1
                else:
                    current_count += counts[child_idx - n_samples]
            counts[i] = current_count

        linkage_matrix = np.column_stack([model.children_, model.distances_,
                counts]).astype(float)
        return linkage_matrix

    labels = [None for _ in range(len(ft_to_idx))]
    for k, v in ft_to_idx.items():
        labels[v] = k
    linkage_matrix = plot_dendrogram(model, labels=labels,
            orientation='right',
            leaf_font_size=6,
            #distance_sort=True,  Already done
            truncate_mode='lastp',
    )

    with open(output_html, 'w') as f:
        f.write('<!DOCTYPE html>\n<html>')
        f.write('<head><title>FAW Clustering plugin</title>')
        f.write('<script>')
        with open('./vue.js') as f_vue:
            f.write(f_vue.read())
        f.write('\n</script>')
        f.write(r'''<style lang="text/css">
                .dendrogram {
                    display: inline-block;
                    border: solid 1px #888;
                    border-bottom: none;
                }
                .dendrogram .leaf {
                    width: 300px;
                    white-space: pre-wrap;
                }
                </style>''')
        f.write('<script>')
        data = {
                'linkage': linkage_matrix.tolist(),
                'labels': labels,
        }
        f.write(f'window.data = {json.dumps(data)};\n')
        f.write(r'''
                Vue.component('dendrogram', {
                    template: `<div class="dendrogram">
                        <div v-if="leafNodeIs" class="header leaf">{{leafLabel}}</div>
                        <div v-else-if="childDistance === 0" class="header leaf">
                            <!-- special case -- everything here matches perfectly -->
                            <ul>
                                <li v-for="v of getChildLabels()">{{v}}</li>
                            </ul>
                        </div>
                        <div v-else-if="collapsed" @click="collapsed = !collapsed" class="header">
                            Collapsed ({{childCount}})
                        </div>
                        <table v-else>
                            <tr>
                                <td></td>
                                <td class="header" @click="collapsed = !collapsed">
                                    Cluster with {{childCount}} children.
                                </td>
                                <td></td>
                            </tr>
                            <tr>
                                <td>
                                    <dendrogram
                                            :id="childLeft" :linkage="linkage"
                                            :labels="labels" />
                                </td>
                                <td>
                                    Distance: {{childDistance}}
                                </td>
                                <td>
                                    <dendrogram
                                            :id="childRight" :linkage="linkage"
                                            :labels="labels" />
                                </td>
                            </tr>
                        </table>
                    </div>`,
                    props: {
                        labels: Object,
                        linkage: Object,
                        id: Number,
                    },
                    data() {
                        return {
                            collapsed: true,
                            expandLeft: false,
                            expandRight: false,
                            expandLabels: false,
                        };
                    },
                    computed: {
                        childCount() {
                            return this.linkage[this.id - this.labels.length][3];
                        },
                        childLeft() {
                            return this.linkage[this.id - this.labels.length][0];
                        },
                        childRight() {
                            return this.linkage[this.id - this.labels.length][1];
                        },
                        childDistance() {
                            return this.linkage[this.id - this.labels.length][2];
                        },
                        leafLabel() {
                            return this.labels[this.id];
                        },
                        leafNodeIs() {
                            return this.id < this.labels.length;
                        },
                    },
                    methods: {
                        getChildLabels() {
                            const r = [];
                            let stack = [this.id];
                            while (stack.length > 0) {
                                let i = stack.pop();
                                if (i < this.labels.length) {
                                    r.push(this.labels[i]);
                                }
                                else {
                                    i -= this.labels.length;
                                    stack.push(this.linkage[i][0]);
                                    stack.push(this.linkage[i][1]);
                                }
                            }
                            return r;
                        },
                    },
                });
        ''')
        f.write('\n</script>')
        f.write('</head>')
        f.write('<body>')
        #f.write(f'<img src="data:image/png;base64,{base64.b64encode(img).decode()}" />')
        f.write('<div id="app"><dendrogram :id="labels.length + linkage.length - 1" :linkage="linkage" :labels="labels" /></div>')
        f.write("<script>let app = new Vue({el: '#app', data: window.data})</script>")
        f.write('</body>')
        f.write('</html>')


if __name__ == '__main__':
    typer.run(main)

