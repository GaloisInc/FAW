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
            if k.startswith('ml-test') or k.startswith('plugin-rl-grit'):
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
    label_counts = X.sum(1)

    # And we want each dim to be zero mean, unit std s.t. euclidean distance
    # takes rarity into account.
    X -= X.mean(1, keepdims=True)
    X /= X.std(1, keepdims=True) + 1e-30

    labels = [None for _ in range(len(ft_to_idx))]
    for k, v in ft_to_idx.items():
        labels[v] = k

    if False:
        # Summarize
        X = X[:50, :50]
        labels = labels[:50]
        label_counts = label_counts[:50]

    # NOTE -- tried HDBSCAN, ran in about 2x the time as scikit-learn's
    # AgglomerativeClustering. Also, had worse results (on this linkage tree
    # problem).
    print(f'About to hierarchically cluster on {X.shape}', file=sys.stderr)
    import time
    cluster_start = time.time()

    model = AgglomerativeClustering(
            distance_threshold=0.,
            n_clusters=None,
            ).fit(X)

    def plot_dendrogram(model):
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

    linkage_matrix = plot_dendrogram(model)
    print(f'...done! After {time.time() - cluster_start:.1f}s', file=sys.stderr)

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
                .dendrogram .collapsible {
                    cursor: pointer;
                }
                </style>''')
        f.write('<script>')
        data = {
                'linkage': linkage_matrix.tolist(),
                'labels': labels,
                'label_counts': label_counts.tolist(),
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
                        <div v-else-if="collapsed" @click="collapsed = !collapsed" class="header collapsible">
                            Collapsed ({{childCount}})
                        </div>
                        <table v-else>
                            <tr>
                                <td></td>
                                <td class="header collapsible" @click="collapsed = !collapsed">
                                    Cluster with {{childCount}} features.
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
                        startUncollapsed: Boolean,
                    },
                    data() {
                        return {
                            collapsed: !this.startUncollapsed,
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

                Vue.component('similarity-search', {
                    template: `<div class="similarity-search">
                        Search phrase (as regex): <input v-model="search" type="text" /> <input v-model="searchCaseSensitive" type="checkbox" /> Case sensitive
                        <ul class="search-results">
                            <li v-for="v of searchList" @click="searchActual = v">
                                <span v-if="v !== -1">{{labels[v]}}  [{{labelCounts[v]}}]</span>
                                <span v-else>(truncated)</span>
                            </li>
                        </ul>
                        <ul v-if="searchActual >= 0" class="similar-results">
                            <li v-for="v of results">
                                <span v-if="typeof v === 'number'" @click="searchActual = v">{{labels[v]}}  [{{labelCounts[v]}}]</span>
                                <span v-else style="font-weight: bold">Distance: {{v.distance}}</span>
                            </li>
                            <li>
                                <input v-if="searchNext" type="button" value="More" @click="resultAddNext()" />
                            </li>
                            <li>
                                <input type="button" value="Clear" @click="searchActual = -1" />
                            </li>
                        </ul>
                    </div>
                    `,
                    props: {
                        labels: Object,
                        labelCounts: Object,
                        linkage: Object,
                    },
                    data() {
                        const linkageReverse = {};
                        let n = this.labels.length;
                        for (let i = 0, m = this.linkage.length; i < m; i++) {
                            linkageReverse[this.linkage[i][0]] = n + i;
                            linkageReverse[this.linkage[i][1]] = n + i;
                        }
                        return {
                            linkageReverse: linkageReverse,
                            results: [],
                            search: '',
                            searchActual: -1,
                            searchCaseSensitive: false,
                            searchList: [],
                            searchListMax: 10,
                            searchNext: null,
                            searchNextDistance: 0,
                            searchTimeout: null,
                        };
                    },
                    watch: {
                        search() {
                            this.searchTimeout !== null && clearTimeout(this.searchTimeout);
                            this.searchTimeout = setTimeout(() => this.searchUpdate(), 300);
                        },
                        searchCaseSensitive() {
                            this.searchTimeout !== null && clearTimeout(this.searchTimeout);
                            this.searchTimeout = setTimeout(() => this.searchUpdate(), 300);
                        },
                        searchActual() {
                            this.results = [];
                            if (this.searchActual === -1) {
                                this.searchNext = null;
                                return;
                            }

                            this.results.push(this.searchActual);

                            this.searchNextDistance = 0;
                            this.searchNext = this.searchActual;
                            while (this.searchNextDistance === 0) {
                                this.resultAddNext();
                            }
                        },
                    },
                    methods: {
                        searchUpdate() {
                            this.searchList = [];
                            const flags = (this.searchCaseSensitive ? '' : 'i');
                            const re = new RegExp(this.search, flags);
                            for (let i = 0, m = this.labels.length; i < m; i++) {
                                if (!re.test(this.labels[i])) continue;

                                this.searchList.push(i);
                                if (this.searchList.length >= this.searchListMax) {
                                    this.searchList.push(-1);
                                    break;
                                }
                            }
                        },
                        /** Adds the sibling of `node`, all of its descendents,
                            and the parent. Returns parent. */
                        resultAddNext() {
                            const node = this.searchNext;
                            const p = this.linkageReverse[node];
                            if (p === undefined) {
                                // Done traversing
                                return;
                            }
                            const pInd = p - this.labels.length;

                            if (this.linkage[pInd][2] > this.searchNextDistance) {
                                this.searchNextDistance = this.linkage[pInd][2];
                                this.results.push({distance: this.searchNextDistance});
                            }

                            const stack = [];
                            if (this.linkage[pInd][0] === node) {
                                stack.push(this.linkage[pInd][1]);
                            }
                            else {
                                stack.push(this.linkage[pInd][0]);
                            }

                            while (stack.length > 0) {
                                let pp = stack.pop();
                                if (pp < this.labels.length) {
                                    this.results.push(pp);
                                    continue;
                                }

                                pp -= this.labels.length;
                                stack.push(this.linkage[pp][0]);
                                stack.push(this.linkage[pp][1]);
                            }

                            this.searchNext = p;
                        },
                    },
                });
        ''')
        f.write('\n</script>')
        f.write('</head>')
        f.write('<body>')
        #f.write(f'<img src="data:image/png;base64,{base64.b64encode(img).decode()}" />')
        f.write('<div id="app">If you see this, Vue is loading or broken</div>')
        f.write(r'''<script>let app = new Vue({el: '#app', data: window.data,
                template: `<div>
                    <similarity-search :linkage="linkage" :labels="labels" :label-counts="label_counts" />
                    <dendrogram :id="labels.length + linkage.length - 1" :linkage="linkage" :labels="labels" :start-uncollapsed="true" />
                </div>`})</script>''')
        f.write('</body>')
        f.write('</html>')


if __name__ == '__main__':
    typer.run(main)

