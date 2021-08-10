import base64
import json
import numpy as np
import scipy.sparse
from sklearn.cluster import AgglomerativeClustering
import sys
import typer

def main(workbench_api_url: str, json_arguments: str, output_html: str):

    # Load parameters
    dec_args = json.loads(json_arguments)

    # Set defaults
    debug_str = ''
    dec_args.setdefault('dependent', True)
    dec_args.setdefault('linkage', 'complete')
    dec_args.setdefault('min_samples', 2)
    dec_args.setdefault('feature', '')
    dec_args.setdefault('feature_search', '')

    # Compute sparse features
    file_to_idx = {}
    ft_to_idx = {}
    file_ft = []

    # Filter out features which appear only one time, as these are not very
    # informative and skew the statistics
    ft_holding = {}  # {feature: file idx set}
    min_samples = dec_args['min_samples']
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj = json.loads(line)
        file_name = obj.pop('_id')
        file_idx = len(file_to_idx)
        file_to_idx[file_name] = file_idx

        for k, v in obj.items():
            ft_idx = ft_to_idx.get(k)
            if ft_idx is None:
                fh_set = ft_holding.get(k)
                if fh_set is None:
                    # Hold for sure
                    ft_holding[k] = set([file_idx])
                else:
                    if len(fh_set) < min_samples:
                        # Stay holding?
                        fh_set.add(file_idx)
                    else:
                        # Release this feature
                        ft_idx = len(ft_to_idx)
                        ft_to_idx[k] = ft_idx

                        for f in fh_set:
                            file_ft.append((f, ft_idx))

                        del ft_holding[k]

                        # Fall through to add current file

            if ft_idx is not None:
                file_ft.append((file_idx, ft_idx))

    labels = [None for _ in range(len(ft_to_idx))]
    for k, v in ft_to_idx.items():
        labels[v] = k

    if True:
        # NEW CODE -- only one feature at a time.
        X = scipy.sparse.dok_matrix((len(ft_to_idx), len(file_to_idx)),
                dtype=int)
        X_nonzero = np.asarray(file_ft, dtype=int)
        X[X_nonzero[:, 1], X_nonzero[:, 0]] = 1
        label_counts = np.asarray(X.sum(1))[:, 0]

        if dec_args['feature']:
            # Attributable risk, but only for ft of interest
            i = ft_to_idx[dec_args['feature']]

            N = X.shape[1]
            X_sum = X.sum(1)
            eps = 1e-20
            X_joint = X[i] @ X.T
            X_b = (
                    X_joint / (X_sum.T + eps)
                    - (X_sum[i] - X_joint) / (N - X_sum.T + eps))
            X_a = (
                    X_joint / (X_sum[i] + eps)
                    - (X_sum.T - X_joint) / (N - X_sum[i] + eps))

            X_a = np.asarray(X_a)
            X_b = np.asarray(X_b)
            u = (abs(X_a) < abs(X_b))
            if dec_args['dependent']:
                u = 1 - u
            X = X_a * u + (1 - u) * X_b
            X = 1 - X
        else:
            X = np.asmatrix([[]])
    elif False:
        # OLD CODE
        X = np.zeros((len(file_to_idx), len(ft_to_idx)))
        X_nonzero = np.asarray(file_ft, dtype=int)
        X[X_nonzero[:, 0], X_nonzero[:, 1]] = 1.

        # X is (file, ft) at the moment. We actually want to find features which
        # are the same.
        X = X.T
        label_counts = X.sum(1).astype(int)

        if False:
            # Compute cosine similarity using zero-mean vectors
            X -= X.mean(1, keepdims=True)
            X /= (X ** 2).sum(1, keepdims=True) ** 0.5 + 1e-30
            X = 1. - (X @ X.T)
        elif True:
            # Predictive power.... 0 means no correlation, 1 means synchronized...
            # Want 1 - P(knowing B tells you something about A)
            # P(A|B) vs P(A|~B)... there's no way it's that simple?

            # https://en.wikipedia.org/wiki/Relative_risk_reduction
            # Risk reduction methodology. Specifically, "attributable risk"

            # X is (ft, file)
            N = X.shape[1]
            X_sum = X.sum(1, keepdims=True)
            eps = 1e-20
            X_b = (
                    # P(A|B)
                    (X @ X.T) / (X_sum.T + eps)
                    # P(A|~B)
                    - (X @ (1 - X.T)) / (N - X_sum.T + eps)
            )
            X_a = (
                    (X @ X.T) / (X_sum + eps)
                    - ((1 - X) @ X.T) / (N - X_sum + eps))

            # Default is independence, which lets us peruse highly related features.
            # Auxiliary is dependence, which lets us peruse "implies" relationships
            u = (abs(X_a) < abs(X_b))
            if dec_args['dependent']:
                # Max dependence
                u = 1 - u
            X = X_a * u + (1 - u) * X_b

            X = 1 - X
        else:
            # Compute 1 - min(P(A|B))
            X_sum = X.sum(1, keepdims=True)
            X = (X @ X.T) / np.maximum(X_sum, X_sum.T)
            X = 1. - X

        if False:
            # Debug code to inspect distances
            fts = ['CalledProcessError', "'<' not supported"]
            idx = [-1 for _ in fts]
            for k, v in ft_to_idx.items():
                for fti, ft in enumerate(fts):
                    if ft in k:
                        assert idx[fti] < 0, 'Duplicate for ' + ft
                        idx[fti] = v

            debug_str = str(X[np.asarray(idx)][:, np.asarray(idx)])

        if False:
            # Summarize
            X = X[:50, :50]
            labels = labels[:50]
            label_counts = label_counts[:50]

        # NOTE -- tried HDBSCAN, ran in about 2x the time as scikit-learn's
        # AgglomerativeClustering. Also, had worse results (on this linkage tree
        # problem).
        # Linkage = 'single' means min between cluster and new candidate point;
        # 'complete' means max.
        print(f'About to hierarchically cluster on {X.shape}', file=sys.stderr)
        import time
        cluster_start = time.time()

        model = AgglomerativeClustering(
                affinity='precomputed', linkage=dec_args['linkage'],
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
                'debug_str': debug_str,
                'distance_matrix': X.tolist(),
                #'linkage': linkage_matrix.tolist(),
                'labels': labels,
                'label_counts': label_counts.tolist(),
                'api_url': workbench_api_url,
                'dec_args': dec_args,
        }
        f.write(f'window.data = {json.dumps(data)};\n')
        f.write(r'''
                let dEpsilon = 1e-8;
                function callRedecide(args) {
                    let argsNew = Object.assign({}, window.data.dec_args, args);

                    let req = new XMLHttpRequest();
                    let url = window.data.api_url + 'redecide';
                    req.open('post', url, true);
                    req.setRequestHeader('Content-Type', 'application/json');
                    req.send(JSON.stringify(argsNew));
                }
                Vue.component('dendrogram', {
                    template: `<div class="dendrogram">
                        <div v-if="leafNodeIs" class="header leaf">{{leafLabel}}</div>
                        <div v-else-if="childDistance < dEpsilon" class="header leaf">
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
                                            :labels="labels"
                                            :label-counts="labelCounts" />
                                </td>
                                <td>
                                    <span v-if="false">Distance: {{childDistance}}</span>
                                    <span v-else>Similarity: {{1 - childDistance}}</span>
                                </td>
                                <td>
                                    <dendrogram
                                            :id="childRight" :linkage="linkage"
                                            :labels="labels"
                                            :label-counts="labelCounts" />
                                </td>
                            </tr>
                        </table>
                    </div>`,
                    props: {
                        labels: Object,
                        labelCounts: Object,
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
                            return this.getLabelFor(this.id);
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
                                    r.push(this.getLabelFor(i));
                                }
                                else {
                                    i -= this.labels.length;
                                    stack.push(this.linkage[i][0]);
                                    stack.push(this.linkage[i][1]);
                                }
                            }
                            return r;
                        },
                        getLabelFor(i) {
                            return `[${this.labelCounts[i]}] - ${this.labels[i]}`;
                        },
                    },
                });

                Vue.component('vue-header', {
                    template: `<div class="header">
                        <div v-if="debugStr.length">DEBUG: {{debugStr}}</div>
                        <div>Similarities shown are absolute attributable risk; using '{{decArgs.dependent ? 'IMPLIES' : 'AND'}}' relationships with '{{decArgs.linkage}}' linkage</div>
                        <div><input type="button" :value="'Switch to ' + (decArgs.dependent ? 'AND' : 'IMPLIES')" @click="callRedecide({dependent: !decArgs.dependent})" /></div>
                        <div><input type="button" :value="'Switch to ' + (decArgs.linkage === 'single' ? 'complete' : 'single') + ' linkage'" @click="callRedecide({linkage: decArgs.linkage === 'single' ? 'complete' : 'single'})" /></div>
                    </div>`,
                    props: {
                        debugStr: String,
                        decArgs: Object,
                    },
                });

                // Direct version, not going through cluster
                Vue.component('similarity-search-direct', {
                    template: `<div class="similarity-search">
                        Search phrase (as regex): <input v-model="search" type="text" /> <input v-model="searchCaseSensitive" type="checkbox" /> Case sensitive
                        <ul class="search-results">
                            <li v-for="v of searchList" @click="featureChange(v)">
                                <span v-if="v !== -1">[{{labelCounts[v]}}] {{labels[v]}} </span>
                                <span v-else>(truncated)</span>
                            </li>
                        </ul>
                        <p>Related features
                            <ul v-if="searchActual >= 0" class="similar-results">
                                <li v-for="v of results">
                                    <span @click="featureChange(v)">{{(1 - searchDistance(v)).toFixed(3)}} [{{labelCounts[v]}}] {{labels[v]}}</span>
                                </li>
                                <li>
                                    <input v-if="searchNext" type="button" value="More" @click="resultAddNext()" />
                                </li>
                            </ul>
                        </p>
                    </div>
                    `,
                    props: {
                        distances: Object,
                        feature: String,
                        featureSearchInit: String,
                        labels: Object,
                        labelCounts: Object,
                    },
                    data() {
                        return {
                            results: [],
                            search: this.featureSearchInit,
                            searchActual: -1,
                            searchCaseSensitive: false,
                            searchList: [],
                            searchListMax: 10,
                            searchNext: null,
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
                    },
                    mounted() {
                        this.searchUpdate();

                        if (this.feature.length === 0) return;

                        this.searchActual = this.labels.indexOf(this.feature);

                        // Compute searchNext
                        this.searchNext = (this.distances[0]
                                .map((x, idx) => [x, idx])
                                .sort((a, b) => a[0] - b[0])
                                .map(x => x[1]));
                        this.resultAddNext();
                    },
                    methods: {
                        featureChange(v) {
                            callRedecide({feature: this.labels[v],
                                    feature_search: this.search});
                        },
                        searchDistance(i) {
                            return this.distances[0][i];
                        },
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
                            let i = Math.min(this.searchNext.length, 10);
                            this.results.push.apply(this.results, this.searchNext.splice(0, i));
                        },
                    },
                });
        ''')
        f.write('\n</script>')
        f.write('</head>')
        f.write('<body>')
        #f.write(f'<img src="data:image/png;base64,{base64.b64encode(img).decode()}" />')
        f.write('<div id="app">If you see this, Vue is loading or broken</div>')
        f.write(r'''<script>let app = new Vue({el: '#app', data: Object.freeze(window.data),
                template: `<div>
                    <vue-header :dec-args="data.dec_args" :debug-str="debug_str" />
                    <!-- <similarity-search :linkage="linkage" :labels="labels" :label-counts="label_counts" /> -->
                    <similarity-search-direct :feature="data.dec_args.feature"
                            :feature-search-init="data.dec_args.feature_search"
                            :distances="distance_matrix" :labels="labels" :label-counts="label_counts" />
                    <!-- <dendrogram :id="labels.length + linkage.length - 1" :linkage="linkage" :labels="labels" :label-counts="label_counts" :start-uncollapsed="true" /> -->
                </div>`})</script>''')
        f.write('</body>')
        f.write('</html>')


if __name__ == '__main__':
    typer.run(main)

