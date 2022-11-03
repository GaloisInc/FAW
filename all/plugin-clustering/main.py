import base64
import ujson as json
import numpy as np
import re
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
    dec_args.setdefault('local_filter', '')
    dec_args.setdefault('local_filter_case_sensitive', False)
    dec_args.setdefault('min_samples', '')
    dec_args.setdefault('prefix_skip', '')
    dec_args.setdefault('prefix_max', '')
    dec_args.setdefault('feature', '')
    dec_args.setdefault('feature_search', '')

    try:
        min_samples = int(dec_args['min_samples'])
    except ValueError:
        min_samples = 2
        dec_args['min_samples'] = min_samples

    try:
        prefix_skip = int(dec_args['prefix_skip'])
    except ValueError:
        prefix_skip = 1
        dec_args['prefix_skip'] = prefix_skip
    try:
        prefix_max = int(dec_args['prefix_max'])
    except ValueError:
        prefix_max = 0
        dec_args['prefix_max'] = prefix_max

    # Compute sparse features
    file_to_idx = {}
    ft_count = {}  # {ft: set(file idx)}
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj = json.loads(line)
        file_name = obj.pop('_id')
        file_idx = len(file_to_idx)
        file_to_idx[file_name] = file_idx

        for kk, v in obj.items():
            kk_fts = set([kk])
            if '_' in kk:
                pfx, sfx = kk.split('_', 1)
                if prefix_max > 0:
                    for ki in range(prefix_skip, min(prefix_skip * (1+prefix_max), len(sfx) + prefix_skip), prefix_skip):
                        kk_fts.add(f'{pfx}_{sfx[:ki]}')

            for k in kk_fts:
                ft_c = ft_count.get(k)
                if ft_c is None:
                    ft_count[k] = set([file_idx])
                else:
                    ft_c.add(file_idx)

    # Throw out features with fewer than min_samples or more than N - min_samples.
    # This ensures that probabilities are calculated with sufficient granularity
    # to reduce false correlates.
    file_ft = []
    ft_to_idx = {}
    labels = []

    max_samples = len(file_to_idx) - min_samples
    for k, kset in ft_count.items():
        if max_samples >= len(kset) >= min_samples:
            labels.append(k)
            idx = ft_to_idx[k] = len(ft_to_idx)
            file_ft.extend([(f_idx, idx) for f_idx in kset])

    labels = [None for _ in range(len(ft_to_idx))]
    for k, v in ft_to_idx.items():
        labels[v] = k

    if True:
        # NEW CODE -- only one feature at a time.
        X = scipy.sparse.lil_matrix((len(ft_to_idx), len(file_to_idx)),
                dtype=int)
        X_nonzero = np.asarray(file_ft, dtype=int).reshape(-1, 2)
        X[X_nonzero[:, 1], X_nonzero[:, 0]] = 1
        X = X.tocsr()
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
                .pw {
                    white-space: pre-wrap;
                }
                input[type=button], summary {
                    cursor: pointer;
                }
                li.grid:nth-child(odd) {background-color: #eee;}
                </style>''')
        f.write('<script>')
        data = {
                'debug_str': debug_str,
                'distance_matrix': X.tolist(),
                #'linkage': linkage_matrix.tolist(),
                'labels': labels,
                'label_counts': label_counts.tolist(),
                'file_count': len(file_to_idx),
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
                            return `[${this.labelCounts[i]}] ${this.labels[i]}`;
                        },
                    },
                });

                Vue.component('vue-header', {
                    template: `<div class="header">
                        <div v-if="debugStr.length">DEBUG: {{debugStr}}</div>
                    </div>`,
                    props: {
                        debugStr: String,
                    },
                });

                // Direct version, not going through cluster
                Vue.component('similarity-search-direct', {
                    template: `<div class="similarity-search">
                        <div>Similarities shown are absolute attributable risk; using '{{decArgs.dependent ? 'IMPLIES' : 'AND'}}' relationships with '{{decArgs.linkage}}' linkage</div>
                        <div><input type="button" :value="'Switch to ' + (decArgs.dependent ? 'AND' : 'IMPLIES')" @click="featureChange(null, {dependent: !decArgs.dependent})" /></div>
                        <div><input type="button" :value="'Switch to ' + (decArgs.linkage === 'single' ? 'complete' : 'single') + ' linkage'" @click="featureChange(null, {linkage: decArgs.linkage === 'single' ? 'complete' : 'single'})" /></div>
                        <div>
                            Search phrase (as regex): <input v-model="search" type="text" /> <input v-model="searchCaseSensitive" type="checkbox" /> Case sensitive
                        </div>
                        <div>
                            Min samples for feature: <input v-model="minSamples" type="text" />
                        </div>
                        <div>
                            Prefix search: every <input v-model="prefixSkip" type="text" /> chars, max of <input v-model="prefixMax" type="text" /> copies
                        </div>
                        <div><input type="button" value="Re-apply above settings" @click="featureChange(null)" /></div>
                        <div>
                            Current settings yielded {{labels.length}} features from {{fileCount}} files
                        </div>
                        <ul class="search-results">
                            <template v-for="v of searchList">
                                <li class="grid" v-if="v !== -1">
                                    <input type="button" value="Focus" @click="featureChange(v)" />
                                    <span class="pw">{{getLabelFor(v)}}</span>
                                </li>
                                <li class="grid" v-else>
                                    <input type="button" value="More" @click="searchAddNext()" />
                                </li>
                            </template>
                        </ul>
                        <h3 class="pw">{{getLabelFor(searchActual)}}</h3>
                        <div>
                            <div style="display: flex; flex-direction: row;">
                                Only show matching regex:&nbsp;
                                <input type="text" v-model="localFilter" />
                                <input type="checkbox" v-model="localFilterCaseSensitive" /> Case sensitive
                            </div>
                        </div>
                        <details>
                            <summary>Related features, top N by parser; worst matches first</summary>
                            <ul v-if="searchActual >= 0" class="similar-results">
                                <li v-for="[klist, k] of resultsByParserSorted">
                                    {{k}}
                                    <ul>
                                        <li class="grid" v-for="v of klist">
                                            <input type="button" value="Focus" @click="featureChange(v)" />
                                            <span class="pw">{{searchDistance(v).toFixed(3)}} {{getLabelFor(v)}}</span>
                                        </li>
                                    </ul>
                                </li>
                            </ul>
                        </details>
                        <p>Related features, all {{results.length + searchNext.length}} matching
                            <ul v-if="searchActual >= 0" class="similar-results">
                                <li class="grid" v-for="v of results">
                                    <input type="button" value="Focus" @click="featureChange(v)" />
                                    <span class="pw">{{searchDistance(v).toFixed(3)}} {{getLabelFor(v)}}</span>
                                </li>
                                <li class="grid">
                                    <input v-if="searchNext.length" type="button" value="More" @click="resultAddNext()" />
                                </li>
                            </ul>
                        </p>
                    </div>
                    `,
                    props: {
                        decArgs: Object,
                        distances: Object,
                        feature: String,
                        featureSearchInit: String,
                        fileCount: Number,
                        labels: Object,
                        labelCounts: Object,
                        minSamplesInit: String,
                        prefixSkipInit: String,
                        prefixMaxInit: String,
                    },
                    data() {
                        return {
                            labelIndicesPresorted: [],
                            localFilter: '',
                            localFilterCaseSensitive: false,
                            minSamples: this.minSamplesInit,
                            prefixSkip: this.prefixSkipInit,
                            prefixMax: this.prefixMaxInit,
                            results: [],
                            resultsByParser: {},
                            search: this.featureSearchInit,
                            searchActual: -1,
                            searchCaseSensitive: false,
                            searchList: [],
                            searchListMax: 10,
                            searchNext: [],
                            searchTimeout: null,
                        };
                    },
                    computed: {
                        resultsByParserSorted() {
                            const arr = Object.entries(this.resultsByParser);
                            // Individual entries are already sorted by
                            // descending.

                            // Invert to match default Vue ordering.
                            return (
                                    arr.sort((a, b) =>
                                        Math.abs(this.distances[0][a[1][0]])
                                        - Math.abs(this.distances[0][b[1][0]]))
                                    .map((v) => [v[1], v[0]])
                            );
                        },
                    },
                    watch: {
                        localFilter() {
                            this.localFilterTimeout !== null && clearTimeout(this.localFilterTimeout);
                            this.localFilterTimeout = setTimeout(() => this.localFilterUpdate(), 300);
                        },
                        localFilterCaseSensitive() {
                            this.localFilterTimeout !== null && clearTimeout(this.localFilterTimeout);
                            this.localFilterTimeout = setTimeout(() => this.localFilterUpdate(), 300);
                        },
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
                        this.resultsRerun();
                    },
                    methods: {
                        featureChange(v, args) {
                            const update = {
                                    feature_search: this.search,
                                    local_filter: this.localFilter,
                                    local_filter_case_sensitive: this.localFilterCaseSensitive,
                                    min_samples: this.minSamples,
                                    prefix_skip: this.prefixSkip,
                                    prefix_max: this.prefixMax,
                            };
                            if (v != null) update['feature'] = this.labels[v];
                            if (args) Object.assign(update, args);
                            callRedecide(update);
                        },
                        getLabelFor(i) {
                            return `[+${this.labelCounts[i]} / -${this.fileCount - this.labelCounts[i]}] ${this.labels[i]}`;
                        },
                        localFilterUpdate() {
                            this.resultsRerun();
                        },
                        resultsRerun() {
                            // Populate pre-sorted labels s.t. most interesting
                            // features show at top of search.
                            const vHalf = this.fileCount * 0.5;
                            const ordering = Array.from({length: this.labels.length},
                                    (_, i) => i);
                            ordering.sort((a, b) =>
                                    Math.abs(vHalf - this.labelCounts[a])
                                    - Math.abs(vHalf - this.labelCounts[b]));
                            this.labelIndicesPresorted = ordering;

                            this.searchUpdate();

                            this.results.length = 0;
                            if (this.feature.length === 0) return;

                            this.searchActual = this.labels.indexOf(this.feature);
                            let filterFn = () => true;
                            if (this.localFilter.length) {
                                const fr = new RegExp(this.localFilter,
                                        this.localFilterCaseSensitive ? '' : 'i');
                                filterFn = (x) => (
                                        fr.test(this.labels[x[1]])
                                        || x[1] === this.searchActual);
                            }

                            // Compute searchNext
                            this.searchNext = (this.distances[0]
                                    .map((x, idx) => [-Math.abs(x), idx])
                                    .filter(filterFn)
                                    .sort((a, b) => a[0] - b[0])
                                    .map(x => x[1]));
                            this.resultSetup();
                            this.resultAddNext();
                        },
                        searchAddNext() {
                            this.searchListMax += 20;
                            this.searchUpdate();
                        },
                        searchDistance(i) {
                            return this.distances[0][i];
                        },
                        searchUpdate() {
                            this.searchList = [];
                            const flags = (this.searchCaseSensitive ? '' : 'i');
                            const re = new RegExp(this.search, flags);
                            for (const i of this.labelIndicesPresorted) {
                                if (!re.test(this.labels[i])) continue;

                                this.searchList.push(i);
                                if (this.searchList.length >= this.searchListMax) {
                                    this.searchList.push(-1);
                                    break;
                                }
                            }
                        },
                        resultAddNext() {
                            let i = Math.min(this.searchNext.length, 50);
                            this.results.push.apply(this.results, this.searchNext.splice(0, i));
                        },
                        resultSetup() {
                            const nPerParser = 3;
                            const byParser = {};
                            this.resultsByParser = {};
                            for (let i = 0, m = this.searchNext.length; i < m; i++) {
                                const vi = this.searchNext[i];
                                const vv = this.labels[vi];
                                const vParser = vv.split('_', 1)[0];
                                const vOld = byParser[vParser] || 0;
                                if (vOld < nPerParser) {
                                    byParser[vParser] = vOld + 1;
                                    if (vOld === 0) {
                                        this.resultsByParser[vParser] = [];
                                    }
                                    this.resultsByParser[vParser].push(vi);
                                }
                            }
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
                    <vue-header :debug-str="debug_str" />
                    <!-- <similarity-search :linkage="linkage" :labels="labels" :label-counts="label_counts" /> -->
                    <similarity-search-direct
                            :dec-args="data.dec_args"
                            :feature="data.dec_args.feature"
                            :feature-search-init="data.dec_args.feature_search"
                            :local-filter="data.dec_args.local_filter"
                            :local-filter-case-sensitive="data.dec_args.local_filter_case_sensitive"
                            :min-samples-init="data.dec_args.min_samples"
                            :prefix-skip-init="data.dec_args.prefix_skip"
                            :prefix-max-init="data.dec_args.prefix_max"
                            :distances="distance_matrix" :labels="labels" :label-counts="label_counts" :file-count="file_count" />
                    <!-- <dendrogram :id="labels.length + linkage.length - 1" :linkage="linkage" :labels="labels" :label-counts="label_counts" :start-uncollapsed="true" /> -->
                </div>`})</script>''')
        f.write('</body>')
        f.write('</html>')


if __name__ == '__main__':
    typer.run(main)

