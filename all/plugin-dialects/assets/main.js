let dEpsilon = 1e-8;
function callRedecide(args) {
    let argsNew = Object.assign({}, window.data.dec_args, args);

    let req = new XMLHttpRequest();
    let url = window.data.api_url + 'redecide';
    req.open('post', url, true);
    req.setRequestHeader('Content-Type', 'application/json');
    req.send(JSON.stringify(argsNew));
}

// Dumbest omission from a language prelude
function isEmpty(o) {
    return Object.keys(o).length === 0;
    // for (const p in o) {
    //     if (Object.hasOwn(o, p)) return false;
    // }
    // return true;
}

Vue.component('vue-header', {
    template: `<div class="header">
        <details v-if="debugStr.length">
            <summary>Debug Info</summary>
            <pre>{{debugStr}}</pre>
        </details>
    </div>`,
    props: {
        debugStr: String,
    },
});

Vue.component('dialect-wizard', {
    template: `
<div class="dialect-wizard">
    <div>
        Total: {{featureText.length}} features from {{numFiles}} files
    </div>
    <hr>
    <details
        class="feature-search"
        :open="searchSettings.feature_search_expanded"
        @toggle="searchSettings.feature_search_expanded = $event.target.open"
    >
        <summary>Target Feature Search</summary>
        <div>
            <label>
                Search phrase (as regex): 
                <input v-model="searchSettings.feature_search_regex" type="text" />
            </label>
            <label>
                <input v-model="searchSettings.feature_search_regex_case_sensitive" type="checkbox" />
                Case sensitive
            </label>
        </div>
        <div>
            <label>
                Sort by:
                <select v-model.number="searchSettings.sort_order">
                    <option value="descending">Most Files</option>
                    <option value="ascending">Least Files</option>
                    <option value="entropy">Greatest Entropy</option>
                </select>
            </label>
        </div>
        <ul class="search-results">
            <li>
                <button @click="targetAllFiles">Clear Target</button>
                <span>All {{numFiles}} files</span>
            </li>
            <li v-for="featureIndex in featureSearchResults">
                <template v-if="featureIndex != -1">
                    <button @click="targetFeature(featureIndex, true)">Target (AND)</button>
                    <button @click="targetFeature(featureIndex, false)">Target (AND NOT)</button>
                    <button @click="excludeFeature(featureIndex)">Exclude</button>
                    <span>{{featureLabel(featureIndex)}}</span>
                </template>
                <button v-else @click="featureSearchAddNext()">More</button>
            </li>
        </ul>
    </details>
    <hr>
    <div class="targeted-features">
        Target:
        <span v-if="targetedFeaturesCnf.length == 0">
            All {{numFiles}} files
        </span>
        <ul v-else class="target-list">
            <li v-for="[featureIndex, included] in targetedFeaturesCnf" :key="featureIndex" >
                {{featureLabel(featureIndex, included)}}
                <button @click="untargetFeature(featureIndex);">
                    Remove
                </button>
            </li>
        </ul>
    </div>
    <hr>
    <div class="excluded-features">
        Excluded Features:
        <span v-if="dialectSettings.excluded_features.length === 0">(none)</span>
        <ul v-else>
            <li v-for="(featureIndex, i) in dialectSettings.excluded_features">
                {{featureLabel(featureIndex)}}
                <button @click="dialectSettings.excluded_features.splice(i, 1)">
                    Remove
                </button>
            </li>
        </ul>
    </div>
    <label>
        Similar features are also excluded if above attributable risk threshold: 
        <input v-model.number="dialectSettings.exclusion_min_attr_risk" type="number" min="-1" max="1" />
    </label>
    <hr>
    <label>
        Min dialect size: 
        <input v-model.number="dialectSettings.min_dialect_size" type="number" min="1" />
    </label>
    <br/>
    <label>
        Dialect out-of-target requirement: 
        <select v-model="dialectSettings.target_restriction_mode">
            <option value="target_only">Not represented outside target</option>
            <option value="homogeneous_outside">Homogeneous outside target</option>
            <option value="ignore_outside">No restriction (ignore out of target)</option>
        </select>
    </label>
    <br/>
    <span v-if="dialectSettings.target_restriction_mode === 'target_only'">
        Out-of-target files with the dialects' hero features count against max outliers.
    </span>
    <span v-else-if="dialectSettings.target_restriction_mode === 'homogeneous_outside'">
        Out-of-target files with (or without, whichever is fewer) the dialects' hero
        features count against max outliers.
    </span>
    <span v-else>
        Out-of-target files do not count against max outliers.
    </span>
    <br/>
    <label>
        Max files in multiple/no dialects (outliers): 
        <input v-model.number="dialectSettings.max_slop_files" type="number" min="0" />
    </label>
    <br/>
    <label>
        Include dialects based on inverted features
        <input v-model="dialectSettings.allow_inverted_features" type="checkbox" />
    </label>
    <hr>
    <label>
        Max partitions
        <input v-model.number="dialectSettings.max_partitions" type="number" min="1" />
    </label>
    <br/>
    <label>
        Max dialects per partition
        <input v-model.number="dialectSettings.max_dialects" type="number" min="2" />
    </label>
    <br/>
    <label>
        Disallow very similar dialects between partitions
        <input v-model="dialectSettings.no_partition_overlap" type="checkbox" />
    </label>
    <hr>
    <div>
        Highlight file: 
        <input type="text" v-model="dialectSettings.highlighted_filename" />
    </div>
    <button
        @click="findDialects"
        :style="dialectSettingsUpdated ? 'background-color: limegreen;' : ''"
    >
        Find new dialects
    </button>

    <div v-if="dialectSettingsInit.find_dialects">
        <h3>Dialect Wizard</h3>
        <div>
            Target:
            <span v-if="isEmpty(dialectSettingsInit.targeted_features_cnf)">
                All {{numFiles}} files
            </span>
            <ul v-else class="target-list">
                <li
                    v-for="(included, featureIndex) in dialectSettingsInit.targeted_features_cnf"
                    :key="featureIndex"
                >
                    {{featureLabel(featureIndex, included)}}
                </li>
            </ul>
        </div>
        <div class="partitions-container">
            <p v-if="partitions.length === 0">No Partitions</p>
            <p v-else>{{partitions.length}} Partition{{partitions.length > 1 ? 's' : ''}}</p>
            <ol v-if="partitions.length !== 0" class="partitions-list">
                <li v-for="partition of partitions">
                    <b>{{partition.dialects.length}} Dialects</b>
                    <details>
                        <summary>Quality Metrics</summary>
                        <ul>
                            <li v-for="(partition_quality, metric_name) in partition.partition_quality">
                                {{metric_name}}: {{partition_quality.toFixed(4)}}
                            </li>
                        </ul>
                    </details>
                    <ol class="dialects-list">
                        <li v-for="dialect of partition.dialects">
                            <div :style="dialect.highlight ? 'background-color: yellow;' : ''">
                                <template v-if="dialect.inverted">NOT </template>
                                <code>{{featureText[dialect.hero_feature]}}</code><br/>
                                {{featureFractionReport(dialect.size_target, dialect.size_global)}}
                            </div>
                            <button @click="targetFeature(dialect.hero_feature, true)">Target (AND)</button>
                            <button @click="targetFeature(dialect.hero_feature, false)">Target (AND NOT)</button>
                            <button @click="excludeFeature(dialect.hero_feature)">Exclude</button>
                            <details v-if="dialect.filenames_outside_target.length > 0">
                                <summary v-if="dialect.filenames_outside_target_inverted">
                                    {{dialect.filenames_outside_target.length}} outliers without feature outside target
                                </summary>
                                <summary v-else>
                                    {{dialect.filenames_outside_target.length}} outliers with feature outside target
                                </summary>
                                <ul class="file-list">
                                    <li
                                        v-for="([filename, highlight], i) in dialect.filenames_outside_target"
                                        :style="highlight ? 'background-color: yellow;' : ''"
                                    >
                                        {{filename}}
                                    </li>
                                </ul>
                            </details>
                            <details>
                                <summary>Implied Features ({{dialect.similar_features.length}})</summary>
                                <ul class="implied-features-list">
                                    <li v-for="simFeature in dialect.similar_features">
                                        <div :style="simFeature.highlight ? 'background-color: yellow;' : ''">
                                            <template v-if="simFeature.inverted">NOT </template>
                                            <code>{{featureText[simFeature.feature]}}</code><br/>
                                            Attributable risk: {{simFeature.attr_risk.toFixed(2)}}<br/>
                                            {{featureFractionReport(simFeature.size_target, simFeature.size_global)}}
                                        </div>
                                    </li>
                                </ul>
                            </details>
                        </li>
                    </ol>
                    <details>
                        <summary :style="partition.slop_files.some(slopFile => slopFile.highlight) ? 'background-color: yellow;' : ''"
                        >Files in Multiple/No Dialects ({{partition.slop_files.length}})</summary>
                        <ul>
                            <li v-for="([n, [slopFiles, highlightAny]], i) of slopFileHistogram(partition.slop_files)">
                                <details>
                                    <summary :style="highlightAny ? 'background-color: yellow;' : ''">{{slopFiles.length}} files in {{n}} dialects</summary>
                                    <ul class="file-list">
                                        <li v-for="slopFile of slopFiles" :style="slopFile.highlight ? 'background-color: yellow;' : ''">
                                            {{slopFile.filename}} <br/>
                                            In {{slopFile.in_dialects.length}} dialects: 
                                            {{slopFile.in_dialects.map((x) => x + 1)}}
                                        </li>
                                    </ul>
                                </details>
                            </li>
                        </ul>
                    </details>
                    <button @click="excludeFeaturesFromPartition(partition)">
                        Exclude features in this partition
                    </button>
                </li>
            </ul>
        <div>
    </div>
</div>
    `,
    props: {
        searchSettingsInit: Object,
        dialectSettingsInit: Object,
        featureText: Array,
        featureCounts: Array,
        numFiles: Number,
        partitions: Array,
        targetSize: Number,
    },
    data() {
        return {
            searchSettings: structuredClone(this.searchSettingsInit),
            dialectSettings: structuredClone(this.dialectSettingsInit),
            dialectSettingsUpdated: false,
            sortedFeatures: [],
            featureSearchResults: [],
            featureSearchResultsMax: 10,
            searchTimeout: null,
            // Copy target into a form watchable by vue
            targetedFeaturesCnf: Object.entries(this.dialectSettingsInit.targeted_features_cnf),
        };
    },
    computed: {
        /**
         * 
         * @returns args to callRedecide
         */
        currentSettings() {
            return {
                search_settings: this.searchSettings,
                dialect_settings: this.dialectSettings,
            };
        },
    },
    watch: {
        dialectSettings: {
            handler() {
                this.dialectSettingsUpdated = true;
            },
            deep: true,
        },
        targetedFeaturesCnf: {
            handler() {
                this.dialectSettingsUpdated = true;
            },
            deep: true,
        },
        'searchSettings.feature_search_regex'() {
            this.searchTimeout !== null && clearTimeout(this.searchTimeout);
            this.searchTimeout = setTimeout(() => this.featureSearchUpdate(), 300);
        },
        'searchSettings.feature_search_regexCaseSensitive'() {
            this.searchTimeout !== null && clearTimeout(this.searchTimeout);
            this.searchTimeout = setTimeout(() => this.featureSearchUpdate(), 300);
        },
        'searchSettings.sort_order'() {
            this.searchTimeout !== null && clearTimeout(this.searchTimeout);
            this.searchTimeout = setTimeout(() => {
                this.sortFeaturesBy(this.searchSettings.sort_order);
                this.featureSearchUpdate();
            }, 300);
        },
    },
    mounted() {
        this.presortFeatures();
        this.featureSearchUpdate();
    },
    methods: {
        presortFeatures() {
            this.sortedFeatures = Array.from(
                {length: this.featureText.length},
                (_, i) => i
            );
            this.sortFeaturesBy(this.searchSettings.sort_order);
        },
        sortFeaturesBy(order) {
            if (order == 'ascending') {
                this.sortedFeatures.sort((f1, f2) => this.featureCounts[f1] - this.featureCounts[f2]);
            } else if (order == 'descending') {
                this.sortedFeatures.sort((f1, f2) => this.featureCounts[f2] - this.featureCounts[f1]);
            } else if (order == 'entropy') {
                // Highest entropy when ratio of pos:neg files close to 1
                this.sortedFeatures.sort((f1, f2) =>
                    Math.abs(this.numFiles / 2 - this.featureCounts[f1])
                    - Math.abs(this.numFiles / 2 - this.featureCounts[f2])
                );
            }
        },
        /**
         * 
         * @param {number} featureIndex Feature to target
         * @param {boolean} included If True, include feature, else exclude
         */
        targetFeature(featureIndex, included) {
            const map = new Map(this.targetedFeaturesCnf);
            map.set(featureIndex, included);
            this.targetedFeaturesCnf = [...map.entries()];
        },
        targetAllFiles() {
            this.targetedFeaturesCnf = [];
        },
        untargetFeature(featureIndex) {
            const map = new Map(this.targetedFeaturesCnf);
            map.delete(featureIndex);
            this.targetedFeaturesCnf = [...map.entries()];
        },
        findDialects() {
            this.dialectSettings.find_dialects = true;
            this.dialectSettings.targeted_features_cnf = Object.fromEntries(this.targetedFeaturesCnf);
            callRedecide(this.currentSettings);
        },
        featureLabel(i, included) {
            if (included === undefined) {
                included = true;
            }
            i = Number(i);  // This has some annoying edge cases but I don't care
            if (i >= 0) {
                let posFiles = this.featureCounts[i];
                let negFiles = this.numFiles - posFiles;
                if (!included) {
                    [posFiles, negFiles] = [negFiles, posFiles];
                }
                return `[+${posFiles} / -${negFiles}] ${included ? '' : 'NOT'} ${this.featureText[i]}`;
            } else {
                return `All ${this.numFiles} Files`
            }
        },
        slopFileHistogram(slopFiles) {
            // Organize slop files into dialect-count-major
            // Used when displaying partitions

            // Mapping from dialect count to [list of files, highlight any]
            const slopFilesByDialectCount = {};
            for (const slopFile of slopFiles) {
                if (slopFile.in_dialects.length in slopFilesByDialectCount) {
                    slopFilesByDialectCount[slopFile.in_dialects.length][0].push(slopFile);
                    slopFilesByDialectCount[slopFile.in_dialects.length][1] |= slopFile.highlight;
                } else {
                    slopFilesByDialectCount[slopFile.in_dialects.length] = [[slopFile], slopFile.highlight];
                }
            }
            const sortedHist = Object.entries(slopFilesByDialectCount);
            sortedHist.sort((pairA, pairB) => pairB[0] - pairA[0]);
            return sortedHist;
        },
        featureFractionReport(sizeWithinTarget, sizeGlobal) {
            return (
                `${sizeWithinTarget} files (${(100 * sizeWithinTarget / this.targetSize).toFixed(2)}%) `
                + `within target; ${sizeGlobal - sizeWithinTarget} files outside target`
            );
        },
        excludeFeature(featureIndex) {
            const excludedFeaturesSet = new Set(this.dialectSettings.excluded_features);
            excludedFeaturesSet.add(featureIndex);
            this.dialectSettings.excluded_features = [...excludedFeaturesSet];
        },
        excludeFeaturesFromPartition(partition) {
            const excludedFeaturesSet = new Set(this.dialectSettings.excluded_features);
            for (dialect of partition.dialects) {
                excludedFeaturesSet.add(dialect.hero_feature);
            }
            this.dialectSettings.excluded_features = Array.from(excludedFeaturesSet);
        },
        featureSearchAddNext() {
            this.featureSearchResultsMax += 20;
            this.featureSearchUpdate();
        },
        featureSearchUpdate() {
            this.featureSearchResults = [];
            const flags = (this.searchSettings.feature_search_regex_case_sensitive ? '' : 'i');
            const re = new RegExp(this.searchSettings.feature_search_regex, flags);
            for (const i of this.sortedFeatures) {
                if (!re.test(this.featureText[i])) continue;

                this.featureSearchResults.push(i);
                if (this.featureSearchResults.length >= this.featureSearchResultsMax) {
                    this.featureSearchResults.push(-1); // indicates we should show 'more' button
                    break;
                }
            }
        },
    },
});
