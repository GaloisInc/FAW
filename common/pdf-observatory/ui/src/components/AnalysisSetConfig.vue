<template lang="pug">
  v-sheet(:elevation="3" style="padding: 1em; margin: 1em;")
    <!-- summary -->
    v-autocomplete(label="Analysis set" :items="asOptions"
        :value="currentId" @input="$emit('update:currentId', $event)")

    v-btn(block @click="expanded = !expanded")
      span(v-if="!expanded") Details / Edit / Regenerate
      span(v-else) Collapse
    div(v-if="expanded" style="border: solid 1px #000; border-top: 0; padding: 0.25em;")
      <!-- Description / definition of this set -->
      v-form(ref="saveAsForm" v-model="asFormValid")
        v-container
          v-row
            v-text-field(label="Name"
                :disabled="!saveAsFork && currentId !== NEW_ID" v-model="saveAs.id"
                :rules="[v => /^[a-zA-Z][a-zA-Z0-9_-]*$/.test(v) || 'Must match  [a-zA-Z][a-zA-Z0-9_-]*', v => saveAsValidateName() || 'Name already in use']")
          template(v-if="currentId !== NEW_ID")
            v-row
              v-checkbox(v-model="saveAsFork" label="Fork analysis set")
          v-row
            v-text-field(label="Max documents (leave blank to disable)"
                v-model="saveAsSample"
                :rules="[v => /^[0-9]*$/.test(v) || 'Numbers only']")
          v-row
            v-textarea(label="File specification regex" prefix="^" v-model="saveAs.definition.files")
          v-row
            v-checkbox(v-model="saveAs.definition.files_case" label="Case sensitive"
                style="margin-top: 0")
          v-row
            v-label Feature specifications
            table(style="width: 100%")
              tr(v-for="f of (saveAs.definition.features || [])")
                td
                  v-btn(@click="saveAs.definition.features.splice(saveAs.definition.features.indexOf(f), 1)")
                    v-icon(dark) mdi-delete
                td
                  v-autocomplete(label="Parser" :items="saveAsUiFeatureParsers"
                      v-model="f.parser")
                td
                  v-text-field(v-model="f.ft" :label="saveAsUiFeatureLabel(f)" prefix="^")
                td
                  v-checkbox(v-model="f.ft_case" label="Case sensitive")
              tr
                td
                  v-btn(@click="saveAsNewFeature()")
                    v-icon(dark) mdi-plus
          v-row(style="display: block")
            v-expansion-panels
              v-expansion-panel(v-for="parser of saveAsUiRules" :key="parser.id")
                v-expansion-panel-header(:color="parser.rules.length > 0 ? (parser.rulesComplex ? 'orange lighten-3' : 'green lighten-3') : ''")
                  div
                    span {{parser.id}} ({{parser.size ? `~${(parser.size / 1024).toFixed(2)}kB/doc` : 'no size'}}
                    span(v-if="parser.rules.length > 0") ; included
                    span(v-else) {{'\xa0'}}; omitted
                    span )
                v-expansion-panel-content
                  v-container
                    v-row(v-for="rule of parser.rules" :key="rule.index")
                      v-text-field(v-model="rule.rule.src" label="Include" prefix="^" placeholder="(empty for all)")
                      v-checkbox(v-model="rule.rule.src_case" label="Case sensitive")
                      v-text-field(v-model="rule.rule.dst" label="As" placeholder="Leave blank for no change; otherwise, new name")
                      v-btn(color="red" @click="saveAs.definition.rules.splice(rule.index, 1)")
                        v-icon mdi-trash-can-outline
                    v-row
                      v-btn(@click="saveAs.definition.rules.push({parser: parser.id, src: '', dst: ''})") Add new
          v-row(class="btn-row")
            v-btn(@click="asFormSave" :disabled="!asFormValid") Save / regenerate
            v-btn(color="red" @click="asFormDelete" :disabled="currentId === NEW_ID") Delete
</template>

<style scope lang="scss">
  .headerRow {
    cursor: pointer;
  }
  .btn-row {
    margin-top: 2em !important;

    button ~ button {
      margin-left: 1em !important;
    }
  }
</style>

<script lang="ts">
import Vue from 'vue';

/** Filter requirement for feature */
interface AsFeature {
  parser: string;
  ft: string;
  ft_case?: boolean;
}

interface AsRule {
  parser: string;
  filesets?: Array<string>;
  src: string;
  src_case?: boolean;
  dst: string;
}

interface AsParser {
  id: string;
  size_doc: number;
  filesets?: Array<string>;
}

/** Contains information required to regenerate the analysis set
  */
interface AsDefinition {
  files: string;
  files_case: boolean;
  features?: Array<AsFeature>;
  sample: number;
  rules: Array<AsRule>;
}

interface AsStatus {
  id: string;
  size_docs: number;
  size_disk: number;
  status: string;
  definition: AsDefinition,
}
namespace AsStatus {
  export function makeEmpty(id: string, parsers?: Array<AsParser>): AsStatus {
    const r: AsStatus = {
      id,
      size_docs: 0,
      size_disk: 0,
      status: '',
      definition: {
        files: '',
        files_case: false,
        sample: 0,
        rules: [],
      },
    };

    if (parsers) {
      for (const p of parsers) {
        // Include anything adding 10 or fewer bytes per file by default
        if (p.size_doc > 10) continue;
        r.definition.rules.push({parser: p.id, src: '', dst: ''});
      }
    }

    return r;
  }
}

interface AsData {
  asets: Array<AsStatus>;
  parsers: Array<AsParser>;
}

export default Vue.extend({
  components: {
  },
  props: {
    currentId: String,
  },
  data() {
    return {
      NEW_ID: '<new>',
      alive: true,
      asData: {asets: [], parsers: []} as AsData,
      asFormValid: false,
      expanded: false,
      ftCountCache: new Map<string, Map<string, {count: number|string, updated: number}>>(),
      saveAs: AsStatus.makeEmpty(''),
      saveAsFork: false,
    }
  },
  computed: {
    asOptions(): Array<any> {
      let options = this.asData.asets.map(x => {
        let label = [x.id, ' (', x.size_docs.toString(), ' documents, ',
            (x.size_disk / 1024 / 1024).toFixed(1), ' MB'];
        if (x.status.length) label.push(`; ${x.status}`);
        label.push(')');
        return {
          text: label.join(''),
          value: x.id,
        }
      });
      options.push({
          text: '<new>',
          value: '<new>',
      });
      return options;
    },
    saveAsSample: {
      get(): string {
        return this.saveAs.definition.sample ? this.saveAs.definition.sample.toString() : '';
      },
      set(v: string) {
        let vv = parseInt(v);
        if (isNaN(vv)) vv = 0;
        this.saveAs.definition.sample = vv;
      },
    },
    /** Return information on parsers which can be used to filter analysis sets
        via features. */
    saveAsUiFeatureParsers(): Array<any> {
      return this.asData.parsers.filter(x => true).map(x => {return {
        text: x.id,
        value: x.id,
      }});
    },
    /** Return information on parsers cross UI rules */
    saveAsUiRules(): Array<any> {
      let r = [];
      const map = new Map<string, any>();
      for (const p of this.asData.parsers) {
        r.push({id: p.id, size: p.size_doc, rulesComplex: false, rules: []});
        map.set(p.id, r[r.length - 1]);
      }
      const rules = this.saveAs.definition.rules;
      for (let i = 0, m = rules.length; i < m; i++) {
        const rule = rules[i];

        let rr = map.get(rule.parser);
        if (rr === undefined) {
          // A rule based on old data? Still show the user
          const rrName = `<NOT FOUND> ${rule.parser}`;
          rr = map.get(rrName);
          if (rr === undefined) {
            rr = {id: rrName, size: null, rules: []};
            r.push(rr);
            map.set(rrName, rr);
          }
        }
        rr.rules.push({index: i, rule});

        // Flag more complicated things
        if (rule.src.trim().length || rule.dst.trim().length) rr.rulesComplex = true;
      }
      return r;
    },
  },
  watch: {
    currentId() {
      this.saveAsFork = false;
      if (this.currentId === this.NEW_ID) {
        this.expanded = true;
        // Reset to defaults
        this.saveAs = AsStatus.makeEmpty('', this.asData.parsers);
      }
      else {
        // Deep clone so user doesn't mess up cached representation of this
        // data set.
        let found: AsStatus|undefined;
        for (const a of this.asData.asets) {
          if (a.id === this.currentId) {
            found = a;
            break
          }
        }
        if (!found) {
          found = AsStatus.makeEmpty('<error>', this.asData.parsers);
        }
        this.saveAs = JSON.parse(JSON.stringify(found));
      }
    },
    saveAsFork() {
      if (this.currentId !== this.NEW_ID) {
        this.saveAs.id = this.currentId;
      }
      (this.$refs.saveAsForm as any).validate();
    },
  },
  mounted() {
    this.asyncTry(async () => {
      while (this.alive) {
        await this.update();
        await new Promise((resolve) => setTimeout(resolve, 5000));
      }
    });
  },
  beforeDestroy() {
    this.alive = false;
  },
  methods: {
    asFormSave() {
      this.asyncTry(async () => {
        if (!this.asFormValid) return;

        // Client-side valid, submit request
        await this.$vuespa.call('analysis_set_update', this.saveAs);
        await this.update();
        this.$emit('update:currentId', this.saveAs.id);
        // Emit a standardized "update" in case our id didn't change and a
        // reset was not triggered.
        this.$emit('update');
        this.expanded = false;
      });
    },
    asFormDelete() {
      this.asyncTry(async () => {
        await this.$vuespa.call('analysis_set_delete', this.currentId);
        this.expanded = false;
        this.$emit('update:currentId', undefined);
        await this.update();
      });
    },
    saveAsNewFeature() {
      let f = this.saveAs.definition.features;
      if (!f) {
        f = [];
        this.$set(this.saveAs.definition, 'features', f);
      }
      f.push({parser: '', ft: ''});
    },
    /** Fetches the feature label for a given query. */
    saveAsUiFeatureLabel(f: AsFeature): string {
      const fParser = f.parser;
      let parserStats = this.ftCountCache.get(fParser);
      if (parserStats === undefined) {
        parserStats = new Map();
        this.ftCountCache.set(fParser, parserStats);
      }

      const fFtKey = f.ft + '///' + (f.ft_case ? '' : 'i');
      let rec = parserStats.get(fFtKey);
      const oldTime = Date.now() - 120 * 1000;
      let count: any = '<pending>';
      if (rec === undefined || rec.updated < oldTime) {
        // Update -- clean cache first to prevent memory leaks
        for (const [k, v] of parserStats.entries()) {
          if (v.updated < oldTime) {
            parserStats.delete(k);
          }
        }

        // Then insert and query
        rec = {count: '<pending>', updated: Date.now()};
        parserStats.set(fFtKey, rec);
        (async () => {
          try {
            const r = await this.$vuespa.call('analysis_set_ft_count', f);
            rec.count = r;
          }
          catch (e) {
            rec.count = '<error>';
            throw e;
          }
          finally {
            // Reactivity
            rec.updated = Date.now();
            this.ftCountCache = new Map(this.ftCountCache);
          }
        })().catch(console.error);
      }
      else {
        count = rec.count;
      }
      return `Feature (${count} matching files)`;
    },
    saveAsValidateName() {
      if (this.currentId === this.NEW_ID || this.saveAsFork) {
        // Must not be in list
        return (this.asData.asets.map(x => x.id).indexOf(this.saveAs.id)
            === -1);
      }
      return true;
    },
    async update() {
      this.asData = await this.$vuespa.call('analysis_set_data');
      if (!this.currentId) {
        if (this.asData.asets.length > 0) {
          this.$emit('update:currentId', this.asData.asets[0].id);
        }
        else {
          this.$emit('update:currentId', this.NEW_ID);
        }
      }
    },

    async asyncTry(fn: {(): Promise<void>}) {
      try {
        await fn();
      }
      catch (e) {
        this.$emit('error', e);
        throw e;
      }
    },
  },
})

</script>
