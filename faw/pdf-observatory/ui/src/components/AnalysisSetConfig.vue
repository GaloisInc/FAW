<template lang="pug">
  v-sheet.analysis-set-config(:elevation="3" style="padding: 1em; margin-block: 1em;")
    <!-- summary -->
    v-autocomplete(
      v-if="asOptions.length"
      label="Analysis set"
      :items="asOptions"
      :value="currentId"
      @input="$emit('update:currentId', $event)"
    )
    v-alert(v-else dense type="error") No analysis sets built

    v-btn-toggle(
      @change="onChangeSettingsView"
      v-model="selectedSettingsView"
    )
      v-btn Create New Analysis Set
      v-btn(v-if="currentId !== null") Active Analysis Set Details / Edit / Regenerate
    div(v-if="expanded" style="border: solid 1px #000; border-top: 0; padding: 0.25em;")
      <!-- Description / definition of this set -->
      v-form(ref="saveAsForm" v-model="asFormValid")
        v-container
          v-row
            v-text-field(label="Name"
                :disabled="!saveAsFork && !creatingNewAset" v-model="saveAs.id"
                :rules="[v => /^[a-zA-Z][a-zA-Z0-9_-]*$/.test(v) || 'Must match  [a-zA-Z][a-zA-Z0-9_-]*', v => saveAsValidateName() || 'Name already in use']")
          template(v-if="!creatingNewAset")
            v-row
              v-checkbox(v-model="saveAsFork" label="Fork analysis set")
          v-row
            v-text-field(label="Max documents (leave blank to disable)"
                v-model="saveAsSample"
                :rules="[v => /^[0-9]*$/.test(v) || 'Numbers only']")
          v-row
            v-textarea(
              label="File specification regex"
              prefix="^"
              v-model="saveAs.definition.files"
              auto-grow
              rows=1
            )
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
            v-label Parsers (expand and click "Add New" to include)
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
            v-btn(@click="asFormSave" :disabled="!asFormValid" color="primary")
              v-template(v-if="creatingNewAset || saveAsFork") Create
              v-template(v-else) Save / Regenerate
            v-dialog(
              v-if="!(creatingNewAset || saveAsFork)"
              v-model="asFormDeleteDialog"
              persistent
              max-width="800"
            )
              template(v-slot:activator="{on}")
                v-btn(color="red" v-on="on") Delete
              v-card
                v-card-title Really delete this analysis set?
                v-card-actions(:style="{'flex-wrap': 'wrap'}")
                  v-btn(@click="asFormDeleteDialog = false") Cancel
                  v-btn(@click="asFormDelete(); asFormDeleteDialog = false") Delete analysis set

    v-btn(block @click="pipeExpanded = !pipeExpanded" style="margin-top: 1em")
      span(v-if="!pipeExpanded") Pipelines
      span(v-else) Collapse
    div(v-if="pipeExpanded" style="border: solid 1px #000; border-top: 0; padding: 0.25em;")
      //- pipeline information for the currently selected analysis set.
      div
        span
          v-icon mdi-help-rhombus
        span(style="font-size:0.85em") Pipelines attached to analysis sets are a two-stage process: 1) The pipeline is initiated. All files *currently* matching the analysis set definition, ignoring max documents, will be available to the pipeline. Thus, best to wait until all non-pipeline parsers have finished. 2) When the pipeline finishes all of its tasks, and if the pipeline specifies any parsers, all analysis sets which have specified that they will use this pipeline's parsers will have their files processed with the parsers learned from this analysis set.
      v-expansion-panels
        AnalysisSetPipelineInfo(v-for="[k, v] of Object.entries(pipeCfg)"
            v-if="!v.disabled"
            :key="currentId + '-' + k"
            :pipeline="k"
            :aset="currentId"
            :aset-data="currentAsPipelines[k]"
            :cfg="v"
            @update="update()")
        //- Show expired / renamed pipelines so users can fix from UI
        AnalysisSetPipelineInfo(v-for="[k, v] of Object.entries(currentAsPipelines || {})"
            v-if="!pipeCfg[k] || pipeCfg[k].disabled"
            :key="currentId + '-' + k"
            :pipeline="k"
            :aset="currentId"
            :aset-data="currentAsPipelines[k]"
            :cfg="{label: '<<renamed or disabled? ' + k + '>>', disabled: false, tasks: {}}"
            @update="update()")
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
import bus from '@/bus';
import AnalysisSetPipelineInfo from '@/components/AnalysisSetPipelineInfo.vue';
import {AsData, AsFeature, AsPipeline, AsStatus} from '@/interface/aset.ts';
import Vue from 'vue';

export default Vue.extend({
  components: {
    AnalysisSetPipelineInfo,
  },
  props: {
    currentId: String, // may be null
    pipeCfg: Object as () => any,
  },
  data() {
    return {
      creatingNewAset: false,
      alive: true,
      asData: {asets: [], parsers: []} as AsData,
      asFormDeleteDialog: false,
      asFormValid: false,
      expanded: false,
      ftCountCache: new Map<string, Map<string, {count: number|string, updated: number}>>(),
      pipeExpanded: false,
      saveAs: AsStatus.makeEmpty(''),
      saveAsFork: false,
      selectedSettingsView: undefined as undefined | number,
    }
  },
  computed: {
    asOptions(): Array<any> {
      let options = this.asData.asets.map(x => {
        let label = [x.id, ' (', x.size_docs.toString(), ' documents, ',
            (x.size_disk / 1024 / 1024).toFixed(1), ' MB'];
        if (x.status.length) {
          label.push(`; ${x.status}`);
        }
        label.push(')');
        if (Object.keys((x.pipelines || {})).length !== 0) {
          label.push(' -- pipelines ');
          for (const [ki, k] of Object.keys(x.pipelines!).entries()) {
            if (ki !== 0) label.push(', ');
            label.push(k);
            if (!x.pipelines![k].done) {
              label.push('[running]');
            }
          }
        }
        return {
          text: label.join(''),
          value: x.id,
        }
      });
      return options;
    },
    currentAs(): AsStatus {
      const id = this.currentId;
      for (const a of this.asData.asets) {
        if (a.id === id) return a;
      }
      return AsStatus.makeEmpty('<not found>');
    },
    currentAsPipelines(): {[key: string]: AsPipeline} {
      const id = this.currentId;
      for (const a of this.asData.asets) {
        if (a.id === id) return a.pipelines;
      }
      return {};
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
      return this.asData.parsers.filter(x => !x.pipeline).map(x => {return {
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
      r.sort((a, b) => a.id.localeCompare(b.id));
      return r;
    },
  },
  watch: {
    currentId() {
      this.onSwitchedSettings();
    },
    saveAsFork() {
      if (!this.creatingNewAset) {
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
        this.selectedSettingsView = undefined;
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
    onChangeSettingsView(newSelection: null | number) {
      if (newSelection === undefined) {
        this.expanded = false;
      } else {
        this.expanded = true;
        this.creatingNewAset = newSelection === 0;
      }
      this.onSwitchedSettings();
    },
    onSwitchedSettings() {
      this.saveAsFork = false;
      if (this.creatingNewAset) {
        // Reset to defaults
        this.saveAs = AsStatus.makeEmpty('', this.asData.parsers);
      } else {
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
      if (this.creatingNewAset || this.saveAsFork) {
        // Must not be in list
        return (this.asData.asets.map(x => x.id).indexOf(this.saveAs.id)
            === -1);
      }
      return true;
    },
    async update() {
      const oldUpdateTime = this.currentAs.status_done_time;
      this.asData = await this.$vuespa.call('analysis_set_data');
      if (oldUpdateTime !== this.currentAs.status_done_time) {
        // Server-side update to analysis set; flag as needing update locally
        this.$emit('update');
      }
      bus.$emit('analysisSetData', this.asData);
      if (!this.currentId) {
        if (this.asData.asets.length > 0) {
          this.$emit('update:currentId', this.asData.asets[0].id);
        }
        else {
          this.$emit('update:currentId', null);
          this.selectedSettingsView = 0;  // "create new aset"
          this.creatingNewAset = true;
          this.expanded = true;
        }
      }
    },

    async asyncTry(fn: {(): Promise<void>}) {
      try {
        await fn();
      }
      catch (e) {
        bus.$emit('error', e);
        throw e;
      }
    },
  },
})

</script>
