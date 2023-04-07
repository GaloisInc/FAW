<template lang="pug">
  .status
    //- No longer needed, as info now populated by observatory.
      checkmark(:status="decisionSelected.status")
      span {{decisionSelected.status}}
      .decision-info {{decisionSelected.info}}

    div Current decision
      JsonTree(:data="decisionSelected" :level="2")

    div Reference decision
      JsonTree(:data="decisionReference" :level="2")

    div(style="font-weight: bold; border-top: solid 1px #000") In-Tool Decision Details
    .decision-overview
      div Results
      div(v-for="o of (decisionDefinition && decisionDefinition.outputs.keys() || [])"
          style="padding-left: 1em"
          ) {{o}}: {{decisionSelectedDsl[o]}}
      div Filters
      div(v-for="f of (decisionDefinition && decisionDefinition.filters || [])"
          :key="f.name"
          style="padding-left: 1em"
          )
        checkmark(:status="decisionSelectedDsl['filter-' + f.name] ? 'valid' : 'rejected'")
        span {{f.name}}

        template(v-if="fileStats.get(f.name) && fileStats.get(f.name).length > 0")
          v-virtual-scroll(:items="fileStats.get(f.name)" item-height="25" height="100" bench="2")
            template(v-slot:default="{item: k}")
              v-menu(offset-y max-width="400")
                template(v-slot:activator="{on}")
                  .decision-reason(:key="k[0]" v-on="on")
                    checkmark(:status="k[1]")
                    span {{k[0]}}
                v-list
                  v-list-item
                    v-btn(v-clipboard="() => regexEscape(k[0])") (Copy regex to clipboard)
                    v-btn(v-clipboard="() => '^' + regexEscape(k[0]) + '$'") (with ^$)

    .decision-reasons Full listing of reasons (#[checkmark(:status="'valid'")] indicates a match for filters; click to copy to clipboard):
      div(style="display: flex; flex-direction: row; align-items: start; margin-bottom: -1.5em")
        span (
        v-checkbox(
          v-model="fileStatsAsOnly"
          label="only show features included in current analysis set"
          style="padding-top: 0;"
        )
        span )
      div(style="display: flex; flex-direction: row; align-items: center")
        v-text-field(label="Search regex" v-model="fileStatsSearch")
        v-checkbox(v-model="fileStatsSearchInsensitive" label="Case-insensitive" style="margin-left: 0.2em")
      v-virtual-scroll(:items="fileStatsSearchCache" item-height="25" height="750" bench="2")
        template(v-slot:default="{item}")
          div(:key="item[0]")
            v-menu(offset-y max-width="400")
              template(v-slot:activator="{on}")
                .decision-reason(v-on="on")
                  checkmark(:status="item[1]")
                  span {{item[0]}}
              v-list
                v-list-item
                  v-btn(v-clipboard="() => regexEscape(item[0])") (Copy regex to clipboard)
                  v-btn(v-clipboard="() => '^' + regexEscape(item[0]) + '$'") (with ^$)
</template>

<style lang="scss">
// Inherited from Home.vue
</style>

<script lang="ts">
import Vue, {PropType} from 'vue';

import CheckmarkComponent from '@/components/CheckmarkComponent.vue';
import {PdfDecision, sortByReject} from '@/common/common';
import {DslResult} from '@/common/dsl';
import {regexEscape} from '@/util';

export default Vue.extend({
  components: {
    checkmark: CheckmarkComponent,
  },
  props: {
    asOptions: Object as PropType<any>,  // Options for analysis set
    decisionDefinition: Object as PropType<DslResult | null>,
    decisionSelected: Object as PropType<PdfDecision>,
    decisionSelectedDsl: Object as PropType<PdfDecision>,
    decisionReference: Object as PropType<PdfDecision>,
    extraFeaturesByFile: Object as PropType<{[filename:string]:string[]}>,
  },
  data() {
    return {
      fileStats: new Map<string, Array<[string, string]>>(),
      fileStatsAsOnly: false,
      fileStatsSearch: '',
      fileStatsSearchCache: new Array<[string, string]>(),
      fileStatsSearchInsensitive: true,
      fileStatsSearchTimer: null as any,
    };
  },
  watch: {
    decisionSelectedDsl() {
      this.updateDecisionReasons();
    },
    fileStatsAsOnly() {
      this.updateDecisionReasons();
    },
    fileStatsSearch() {
      if (this.fileStatsSearchTimer) clearTimeout(this.fileStatsSearchTimer);
      this.fileStatsSearchTimer = setTimeout(() => this.fileStatsSearchUpdate(), 250);
    },
  },
  methods: {
    fileStatsSearchUpdate() {
      const re = new RegExp(this.fileStatsSearch,
          this.fileStatsSearchInsensitive ? 'i' : '');
      this.fileStatsSearchCache = this.fileStats.get('other')!.filter(
          x => re.test(x[0]));
    },
    regexEscape(v: string): string {
      return regexEscape(v);
    },
    async updateDecisionReasons() {
      this.fileStats.clear();
      this.fileStats.set('other', []);
      this.fileStatsSearchUpdate();

      const tf = this.decisionSelectedDsl.testfile;
      if (tf) {
        const data = await this.$vuespa.call('load_db', tf, 'statsbyfile',
            this.asOptions, {as_only: this.fileStatsAsOnly});
        const extraFeatures = this.extraFeaturesByFile[tf];
        for (const extraFeature of (extraFeatures !== undefined ? extraFeatures : [])) {
          data[0][extraFeature] = 1;
        }
        // Prevent duplicates by re-setting the array whenever we get data
        this.fileStats.clear();
        this.fileStats.set('other', []);
        const otherSeen = new Set<string>();
        for (const [d, dv] of Object.entries(data[0])) {
          if ([0, false, null, undefined].indexOf(dv as any) !== -1 || ['_id'].indexOf(d) !== -1) {
            continue;
          }
          const info = this.decisionSelectedDsl.info;
          const dRe = new RegExp(`^'([^']*)' (rejected|accepted) '${regexEscape(d)}'$`, 'gm');
          const dMatches = new Array<RegExpExecArray|null>();
          let dMatch;
          for (const line of info) {
            while ((dMatch = dRe.exec(line)) !== null) {
              dMatches.push(dMatch);
            }
          }
          if (dMatches.length === 0) {
            // Placeholder for unused message
            dMatches.push(null);
          }
          for (dMatch of dMatches) {
            const dStatus = (
                !dMatch ? 'ignore'
                : dMatch[2] === 'rejected' ? 'rejected'
                : dMatch[2] === 'accepted' ? 'valid'
                : 'other');
            if (dMatch) {
              const dKey = dMatch[1];
              let arr = this.fileStats.get(dKey);
              if (!arr) {
                arr = [];
                this.fileStats.set(dKey, arr);
              }
              arr.push([d, dStatus]);
            }

            // Special behavior for 'other', to show all messages side by side
            if (!otherSeen.has(d)) {
              otherSeen.add(d);
              const dKey = 'other';
              let arr = this.fileStats.get(dKey);
              if (!arr) {
                arr = [];
                this.fileStats.set(dKey, arr);
              }
              arr.push([d, dMatch ? 'valid' : 'ignore']);
            }
          }
        }

        const fsToSort = this.fileStats.get('other')!;
        sortByReject(fsToSort, fsToSort.map(x => `${x[0]},${x[1]}`));
        // Vue doesn't track reactivity on Map objects.  So, re-assign with
        // copy.
        Vue.set(this, 'fileStats', new Map(this.fileStats));
        this.fileStatsSearchUpdate();
      }
    },
  },
});
</script>

