<template lang="pug">
  .status
    //- No longer needed, as info now populated by observatory.
      checkmark(:status="decisionSelected.status")
      span {{decisionSelected.status}}
      .decision-info {{decisionSelected.info}}

    div Current decision
      json-tree(:data="decisionSelected" :level="2")

    div Reference decision
      json-tree(:data="decisionReference" :level="2")

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

        .decision-reason(v-for="k of fileStats.get(f.name)"
            :key="k[0]"
            )
          checkmark(:status="k[1]")
          span {{k[0]}}

    .decision-reasons Full listing of reasons (#[checkmark(:status="'valid'")] for filters: passed 'all' or rejected by 'any'):
      .decision-reason(v-for="k of fileStats.get('other')"
          :key="k[0]"
          @click="filterToggle(k[0], true)"
          )
        checkmark(:status="k[1]")
        span {{k[0]}}
</template>

<style lang="scss">
// Inherited from Home.vue
</style>

<script lang="ts">
import Vue from 'vue';

import CheckmarkComponent from '@/components/Checkmark.vue';
import {PdfDecision, sortByReject} from '@/components/common';
import {DslResult} from '@/dsl';

export default Vue.extend({
  components: {
    checkmark: CheckmarkComponent,
  },
  props: {
    decisionDefinition: Object as () => DslResult | null,
    decisionSelected: Object as () => PdfDecision,
    decisionSelectedDsl: Object as () => PdfDecision,
    decisionReference: Object as () => PdfDecision,
  },
  data() {
    return {
      fileStats: new Map<string, Array<[string, string]>>(),
    };
  },
  watch: {
    decisionSelectedDsl() {
      this.updateDecisionReasons();
    },
  },
  methods: {
    async updateDecisionReasons() {
      this.fileStats.clear();
      this.fileStats.set('other', []);

      const tf = this.decisionSelectedDsl.testfile;
      if (tf) {
        const data = await this.$vuespa.call('load_db', tf, 'statsbyfile');
        // Prevent duplicates by re-setting the array whenever we get data
        this.fileStats.clear();
        this.fileStats.set('other', []);
        const otherSeen = new Set<string>();
        for (const [d, dv] of Object.entries(data[0])) {
          if ([0, false, null, undefined].indexOf(dv as any) !== -1 || ['_id'].indexOf(d) !== -1) {
            continue;
          }
          const info = this.decisionSelectedDsl.info;
          const dRe = new RegExp(`^'([^']*)' (rejected|accepted) '${RegExp.escape(d)}'$`, 'gm');
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
      }
    },
  },
});
</script>

