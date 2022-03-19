<template lang="pug">
v-expansion-panel
  v-expansion-panel-header(:color="asetData !== undefined \
      ? (asetData.done ? 'green' : 'yellow darken-2') \
      : 'grey lighten-2'")
    div
      span(style="margin-right: 0.25em")
        v-icon(v-if="asetData === undefined") mdi-cancel
        v-icon(v-else-if="asetData.done") mdi-check-outline
        v-icon(v-else) mdi-progress-clock
      span {{cfg.label || pipeline}}
  v-expansion-panel-content
    v-btn(@click="pipelineToggleClick")
      span(v-if="asetData !== undefined")
        v-icon mdi-cancel
        span Abort / delete pipeline
      span(v-else)
        span Start pipeline

    v-dialog(v-model="pipelineDeleteDialog" persistent max-width="800")
      template(v-slot:activator="{on}")
      v-card
        v-card-title Delete all pipeline information and stop its execution?
        v-card-actions(:style={'flex-wrap': 'wrap'})
          v-btn(@click="pipelineDeleteDialog=false") Cancel
          v-btn(@click="pipelineDeleteYes()" color="red") Delete and stop

    div(v-if="asetData !== undefined")
      div Tasks
      v-expansion-panels
        template(v-for="task of Object.keys(cfg.tasks)")
          AnalysisSetPipelineTaskInfo(:key="task"
              :aset="aset"
              :pipeline="pipeline"
              :task="task")
</template>
<style lang="scss">
</style>
<script lang="ts">
import bus from '@/bus';
import AnalysisSetPipelineTaskInfo from '@/components/AnalysisSetPipelineTaskInfo.vue';
import {AsPipeline} from '@/interface/aset.ts';
import Vue from 'vue';

export default Vue.extend({
  components: {
    AnalysisSetPipelineTaskInfo,
  },
  props: {
    aset: String,
    asetData: Object as () => AsPipeline|undefined,
    cfg: Object as () => any,
    pipeline: String,
  },
  data() {
    return {
      pipelineDeleteDialog: false,
    };
  },
  mounted() {
    console.log(this.pipeline);
  },
  methods: {
    pipelineToggleClick() {
      (async () => {
        if (this.asetData === undefined) {
          await this.$vuespa.call('analysis_set_pipeline_start', this.aset,
              this.pipeline);
        }
        else {
          this.pipelineDeleteDialog = true;
        }
        this.$emit('update');
      })().catch(bus.error);
    },
    pipelineDeleteYes() {
      (async () => {
        await this.$vuespa.call('analysis_set_pipeline_delete', this.aset,
            this.pipeline);
        this.$emit('update');
        this.pipelineDeleteDialog = false;
      })().catch(bus.error);
    },
  },
});
</script>
