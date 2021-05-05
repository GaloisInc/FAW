<template lang="pug">
  v-expansion-panel
    v-expansion-panel-header
      div(style="display: inline-block")
        v-icon(v-if="status.done" color="green") mdi-check-outline
        v-icon(v-else-if="status.disabled" color="gray") mdi-close-outline
        v-icon(v-else color="red") mdi-progress-clock
        v-icon(v-if="status.disabled_by_config" color="red") mdi-do-not-disturb
        span {{pipeline}} {{task}} -- status: {{status.status_msg}}
    v-expansion-panel-content
      json-tree(:data="status.last_run_info" :level="1")
      v-dialog(v-model="resetDbDialog" persistent max-width="800")
        template(v-slot:activator="{on}")
          v-btn.resetdb(v-on="on") Reset task DB
        // .home turns the yes button red
        v-card.home
          v-card-title Delete all task information, forcing it to start from scratch?
          v-card-actions(:style={'flex-wrap': 'wrap'})
            v-btn(@click="resetDbDialog=false") Cancel
            v-btn.resetdb(@click="dbReset(); resetDbDialog=false") Yes
      v-btn(@click="dbToggleDisabled") {{status.disabled_by_ui ? 'Enable' : 'Disable'}}
</template>

<style lang="scss">
</style>

<script lang="ts">
import Vue from 'vue';

export default Vue.extend({
  props: {
    pipeline: String,
    task: String,
  },
  data() {
    return {
      resetDbDialog: false,
      running: true,
      status: {} as any,
    };
  },
  mounted() {
    (async () => {
      while (this.running) {
        try {
          await this.reload();
        }
        catch (e) {
          console.error(e);
        }
        await new Promise((resolve) => setTimeout(resolve, 10000));
      }
    })();
  },
  beforeDestroy() {
    this.running = false;
  },
  methods: {
    async dbReset() {
      await this.$vuespa.call('pipeline_task_reset', this.pipeline, this.task);
      await this.reload();
    },
    async dbToggleDisabled() {
      await this.$vuespa.call('pipeline_task_set_disabled', this.pipeline,
          this.task, !this.status.disabled_by_ui);
      await this.reload();
    },
    async reload() {
      await this.$vuespa.update('status', 'pipeline_task_status',
          this.pipeline, this.task);
    },
  },
});
</script>

