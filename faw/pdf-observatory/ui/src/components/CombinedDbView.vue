<template lang="pug">
  .db-view
    .error(v-if="error" style="white-space: pre-wrap") {{error}}
    JsonTree(:data="items" :level="1")
</template>


<style lang="scss">
  .db-view {
  }
</style>


<script lang="ts">

import Vue from 'vue';

export default Vue.extend({
  props: {
    pdf: String,
  },
  components: {
  },
  data() {
    return {
      error: null as Error | null,
      items: null as {string: any} | null,
    };
  },
  watch: {
    pdf() {
      this.update();
    },
    collection() {
      this.update();
    },
  },
  mounted() {
    this.update();
  },
  methods: {
    async update() {
      if (!this.pdf) return;
      this.error = null;
      try {
        const data_rawinvocations = await this.$vuespa.call('load_db', this.pdf, 'rawinvocations');
        const data_invocationsparsed = await this.$vuespa.call('load_db', this.pdf, 'invocationsparsed');

        const items_rawinvocations = {} as any;
        for (const d of data_rawinvocations) {
          const cmdline = d.invoker.invName;//[d.invoker.exec, ...d.invoker.preArgs, ...d.invoker.postArgs].join(' ');
          if (Object.hasOwnProperty.call(items_rawinvocations, d)) {
            throw new Error(`Tool ${cmdline} ran twice?`);
          }
          items_rawinvocations[cmdline] = d;
        }
        const items_invocationsparsed = {} as any;
        for (const d of data_invocationsparsed) {
          if (Object.hasOwnProperty.call(items_invocationsparsed, d.parser)) {
            throw new Error(`Parser ${d.parser} seen twice?`);
          }
          items_invocationsparsed[d.parser] = d;
        }

        const items = {} as any;
        for (const [k, v] of Object.entries(items_rawinvocations)) {
          items[k] = {'tool_output': v, 'parser_output': null};
        }
        for (const [k, v] of Object.entries(items_invocationsparsed)) {
          if (!(k in items)) {
            items[k] = {'tool_output': null, 'parser_output': v};
          } else {
            items[k]['parser_output'] = v;
          }
        }
        this.items = items;
      }
      catch (e: any) {
        this.items = null;
        this.error = e;
        throw e;
      }
    }
  },
});
</script>
