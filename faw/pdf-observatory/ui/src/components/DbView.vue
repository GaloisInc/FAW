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
    collection: String,
  },
  components: {
  },
  data() {
    return {
      error: null,
      items: null as any,
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
      if (!this.pdf || !this.collection) return;
      this.error = null;
      try {
        const data = await this.$vuespa.call('load_db', this.pdf, this.collection);
        if (this.collection === 'rawinvocations') {
          const items = {} as any;
          for (const d of data) {
            const cmdline = d.invoker.invName;//[d.invoker.exec, ...d.invoker.preArgs, ...d.invoker.postArgs].join(' ');
            if (Object.hasOwnProperty.call(items, d)) {
              throw new Error(`Tool ${cmdline} ran twice?`);
            }
            items[cmdline] = d;
          }
          this.items = items;
        }
        else if (this.collection === 'invocationsparsed') {
          const items = {} as any;
          for (const d of data) {
            if (Object.hasOwnProperty.call(items, d.parser)) {
              throw new Error(`Parser ${d.parser} seen twice?`);
            }
            items[d.parser] = d;
          }
          this.items = items;
        }
        else if (this.collection === 'statsbyfile') {
          this.items = data[0];
        }
      }
      catch (e) {
        this.items = null;
        this.error = e;
        throw e;
      }
    }
  },
});
</script>
