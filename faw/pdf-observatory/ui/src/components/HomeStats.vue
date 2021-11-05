<template lang="pug">
  span {{stats()}}
</template>

<script lang="ts">

import {PdfDecision} from './common';

import Vue from 'vue';

export default Vue.extend({
  props: {
    decisionAspectSelected: String,
    pdfs: Array as () => PdfDecision[],
    pdfsReference: Array as () => PdfDecision[],
  },
  methods: {
    stats() {
      const n = this.pdfs.length;

      let changed = 0;

      const attr = this.decisionAspectSelected;
      const counts = new Map<any, number>();
      for (const p of this.pdfs) {
        const old = counts.get(p[attr]) || 0;
        counts.set(p[attr], old + 1);

        if (p.changed) {
          changed += 1;
        }
      }
      let header = Array.from(counts.entries());
      header.sort((a, b) => a[0].toLocaleString().localeCompare(b[0].toLocaleString()));
      let hheader = header.map(k => `${k[1]} ${k[0]}`).join(', ');
      let tchanged = (this.pdfsReference.length === 0
          || this.pdfs === this.pdfsReference ? '' : `, ${changed} changed`);

      return hheader + tchanged;
    },
  },
});
</script>