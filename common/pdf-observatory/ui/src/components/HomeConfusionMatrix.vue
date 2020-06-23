<template lang="pug">
  table
    tr
      td Decision \\ Reference
      td(v-for="r of references" :key="r") {{r === undefined ? 'undefined' : r}}
    tr(v-for="d, di of decisions" :key="d")
      td {{d === undefined ? 'undefined' : d}}
      td(v-for="r, ri of references"
          :class="{clickable: counts[di][ri]}"
          :style="(di !== ri) ? 'background-color: ' + spectrum(counts[di][ri]) : 'background-color: #ddf'"
          @click="onClick(di, ri)"
          )
        template(v-if="counts[di][ri]")
          v-menu(offset-y)
            template(v-slot:activator="{on}")
              span(v-on="on")  {{counts[di][ri].toFixed(1)}}%
            v-list
              v-list-item(v-for="ex of examples[di][ri]" :key="ex" @click="$emit('view', ex)") {{ex}}
        span(v-else) 0
</template>

<style scoped lang="scss">
  @import '~vuetify/src/styles/settings/_colors.scss';

  table {
    border-collapse: collapse;
    text-align: center;

    margin-left: auto;
    margin-right: auto;

    tr:first-child td:first-child {
    }

    tr:first-child td, tr td:first-child {
      background-color: map-get($blue, 'darken-2');
      color: white;
      text-align: center;
      padding: 0.5rem;
    }

    tr:not(:first-child) td:not(:first-child) {
      font-size: 2em;
      padding: 0.5rem;

      &.clickable:hover {
        text-decoration: underline;
        cursor: pointer;
      }
    }
  }
</style>

<script lang="ts">
import * as d3 from "d3";
import Vue from 'vue';

import { PdfDecision } from "./common";

export default Vue.extend({
  props: {
    decisionAspectSelected: String,
    pdfs: Array as () => Array<PdfDecision>,
    pdfsReference: Array as () => Array<PdfDecision>,
  },
  data() {
    return {
      decisions: new Array<string|undefined>(),
      references: new Array<string|undefined>(),
      counts: new Array<Array<number>>(),  // decisions, references
      examples: new Array<Array<Array<string>>>(),  // decisions, references, samples

      spectrum: ((v: number) => '#ffffff') as {(v:number): string},
    };
  },
  watch: {
    decisionAspectSelected() {
      this.update();
    },
    pdfs() {
      this.update();
    },
    pdfsReference() {
      this.update();
    },
  },
  mounted() {
    this.update();
  },
  methods: {
    onClick(di: number, ri: number) {
      const ex = this.examples[di][ri];
      if (!ex || !ex.length) {
        return;
      }

      this.$emit('view', ex[0]);
    },
    update() {
      let sawUndefined = false;
      const dVals: Array<string|undefined> = [undefined];
      const rVals: Array<string|undefined> = [undefined];
      const counts = new Array<Array<number>>();
      const examples = new Array<Array<Array<string>>>();

      const map = new Map<string, number>();

      const asp = this.decisionAspectSelected;
      for (const [count, v, arr] of [[false, dVals, this.pdfs], [true, rVals, this.pdfsReference]] as [boolean, Array<string|undefined>, Array<PdfDecision>][]) {
        for (const a of arr) {
          const av = a[asp];
          let i = v.indexOf(av);
          if (i === -1) {
            i = v.length;
            v.push(av);
          }

          if (av === undefined) {
            sawUndefined = true;
          }

          if (!count) {
            map.set(a.testfile, i);
          }
          else {
            let di = map.get(a.testfile);
            if (di === undefined) {
              di = 0;
            }
            
            while (counts.length <= di) {
              counts.push([]);
              examples.push([]);
            }
            const dc = counts[di];
            const de = examples[di];
            while (dc.length <= i) {
              dc.push(0);
              de.push([]);
            }
            dc[i] += 1;
            if (de[i].length < 10) {
              de[i].push(a.testfile);
            }
          }
        }
      }

      // Sort, squarify so diagonal is always right.
      const u = Array.from(new Set([...dVals, ...rVals]));
      if (!sawUndefined) {
        u.splice(u.indexOf(undefined), 1);
      }
      u.sort();
      
      let countsNew: Array<Array<number>> = [], examplesNew: Array<Array<Array<string>>> = [];
      for (let i = 0, m = u.length; i < m; i++) {
        const di = dVals.indexOf(u[i]);
        if (di === -1) {
          countsNew.push([]);
          examplesNew.push([]);
        }
        else {
          countsNew.push(counts[di]);
          examplesNew.push(examples[di]);
        }
      }

      for (let i = 0, m = u.length; i < m; i++) {
        const cc = countsNew[i];
        const ee = examplesNew[i];
        const ccNew = [];
        const eeNew = [];
        for (let j = 0, k = u.length; j < k; j++) {
          const ri = rVals.indexOf(u[j]);
          if (ri === -1) {
            ccNew.push(0);
            eeNew.push([]);
          }
          else {
            ccNew.push(cc[ri] || 0);
            eeNew.push(ee[ri] || []);
          }
        }
        // Also port to percentages
        countsNew[i] = ccNew.map(x => x * 100 / this.pdfsReference.length);
        examplesNew[i] = eeNew;
      }

      this.decisions = u;
      this.references = u;
      this.counts = countsNew;
      this.examples = examplesNew;
      
      const specFn = d3.interpolateCubehelix("white", "#9e9ac8");
      const vMax = Math.max(1e-20, ...this.counts.map((x, xi) => Math.max(...x.map((y, yi) => xi === yi ? 0 : y))));
      this.spectrum = (v) => specFn(v / vMax);
    }
  },
});
</script>
