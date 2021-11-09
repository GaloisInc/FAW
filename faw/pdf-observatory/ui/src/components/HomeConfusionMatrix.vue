<template lang="pug">
  table
    tr
      td Decision \\ Reference
      td(v-for="r of references" :key="r") {{r === undefined ? 'undefined' : r}}
    tr(v-for="d, di of decisions" :key="d")
      td {{d === undefined ? 'undefined' : d}}

      template(v-for="r, ri of references")
        template(v-if="counts[di][ri]")
          v-menu(offset-y :key="r")
            template(v-slot:activator="{on}")
              td.clickable(
                  v-on="on"
                  :style="(di !== ri) ? 'background-color: ' + spectrum(counts[di][ri]) : 'background-color: #ddf'"
                  )  {{counts[di][ri].toFixed(1)}}%
            v-list
              v-list-item
                v-btn(@click="$emit('filter-file-list', {name: fileFilterListName(di, ri), files: examples[di][ri]})") Filter in FAW
                v-btn(v-clipboard="() => JSON.stringify(examples[di][ri])") (Copy file list as JSON)
                v-btn(@click="$emit('view', randomInList(examples[di][ri]))") (View random file)
                span(style="margin-left: 0.25em") {{examples[di][ri].length}} files
              v-list-item(v-for="ex of examples[di][ri].slice(0, 10)" :key="ex" @click="$emit('view', ex)") {{ex}}
        td(v-else) 0
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
    fileFilterListName(di: number, ri: number): string {
      const r = [
        this.decisionAspectSelected,
        ' == ',
        this.decisions[di] === undefined ? 'undefined' : this.decisions[di],
        ' (',
        this.references[ri] === undefined ? 'undefined' : this.references[ri],
        ')',
      ];
      return r.join('');
    },
    randomInList(v: Array<any>) {
      const i = Math.min(v.length - 1, Math.floor(Math.random() * v.length));
      return v[i];
    },
    update() {
      const dVals: Array<string> = [];
      const counts = new Array<Array<number>>();
      const examples = new Array<Array<Array<string>>>();

      const map = new Map<string, number>();

      const asp = this.decisionAspectSelected;
      const undefinedStr = 'undefined';
      for (const [count, v, arr] of [[false, dVals, this.pdfsReference], [true, dVals, this.pdfs]] as [boolean, Array<string|undefined>, Array<PdfDecision>][]) {
        if (count) {
          while (counts.length < v.length) {
            counts.push([]);
            examples.push([]);
          }
        }
        for (const a of arr) {
          let av = a[asp];
          if (av === undefined) av = undefinedStr;

          let i = v.indexOf(av);
          if (i === -1) {
            i = v.length;
            v.push(av);
          }

          if (!count) {
            map.set(a.testfile, i);
          }
          else {
            let di = map.get(a.testfile);
            if (di === undefined) {
              // A real-world file which has no matching reference file.
              di = v.indexOf(undefinedStr);
              if (di === -1) {
                di = v.length;
                v.push(undefinedStr);
              }
            }

            while (counts.length <= Math.max(i, di)) {
              counts.push([]);
              examples.push([]);
            }
            const dc = counts[i];
            const de = examples[i];
            while (dc.length <= di) {
              dc.push(0);
              de.push([]);
            }
            dc[di] += 1;
            de[di].push(a.testfile);
          }
        }
      }

      // Sort, squarify so diagonal is always right.
      const u = dVals.slice();
      u.sort((a, b) => a === undefinedStr ? 1 : b === undefinedStr ? -1 : a.toString().localeCompare(b.toString()));

      let countsNew: Array<Array<number>> = [], examplesNew: Array<Array<Array<string>>> = [];
      for (let i = 0, m = u.length; i < m; i++) {
        const di = dVals.indexOf(u[i]);
        countsNew.push(counts[di]);
        examplesNew.push(examples[di]);
      }

      for (let i = 0, m = u.length; i < m; i++) {
        const cc = countsNew[i];
        const ee = examplesNew[i];
        const ccNew = [];
        const eeNew = [];
        for (let j = 0, k = u.length; j < k; j++) {
          const ri = dVals.indexOf(u[j]);
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
        countsNew[i] = ccNew.map(x => x * 100 / this.pdfs.length);
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
