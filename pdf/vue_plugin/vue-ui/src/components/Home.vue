<template lang="pug">
  div(style="position: absolute; top: 0; left: 0; right: 0; bottom: 0; overflow: hidden")
    svg(ref="svg" width="100%" height="100%")
</template>

<style lang="scss">
.tooltip {
  background-color: white;
  border: solid 2px #000;
  border-radius: 0.5rem;
  padding: 0.2rem;
  position: absolute;
}
</style>

<script lang="ts">
import Vue from 'vue';

import * as d3 from 'd3';

interface WorkbenchData {
  apiUrl: string;
  files: Array<{testfile: string; fts: Array<string>}>;
}

export default Vue.extend({
  data() {
    return {
      workbench: {apiUrl: '<api>', files: []} as WorkbenchData,
    };
  },
  mounted() {
    this.workbench = (window as any).workbenchArgs;

    // Demo based on https://observablehq.com/@d3/chord-dependency-diagram

    // First, convert data to matrix form
    const data: Array<Array<number>> = [];
    const examples: Array<string> = [];
    const nameByIndex: Array<string> = [];
    const features: {[name: string]: number} = {};
    for (const i of this.workbench.files) {
      for (const j of i.fts) {
        if (features[j] === undefined) {
          features[j] = data.length;
          nameByIndex.push(j);
          examples.push(i.testfile);

          for (const z of data) z.push(0);
          if (data.length === 0)
            data.push([0])
          else
            data.push(data[data.length - 1].map(x => 0));
        }
      }

      for (const j of i.fts) {
        for (const k of i.fts) {
          data[features[j]!][features[k]!] += 1;
          if (j !== k)
            data[features[k]!][features[j]!] += 1;
        }
      }
    }

    const svg = d3.select(this.$refs.svg as Element);

    const box = svg.node()!.getBoundingClientRect();
    const width = box.width;
    const height = box.height;
    const outerRadius = Math.min(width, height) * 0.45;
    const innerRadius = outerRadius - 100;

    // Laze led to these d3 objects being declared type `any` -- d3's example
    // is not friendly to typescript.
    const chord = d3.chord()
      .padAngle(0.04)
      .sortSubgroups(d3.descending)
      .sortChords(d3.descending)

    const arc: any = d3.arc()
      .innerRadius(innerRadius)
      .outerRadius(outerRadius)

    const ribbon: any = d3.ribbon()
      .radius(innerRadius)

    const color: any = d3.scaleOrdinal(d3.schemeCategory10)

    svg
      .attr('viewBox', [-0.5 * width, -0.5 * height, width, height] as any)
      .attr("font-size", 12)
      .attr("font-family", "sans-serif")
      .style("width", "100%")
      .style("height", "auto");

    const chords = chord(data);

    const group = svg.append("g")
      .selectAll("g")
      .data(chords.groups)
      .join("g");

    // Added tooltip function
    const Tooltip = d3.select(this.$el)
      .append('div')
      .style('opacity', 0)
      .attr('class', 'tooltip')
    const bind = (el: any) => {
      el.on('mouseover', (d: any) => {
        Tooltip
          .style('opacity', 1)
          .style('left', d3.event.x + 'px')
          .style('top', d3.event.y + 'px')
          .text(nameByIndex[d.index])
      });
      el.on('mouseleave', (d: any) => {
        Tooltip
          .style('opacity', 0)
      });
    };

    const p = group.append("path")
        .attr("fill", d => color(d.index))
        .attr("stroke", d => color(d.index))
        .attr("d", arc)
        .attr('cursor', 'pointer')
        .on('click', d => {
          const u = new XMLHttpRequest();
          u.open('get', this.workbench.apiUrl + 'showFile?id=' + escape(examples[d.index]), true);
          u.send();
        })
    bind(p)

    group.append("text")
        .each(d => { (d as any).angle = (d.startAngle + d.endAngle) / 2; })
        .attr("dy", ".35em")
        .attr("transform", d => `
          rotate(${((d as any).angle * 180 / Math.PI - 90)})
          translate(${innerRadius + 26})
          ${(d as any).angle > Math.PI ? "rotate(180)" : ""}
        `)
        .attr("text-anchor", d => (d as any).angle > Math.PI ? "end" : null)
        .text(d => nameByIndex[d.index]);

    svg.append("g")
        .attr("fill-opacity", 0.67)
      .selectAll("path")
      .data(chords)
      .join("path")
        .attr("stroke", (d: any) => d3.rgb(color(d.source.index)).darker() as any)
        .attr("fill", (d: any) => color(d.source.index))
        .attr("d", ribbon as any);
  },
});
</script>
