<template>
  <div class="svg-container" />
</template>

<script lang="ts">
import Vue from "vue";
import { PdfDecision } from "@/common/common";
import * as d3 from "d3";
import { DslResult } from "@/common/dsl";

type StatusId = number;

interface Statuses {
  [aspect: string]: Map<any, number> | undefined;
}

interface Centers {
  [aspect: string]: {
    [statusId: number]: number | undefined;
  } | undefined;
}
interface Colors {
  [aspect: string]: {
    [statusId: number]: string | undefined;
  } | undefined;
}
interface NodeStatuses {
  [testfile: string]: { [aspect: string]: StatusId | undefined } | undefined;
}
interface FakeColorToNode {
  [color: string]: Node | undefined;
}
interface Node {
  testfile: string;
  centers: Centers;
  colors: Colors;
  x: number;
  y: number;
  id: number;
}
interface NodeCollection {
  statuses: NodeStatuses;
  ref_statuses: NodeStatuses;
  nodes: Node[];
}

const dotWidth = 5
const width = 1500;
const height = 150;
const y_center = Math.floor(height / 2);
const max_nodes = 750;

export default Vue.extend({
  name: "CirclePlot",
  props: {
    pdfs: Array as () => PdfDecision[],
    pdfsReference: Array as () => PdfDecision[],
    pdfsSearched: Object,
    decisionDefinition: Object,
    decisionAspectSelected: String,
  },
  data() {
    return {
      centers: {} as Centers,
      colors: {} as Colors,
      decisionDefinitionDirty: false as boolean,
      fakeColorToNode: {} as FakeColorToNode,
      pdfsMouseOver: null as any,
      selections: {
        canvas: {} as any,
        hiddenCanvas: {} as any,
        annotation: {} as any,
        context: {} as any,
        hiddenContext: {} as any,
        div: {} as any
      },
      simulation: null as any,
      statuses: {} as Statuses,
      node: {
        statuses: {},
        ref_statuses: {},
        nodes: [],
      } as NodeCollection,
    };
  },
  model: {
    prop: "pdfsSearched",
    event: "pdfsSearched"
  },
  mounted() {
    this.populateParams(this.decisionDefinition);
    this.node = this.nodeInit(this.statuses);
    this.selections.canvas = d3
      .select(this.$el)
      .append("canvas")
      .attr("width", width)
      .attr("height", height)
      .on("click", this.onClick)
      .on("mousemove", this.onMouseOver);
    this.selections.hiddenCanvas = d3
      .select(this.$el)
      .append("canvas")
      .attr("width", width)
      .attr("height", height)
      .style("display", "none");

    this.canvasRender(this.node.nodes);
  },
  watch: {
    decisionAspectSelected() {
      this.canvasUpdate(this.node.nodes);
    },
    pdfs(new_pdfs, old_pdfs) {
      if (this.decisionDefinitionDirty) {
        this.populateParams(this.decisionDefinition);
      }

      this.node = this.nodeInit(this.statuses);
      this.canvasRender(this.node.nodes);
    },
    pdfsMouseOver() {
      this.tick();
    },
    pdfsReference(new_refs, old_refs) {
      if (this.decisionDefinitionDirty) {
        this.populateParams(this.decisionDefinition);
      }

      this.node = this.nodeInit(this.statuses);
      this.canvasRender(this.node.nodes);
    },
    pdfsSearched() {
      this.tick();
    },
    decisionDefinition(new_dec, old_dec) {
      this.decisionDefinitionDirty = true;
    }
  },
  methods: {
    canvasUpdate(nodes: Node[]) {
      const simulation = this.simulation;

      simulation.alpha(1).restart();
      simulation.force("x").initialize(nodes);
    },
    canvasRender(nodes: Node[]) {
      var canvas = this.selections.canvas;

      var context = canvas.node().getContext("2d");
      context.clearRect(0, 0, width, height);
      this.selections.context = context;

      var hiddenCanvas = this.selections.hiddenCanvas;

      var hiddenContext = hiddenCanvas.node().getContext("2d");
      context.clearRect(0, 0, width, height);
      this.selections.hiddenContext = hiddenContext;

      if (this.simulation !== null) {
        this.simulation.stop();
      }
      this.simulation = d3
        .forceSimulation(nodes)
        .force("collision", d3.forceCollide(6).iterations(2))
        .force("x", d3.forceX((d: any) => this.nodeCenter(d)).strength(0.05))
        .force("y", d3.forceY(y_center).strength(0.05))
        .on("tick", this.tick);
    },
    tick() {
      this.draw(true);
      this.draw(false);
    },
    draw(hidden: boolean) {
      var color = hidden ? this.fakeColor : this.nodeColor;
      var context = hidden
        ? this.selections.hiddenContext
        : this.selections.context;

      context.clearRect(0, 0, width, height);
      const circles = this.node.nodes;
      const highlight = hidden ? null : !this.pdfsSearched ? null : this.pdfsSearched.testfile;
      circles.forEach((d: any) => {
        context.beginPath();
        context.arc(d.x, d.y, dotWidth, 0, 2 * Math.PI);
        context.fillStyle = color(d);
        context.fill();
        context.lineWidth = 1;
        context.strokeStyle = hidden ? color(d) : "black";
        context.stroke();

        if (highlight === d.testfile) {
          context.beginPath();
          context.arc(d.x, d.y, dotWidth + 1, 0, 2 * Math.PI);
          context.lineWidth = 4;
          context.strokeStyle = "black";
          context.stroke();
          context.lineWidth = 2;
          context.strokeStyle = "red";
          context.stroke();
        }
      });

      if (!hidden && this.pdfsMouseOver) {
        context.beginPath();
        context.arc(this.pdfsMouseOver.x, this.pdfsMouseOver.y, dotWidth + 1, 0, 2 * Math.PI);
        context.lineWidth = 2;
        context.strokeStyle = "black";
        context.stroke();
      }

      this.annotate(circles, this.decisionAspectSelected, hidden);
    },
    annotate(nodes: any, aspect: string, hidden: Boolean) {
      if (!hidden) {
        var context = this.selections.context;
        const a = this.annotationObject(nodes, aspect);
        a.forEach((d: any) => {
          context.beginPath();
          // Circle bounding document dots
          context.arc(d.x, d.y, d.subject.radius, 0, 2 * Math.PI);
          context.lineWidth = 1;
          context.strokeStyle = d.color;
          context.stroke();

          // Text label with background
          context.font = "16px Arial";
          context.textBaseline = "top";
          context.fillStyle = "rgba(255,255,255,0.85)";
          const textWidth = context.measureText(d.label).width;
          context.fillRect(d.x + d.dx, d.y + d.dy, textWidth, 16);

          //context.fill();
          context.fillStyle = d.color;
          context.fillText(d.label, d.x + d.dx, d.y + d.dy);
        });
      }
    },
    annotationObject(nodes: any, aspect: string) {
      const ss = this.statuses[aspect];
      if (ss === undefined) return [];

      const points = Array.from(ss.entries()).map(e => {
        const [, statusId] = e;
        return nodes
          .filter(
            (d: any) => this.node.statuses[d.testfile]?.[aspect] === statusId
          )
          .map((d: any) => ({ x: d.x, y: d.y, r: 5 }));
      });
      const circle = points.map((p, i) => {
        if (p.length) {
          return d3.packEnclose(p);
        } else {
          // Default when there are no elements with a particular status
          return { x: this.centers[aspect]?.[i] || 0, y: y_center, r: 2 };
        }
      });
      const annotes = Array.from(ss.entries()).map(v => {
        const [status, statusId] = v;
        const color = this.colors[aspect]?.[statusId];
        return {
          label: status,
          dy: 50,
          dx: circle[statusId].r,
          x: circle[statusId].x,
          y: circle[statusId].y,
          color: color,
          subject: {
            radius: circle[statusId].r,
            radiusPadding: 10
          }
        };
      });
      return annotes;
    },
    onClick() {
      // Click based on rendered highlight.
      if (this.pdfsMouseOver === null) return;

      const node = this.pdfsMouseOver;
      const pdf: PdfDecision = this.pdfs.filter(
        (p: PdfDecision) => p.testfile === node.testfile
      )[0];
      this.$emit("pdfsSearched", pdf);

      // Clear whatever the mouse was over.
      this.pdfsMouseOver = null;
    },
    onMouseOver() {
      var event = d3.event;
      const clientRect = this.selections.canvas.node().getBoundingClientRect();
      var mouseX = event.clientX - clientRect.left;
      var mouseY = event.clientY - clientRect.top;

      var col = this.selections.hiddenContext.getImageData(mouseX, mouseY, 1, 1)
        .data;
      var colString = "rgb(" + col[0] + "," + col[1] + "," + col[2] + ")";
      var node = this.fakeColorToNode[colString];
      if (node) {
        this.pdfsMouseOver = node;
      }
      else {
        this.pdfsMouseOver = null;
      }
    },
    fakeColor(node: Node) {
      const color = this.genColor(node);
      this.fakeColorToNode[color] = node;
      return color;
    },
    genColor(node: Node) {
      var ret = [];
      const id = 10 + node.id * 2;
      // via http://stackoverflow.com/a/15804183
      if (id < 16777215) {
        ret.push(id & 0xff); // R
        ret.push((id & 0xff00) >> 8); // G
        ret.push((id & 0xff0000) >> 16); // B
      }
      var col = "rgb(" + ret.join(",") + ")";
      return col;
    },
    updateNodeStatuses(data: PdfDecision[], status_obj: NodeStatuses) {
      const das = Object.keys(this.statuses);
      for (const d of data) {
        for (let da of das) {
          const id = this.statuses[da]?.get(d[da]) || 0;
          const objToSet = status_obj[d.testfile];
          if (objToSet) objToSet[da] = id;
        }
      }
      this.pdfsMouseOver = null;
      return status_obj;
    },
    updateNodeStatuses_selected(data: PdfDecision[], status_obj: NodeStatuses) {
      const da = this.decisionAspectSelected;
      for (const d of data) {
        const id = this.statuses[da]?.get(d[da]) || 0;
        const objToSet = status_obj[d.testfile];
        if (objToSet) objToSet[da] = id;
      }
      this.pdfsMouseOver = null;
      return status_obj;
    },
    updateNodeFeatures() {
      for (let node of this.node.nodes) {
        node.centers = this.centers;
        node.colors = this.colors;
      }
    },
    nodeColor(node: any) {
      const bad_color = 'rgb(255, 255, 255)';
      const status_id = this.node.ref_statuses[node.testfile]?.[
        this.decisionAspectSelected
      ];
      if (status_id === undefined) return bad_color;
      const colors = this.colors[this.decisionAspectSelected]?.[status_id];
      if (colors === undefined) return bad_color;
      return colors;
    },
    nodeCenter(node: any): number {
      const status_id = this.node.statuses[node.testfile]?.[
        this.decisionAspectSelected
      ];
      if (status_id === undefined) return 0.;
      const centers = this.centers[this.decisionAspectSelected];
      if (centers === undefined) {
        return 0.;
      }
      return centers[status_id] || 0.;
    },
    nodeInit(statuses: Statuses): NodeCollection {
      let node_statuses: NodeStatuses = {};
      const nodes: Node[] = [];
      let i = 0;
      const decisionAspects = Object.keys(statuses);
      const num_pdfs = this.pdfs.length;
      const c = num_pdfs > max_nodes ? Math.ceil(num_pdfs / max_nodes) : 1;
      const seenIt: Array<string> = [];
      for (const pdf of this.pdfs) {
        var addThisNode = true;
        var inner: { [aspect: string]: StatusId } = {};
        for (const da of decisionAspects) {
          const st = pdf[da];
          const id = statuses[da]?.get(st);
          inner[da] = id ? id : 0;

          const elem = da.concat("-", st);
          if (seenIt.includes(elem)) addThisNode = false;
          else seenIt.push(elem);
        }
        node_statuses[pdf.testfile] = inner;

        if (i % c == 0 || addThisNode) {
          nodes.push({
            testfile: pdf.testfile,
            centers: this.centers,
            colors: this.colors,
            x:
              (this.centers[this.decisionAspectSelected]?.[0] || 0.)
              + 50 * Math.random(),
            y: y_center + 50 * Math.random(),
            id: i as number
          });
        }

        i += 1;
      }

      const ref_statuses = this.pdfsReference.reduce(
        (obj: NodeStatuses, pdf: PdfDecision) => {
          var inner: { [aspect: string]: StatusId } = {};
          for (const da of decisionAspects) {
            const id = statuses[da]?.get(pdf[da]) || 0;
            inner[da] = id;
          }
          obj[pdf.testfile] = inner;
          return obj;
        },
        {}
      );
      return {
        statuses: node_statuses,
        ref_statuses: ref_statuses,
        nodes: nodes
      };
    },
    populateParams(decisionDefinition: DslResult) {
      var statuses: Statuses = {};
      for (let [aspect, ss] of decisionDefinition.outputs) {
        let i = 0;
        let inner = new Map<string, number>();
        for (let s of ss) {
          inner.set(s[0], i++);
        }
        inner.set("unspecified", i++);
        statuses[aspect] = inner;
      }
      for (let filter of decisionDefinition.filters) {
        let inner = new Map<any, number>();
        inner.set(true, 0);
        inner.set(false, 1);
        inner.set("unspecified", 2);
        statuses["filter-" + filter.name] = inner;
      }
      this.statuses = statuses;

      var centers: Centers = {};
      for (let [aspect, ss] of Object.entries(statuses)) {
        const num_statuses = ss!.size;
        const w = Math.floor(width / (num_statuses + 1));
        const inner = Array.from(ss!.values()).reduce((obj: any, v: number) => {
          obj[v] = (v + 1) * w;
          return obj;
        }, {});
        centers[aspect] = inner;
      }
      this.centers = centers;

      var spectrum = d3.interpolateCubehelixLong("purple", "orange");
      var colors: Colors = {};
      for (let [aspect, ss] of Object.entries(statuses)) {
        const num_statuses = ss!.size;
        const inner = Array.from(ss!.values()).reduce((obj: any, v: number) => {
          obj[v] = spectrum(v / (num_statuses - 1));
          return obj;
        }, {});
        colors[aspect] = inner;
      }
      this.colors = colors;
    }
  }
});
</script>

<style scoped>
</style>
