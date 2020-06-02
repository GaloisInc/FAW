<template lang="pug">
  .home
    v-dialog(
        v-model=" \
          loadingStatus.files_max === 0 \
          || loadingStatus.files_done !== loadingStatus.files_max \
          || loadingStatus.files_err"
        hide-overlay
        persistent
        width="300"
        content-class="loadingStatusDialog"
        )
      v-card(color="grey darken-1" dark)
        v-card-text(style="padding-top: 0.5em") {{loadingStatus.message}}
          v-progress-linear(:height="8"
              :indeterminate="loadingStatus.files_done !== loadingStatus.files_max"
              rounded
              :color="loadingStatus.files_err === 0 ? 'white' : 'red'"
              :value="loadingStatus.files_done === loadingStatus.files_max ? 100 : 0")
    .error(v-if="error" style="font-size: 4em; white-space: pre-wrap") ERROR - SEE CONSOLE

    v-expansion-panels(:multiple="true" :popout="true" :value="expansionPanels" :class="{colored: true}")
      //- Filter configuration - collapsible
      v-expansion-panel(:key="0")
        v-expansion-panel-header Filters
        v-expansion-panel-content
          v-sheet(:elevation="3" style="padding: 1em; margin: 1em")
            div
              AceEditor(ref="decisionCodeEditor"
                  v-model="decisionCode"
                  lang="yaml"
                  @init="decisionCodeEditorInit"
                  style="font-size: 1em"
                  )
            div
              checkmark(:status="decisionDefinition ? 'valid' : 'rejected'")
              span compilation {{decisionDefinition ? 'succeeded' : 'failed'}}

          div(style="display: flex; flex-direction: row; flex-wrap: wrap")
            v-tooltip(bottom :disabled="!!decisionDefinition")
              template(v-slot:activator="{on}")
                //- Wrap disabled button in div -> show tooltip when disabled.
                div(v-on="on")
                  v-btn.reprocess(
                    @click="reprocess" :disabled="!decisionDefinition"
                    ) Reprocess decisions
              span(v-if="!decisionDefinition") Fix filter definition first.
            v-btn.download(@click="download") Download decisions
            v-btn(@click="resetDbErrors") Reprocess DB errors
            v-dialog(v-model="resetDbDialog" persistent max-width="390")
              template(v-slot:activator="{on}")
                v-btn.resetdb(v-on="on") Reset Entire DB (may take awhile)
              v-card
                v-card-title Reset entire DB, re-running all tools and parsers?
                v-card-actions
                  v-spacer
                  v-btn(@click="resetDbDialog=false") Cancel
                  v-btn(@click="reset(); resetDbDialog=false") Reset Entire DB
                  v-btn(@click="resetParsers(); resetDbDialog=false") Reset Most of DB, but not tool invocations

          v-expansion-panels(inset :style="{'margin-top': '1em'}")
            v-expansion-panel
              v-expansion-panel-header(:class="{'grey lighten-2': true}") Decision Plugins
              v-expansion-panel-content
                v-btn(v-for="[pluginKey, plugin] of Object.entries(config && config.decision_views || {})" 
                    :key="pluginKey"
                    @click="pluginDecisionView(pluginKey, {})") {{plugin.label}}
                v-btn(v-show="pluginDecIframeSrc != null || pluginDecIframeLoading" @click="pluginDecIframeSrc = null; pluginDecIframeLoading = 0") (Close current plugin)
                div(v-show="pluginDecIframeSrc != null || pluginDecIframeLoading" style="border: solid 1px #000; position: relative; height: 95vh")
                  v-progress-circular(v-show="pluginDecIframeLoading" :indeterminate="true")
                  iframe(v-show="pluginDecIframeSrc != null" style="width: 100%; height: 100%" ref="pluginDecIframe")

          v-sheet(:elevation="3" style="padding: 1em; margin: 1em")
            //- Allow selection of different things.
            div(v-if="decisionDefinition")
              v-radio-group(row v-model="decisionAspectSelected")
                v-radio(v-for="o of decisionAspectAvailable" :key="o[1]"
                    :label="o[0]" :value="o[1]")

          //- Plot of file statuses
          v-sheet(:elevation="3" style="padding: 1em; margin: 1em")
            plot(v-if="pdfs.length && decisionDefinition"
              v-model="pdfsSearchedUserAction"
              :pdfs="pdfs"
              :pdfsReference="pdfsReference"
              :decisionDefinition="decisionDefinition"
              :decisionAspectSelected="decisionAspectSelected")
            ConfusionMatrix(v-if="pdfs.length"
              @view="showFile($event)"
              :pdfs="pdfs"
              :pdfsReference="pdfsReference"
              :decisionAspectSelected="decisionAspectSelected")


      //- Global listing of reasons files failed
      v-expansion-panel(:key="2")
        v-expansion-panel-header All reasons files were rejected
        v-expansion-panel-content
          .decision-reasons Error message: number of files rejected / uniquely rejected (click rejected to accept)
            .decision-reason(v-for="f of failReasons" :key="f[0]"
                @click="filterToggle(f[0], true)"
                )
              checkmark(status="rejected")
              span {{f[0]}}: {{f[1][0].size}} / {{f[1][1].size}}

      //- File listing
      v-expansion-panel(:key="1")
        v-expansion-panel-header Files
        v-expansion-panel-content
          .file-lists
            v-list(dense)
              v-subheader 
                span(style="white-space: pre-wrap") Current decisions (top {{pdfsToShowMax}}) -!{' '}
                Stats(:pdfs="pdfs" :pdfsReference="pdfsReference" :decisionAspectSelected="decisionAspectSelected")
              v-list-item-group(mandatory)
                v-list-item(
                    v-for="p, ip of pdfsToShow"
                    :key="p.testfile"
                    @click="fileSelected = ip; scrollToFileDetails()"
                    :class="{changed: p.changed, \
                        'v-item--active': fileSelected === ip, \
                        'v-list-item--active': fileSelected === ip}"
                    )
                  v-list-item-content
                    v-list-item-title
                      //- checkmark(:status="p.status")
                      span {{p[decisionAspectSelected]}}&nbsp;
                      span {{p.testfile}}
                v-list-item(v-if="pdfs.length > pdfsToShowMax")
                  v-list-item-content
                    v-list-item-title
                      span ...{{pdfs.length - pdfsToShowMax}} other files processed
                      v-autocomplete(
                          v-model="pdfsSearchedUserAction"
                          :clearable="true"
                          :items="pdfs"
                          item-text="testfile"
                          return-object
                          prepend-icon="mdi-database-search"
                          placeholder="Show specific file...")
            div(style="display: inline-block")
              v-btn(title="By default, the 'Reference Decisions' are initial decisions from page load.  Click this to switch to showing prior decision, and again to use the current results as the baseline."
                  @click="makeBaseline"
                  :color="holdReferences ? 'primary' : ''") -&gt;
            v-list(dense)
              v-subheader 
                span(style="white-space: pre-wrap") Reference decisions -!{' '}
                Stats(:pdfs="pdfsReference" :pdfsReference="pdfsReference" :decisionAspectSelected="decisionAspectSelected")
              v-list-item-group()
                v-list-item(v-for="p of pdfsReference.slice(0, pdfsToShowMax)" :key="p.testfile")
                  v-list-item-content
                    v-list-item-title
                      //- checkmark(:status="p.status")
                      span {{p[decisionAspectSelected]}}&nbsp;
                      span {{p.testfile}}
            v-file-input(
              placeholder="Upload file input"
              hint="Upload saved decisions as reference"
              accept=".json"
              v-model="pdfsReferenceFile"
              :error-messages="pdfsReferenceFileError"
              @change="uploadPdfsReference"
              dense
              full-width
              persistent-hint
              show-size
              style="grid-area: 2/3"
              )
            v-btn(@click="dslReplaceForReferences" color="primary"
                style="grid-area: 3/3"
                title="Replace DSL with closest approximation of reference decisions based on available tools.  Note that order of outputs determines which of false positives or false negatives are minimized."
                ) Rationalizer (replaces DSL)

          //- Big margin-bottom to prevent scroll-back when changing file selection
          v-sheet(:elevation="3" style="margin-top: 1em; padding: 1em; margin-bottom: 100em")
            v-subheader Results for {{decisionSelected.testfile}}
            v-tabs(v-model="dbView" grow)
              v-tab(:key="DbView.Decision") Decision
              v-tab(:key="DbView.Tools") Output - Tools
              v-tab(:key="DbView.Parsers") Output - Parser
              v-tab(:key="DbView.Stats") Output - Stats
            v-tabs-items(v-model="dbView")
              v-tab-item(:key="DbView.Decision")
                v-expansion-panels(inset :style="{'margin-top': '1em'}")
                  v-expansion-panel
                    v-expansion-panel-header(:class="{'grey lighten-2': true}") File Detail Plugins
                    v-expansion-panel-content
                      v-btn(v-for="[pluginKey, plugin] of Object.entries(config && config.file_detail_views || {})" 
                          :key="pluginKey"
                          @click="pluginFileDetailView(pluginKey)") {{plugin.label}}
                      v-btn(v-show="pluginIframeSrc != null || pluginIframeLoading" @click="pluginIframeSrc = null; pluginIframeLoading = 0") (Close current plugin)
                      div(v-show="pluginIframeSrc != null || pluginIframeLoading" style="border: solid 1px #000; position: relative; height: 95vh")
                        v-progress-circular(v-show="pluginIframeLoading" :indeterminate="true")
                        iframe(v-show="pluginIframeSrc != null" style="width: 100%; height: 100%" ref="pluginIframe")
                FileFilterDetail(
                    ref="fileDetailView"
                    :decisionDefinition="decisionDefinition"
                    :decisionSelected="decisionSelected"
                    :decisionSelectedDsl="decisionSelectedDsl"
                    :decisionReference="decisionReference")
              v-tab-item(:key="DbView.Tools")
                DbView(:pdf="decisionSelected.testfile" collection="rawinvocations")
              v-tab-item(:key="DbView.Parsers")
                DbView(:pdf="decisionSelected.testfile" collection="invocationsparsed")
              v-tab-item(:key="DbView.Stats")
                DbView(:pdf="decisionSelected.testfile" collection="statsbyfile")

</template>

<style lang="scss">
  @import '~vuetify/src/styles/settings/_colors.scss';

  .loadingStatusDialog {
    position: fixed;
    top: 0;
    right: 0;
  }

  .home {
    width: 100%;
    display: flex;
    flex-direction: column;
    align-content: start;
    justify-content: center;

    > div {
      display: inline-block;
      margin-left: auto;
      margin-right: auto;
    }

    .reprocessdb {
    }

    .resetdb {
      color: #fff !important;
      background-color: map-get($red, 'base') !important;
    }

    .v-expansion-panels.colored > .v-expansion-panel > .v-expansion-panel-header {
      color: #fff;
      background-color: var(--v-primary-base);
    }

    .v-window {
      width: 100%;
      .v-window-item {
        transition: none !important;
      }
    }

    .filter-special-rows {
      display: flex;
      flex-direction: row;
      flex-wrap: wrap
    }

    .filter-special-row {
      display: inline-block;
      padding-right: 1em;
    }

    .file-lists {
      display: grid;
      grid-template: auto 5em / 1fr 5em 1fr;
      align-items: center;
      justify-items: stretch;
      .v-list {
        max-width: 100%;
        min-width: 80%;
      }
      .v-list-item-group {
        height: 20em;
        overflow-y: scroll;
        border-bottom: solid 1px #{var(--v-accent-lighten1)};
      }
      .v-list-item {
        &.changed {
          background-color: map-get($red, 'lighten-4');
        }
      }
    }

    .decision-info {
      white-space: pre-wrap;
    }

    .decision-reasons {
    }

    .decision-reason {
      cursor: pointer;
      white-space: pre; /* Note: It took 4 Galois engineers 30 min to figure
            out that two spaces were being collapsed to a single space, when
            copy and pasting error messages.  May this bug RIP 2020-03-11. */
      margin-left: 0.5em;
      &:hover {
        background-color: map-get($grey, lighten-2);
      }
    }
  }
</style>

<script lang="ts">
// @ is an alias to /src
//import HelloWorld from '@/components/HelloWorld.vue'

// Editor-related includes
import AceEditorComponent from 'vue2-ace-editor';
import 'brace/mode/yaml';
import 'brace/theme/chrome';
// Needed for ctrl+F
import 'brace/ext/searchbox';

import Vue from 'vue';

import {PdfDecision, sortByReject} from '@/components/common';
import {DslExpression, DslResult, dslParser, dslDefault} from '@/dsl';

import CheckmarkComponent from '@/components/Checkmark.vue';
import CirclePlotComponent from '@/components/circle-plot.vue';
import ConfusionMatrixComponent from '@/components/HomeConfusionMatrix.vue';
import StatsComponent from '@/components/HomeStats.vue';
import DbViewComponent from '@/components/DbView.vue';
import FileFilterDetailComponent from '@/components/FileFilterDetail.vue';

export enum DbView {
  Decision = 0,
  Tools = 1,
  Parsers = 2,
  Stats = 3,
}

export type PdfGroups = {[message: string]: Array<string>};

export class LoadingStatus {
  files_done: number = 0;
  files_max: number = 0;
  files_err: number = 0;
  message: string = '<Loading>';
}

export default Vue.extend({
  name: 'home',
  components: {
    AceEditor: AceEditorComponent,
    checkmark: CheckmarkComponent,
    ConfusionMatrix: ConfusionMatrixComponent,
    DbView: DbViewComponent,
    FileFilterDetail: FileFilterDetailComponent,
    plot: CirclePlotComponent,
    Stats: StatsComponent,
  },
  data() {
    return {
      beforeDestroyFns: [] as Array<{(): any}>,
      config: null as any,
      dbView: DbView.Decision,
      DbView: DbView,
      decisionAspectSelected: 'status' as string,
      decisionCode: dslDefault,
      decisionCodeTimeout: null as any,
      decisionCodeUpdated_handled: false as boolean,
      // Actual parsed output of dslParser.parse().
      decisionDefinition: null as DslResult | null,
      decisionReference: {} as PdfDecision,
      decisionSelectedDsl: {} as PdfDecision,
      error: false as any,
      expansionPanels: [0, 2],
      failReasons: [] as Array<[string, [Set<string>, Set<string>]]>,
      fileSelected: 0,
      holdReferences: true,
      initReferences: false,
      loadingStatus: new LoadingStatus(),
      pdfs: [] as PdfDecision[],
      pdfsDslLast: [] as PdfDecision[], // Specifically decisions calculated with DSL.
      pdfsReference: [] as PdfDecision[],
      pdfsReferenceFile: null as Blob | null,
      pdfsReferenceFileError: "",
      // Selected PDF or null
      pdfsSearched: null as PdfDecision | null | undefined,
      // PDFs to show in UI
      pdfsToShow: [] as PdfDecision[],
      // Number of PDFs to show in lists -- performance issue when too large.
      pdfsToShowMax: 20,
      pdfGroups: {} as PdfGroups,
      pdfGroupsDirty: false,
      pluginIframeLoading: 0,
      pluginIframeLoadingNext: 1,
      pluginIframeSrc: null as string|null,
      pluginIframeSrcMimeType: 'text/html',
      pluginDecIframeLast: '',
      pluginDecIframeLoading: 0,
      pluginDecIframeLoadingNext: 1,
      pluginDecIframeSrc: null as string|null,
      reprocessInnerInit: true,
      reprocessInnerPdfGroups: true,
      resetDbDialog: false,
      vuespaUrl: null as string|null,
    };
  },
  computed: {
    /** Returns [name, value] */
    decisionAspectAvailable(): Array<[string, string]> {
      const r = new Array<[string, string]>();
      const dd = this.decisionDefinition;
      if (dd === null) return r;

      for (const o of dd.outputs.keys()) {
        r.push([o, o]);
      }
      for (const f of dd.filters) {
        r.push([f.name, 'filter-' + f.name]);
      }
      return r;
    },
    /** Given a value for the given decision aspect, return a sort order.
      */
    decisionAspectSelectedOrdering(): {[value: string]: number} {
      const r: {[value: string]: number} = {};
      const dd = this.decisionDefinition!;
      const das = this.decisionAspectSelected;
      if (das.startsWith('filter-')) {
        r['true'] = 1;
        r['false'] = 2;
        r['unspecified'] = 3;
      }
      else {
        let i = 0;
        for (const v of dd.outputs.get(das)!) {
          r[v[0]] = i++;
        }
        r['unspecified'] = i++;
      }
      return r;
    },
    decisionSelected(): PdfDecision {
      if (this.pdfs === undefined) return {info: '', status: 'rejected',
          testfile: ''};
      if (this.pdfsSearched) return this.pdfsSearched;
      return this.pdfsToShow[this.fileSelected] || {};
    },
    pdfsSearchedUserAction: {
      get(): PdfDecision | null | undefined {
        return this.pdfsSearched;
      },
      set(v: PdfDecision | null | undefined): void {
        this.pdfsSearched = v;
        if (v) {
          this.scrollToFileDetails();
        }
      },
    },
  },
  watch: {
    decisionAspectSelected() {
      this.updatePdfsChanged();
    },
    decisionCode() {
      this.decisionCodeUpdated();
    },
    decisionSelected() {
      // Clear old display values
      this.pluginIframeLoading = 0;
      this.pluginIframeSrc = null;
      
      // Update related decisionReference!
      let decRef = null, dslRef = null;
      const testfile = this.decisionSelected.testfile;
      for (const p of this.pdfsReference) {
        if (p.testfile === testfile) {
          decRef = p;
          break;
        }
      }
      for (const p of this.pdfsDslLast) {
        if (p.testfile === testfile) {
          dslRef = p;
          break;
        }
      }
      
      if (decRef === null) {
        decRef = {info: '', status: 'not found', testfile: testfile};
      }
      if (dslRef === null) {
        dslRef = {info: '', status: 'not found', testfile: testfile};
      }
      this.decisionReference = decRef!;
      this.decisionSelectedDsl = dslRef!;
    },
    pdfGroups() {
      this.asyncTry(async () => {
        await this.reprocess();
      });
    },
    pdfs(newVal: any, oldVal: any) {
      // We only want to catch assignments, not re-sorts which happen as a
      // result of `updatePdfsChanged`.
      if (newVal === oldVal) return;
      this.updatePdfsChanged();
    },
    pdfsSearched() {
      this.updatePdfsToShow();
    },
    pdfsReference(newVal: any, oldVal: any) {
      // We only want to catch assignments, not re-sorts which happen as a
      // result of `updatePdfsChanged`.
      if (newVal === oldVal) return;
      this.updatePdfsChanged();
    },
    pluginDecIframeSrc(newVal: any) {
      if (newVal == null) {
        (this.$refs.pluginDecIframe as any).src = '';
        return;
      }

      const u8 = new Uint8Array(newVal.length);
      for (let i = 0, m = newVal.length; i < m; i++) {
        u8[i] = newVal.charCodeAt(i);
      }
      (this.$refs.pluginDecIframe as any).src = URL.createObjectURL(new Blob([u8], {type: 'text/html'}));
    },
    pluginIframeSrc(newVal: any) {
      if (newVal == null) {
        // Works, but HTML only, and doesn't work after `src` is set.
        //const doc = (this.$refs.pluginIframe as any).contentWindow.document;
        //doc.open();
        //doc.write('');
        //doc.close();
        (this.$refs.pluginIframe as any).src = '';

        return;
      }

      // CRUCIAL: blob does weird UTF-16 conversion.
      // See https://blog.logrocket.com/binary-data-in-the-browser-untangling-an-encoding-mess-with-javascript-typed-arrays-119673c0f1fe/
      // string must be in UTF-8
      const u8 = new Uint8Array(newVal.length);
      for (let i = 0, m = newVal.length; i < m; i++) {
        u8[i] = newVal.charCodeAt(i);
      }
      (this.$refs.pluginIframe as any).src = URL.createObjectURL(new Blob([u8], {type: this.pluginIframeSrcMimeType}));
    },
  },
  mounted() {
    this.beforeDestroyFns.push(this.$vuespa.httpHandler(
        (url: string) => {this.vuespaUrl = url; console.log(url);},
        {
          'redecide': (args: {[key: string]: any}) => {
            this.pluginDecisionView(this.pluginDecIframeLast, args);
          },
          'showFile': (args: {id: string}) => {
            this.showFile(args.id);
          },
        },
    ));

    const checkLoad = async () => {
      const oldConfig = this.loadingStatus.config_mtime;
      const oldDone = this.loadingStatus.files_done;
      await this.$vuespa.update('loadingStatus', 'loading_get');
      if (oldDone < this.loadingStatus.files_done) {
        this.pdfGroupsDirty = true;
      }
      if (oldConfig < this.loadingStatus.config_mtime) {
        await this.$vuespa.update('config', 'config_get');
      }
      setTimeout(() => {this.asyncTry(checkLoad)}, 1000);
    };
    this.asyncTry(checkLoad);
    this.asyncTry(async () => {
      try {
        await Promise.all([
            // `pdfGroups` loading triggers `reprocess`
            this.$vuespa.update('pdfGroups', 'decisions_get'),
            // Config provides information only for plugins.
            this.$vuespa.update('config', 'config_get'),
        ]);
        this.decisionCode = this.config.decision_default;
        this.decisionCodeUpdated_handled = true;
        this.decisionCodeUpdated_inner();
      }
      finally {
        // Don't reprocess right away, because we need to wait
        // for the `pdfGroups` watcher to be triggered. So, do
        // the reprocess after a small break.
        setTimeout(() => {
          this.asyncTry(async () => {
            this.reprocessInnerInit = false;
            await this.reprocess();
          })}, 16);
      }
    });

    // Required to correctly show ACE editor before async loading methods return
    this.decisionCodeUpdated_inner();
  },
  beforeDestroy() {
    for (const c of this.beforeDestroyFns) c();
  },
  methods: {
    async asyncTry(fn: {(): Promise<void>}) {
      try {
        await fn();
      }
      catch (e) {
        this.error = e;
        throw e;
      }
    },
    decisionCodeEditorInit(editor: any) {
      editor.setOption('tabSize', 2);
    },
    decisionCodeUpdated() {
      if (this.decisionCodeUpdated_handled) {
        this.decisionCodeUpdated_handled = false;
      }
      this.decisionCodeTimeout && clearTimeout(this.decisionCodeTimeout);
      this.decisionCodeTimeout = setTimeout(
          () => {this.decisionCodeUpdated_inner();},
          300,
      );
    },
    decisionCodeUpdated_inner() {
      // Step 1 -- resize container to appropriate height
      const editor = (this.$refs.decisionCodeEditor as any).editor;
      editor.container.style.height = `${editor.getSession().getScreenLength() * editor.renderer.lineHeight}px`;
      editor.resize();

      // Step 2 -- parse, finderrors, etc.
      this.decisionDefinition = null;
      editor.getSession().setAnnotations([]);
      try {
        this.decisionDefinition = dslParser.parse(this.decisionCode) as DslResult;
      }
      catch (e) {
        if (!e.location) throw e;
        let starter = this.decisionCode.slice(e.location.start.offset);
        starter = starter.split('\n', 1)[0];
        let ender = starter;
        starter = starter.slice(0, e.location.end.offset - e.location.start.offset);
        editor.getSession().setAnnotations([{
          row: e.location.start.line - 1,
          column: e.location.start.column,
          text: `${e.toString()}\nAt text: ${starter}\nFollowed by: ${ender}`,
          type: 'error',
        }]);
      }
    },
    download() {
      const json = JSON.stringify(this.pdfs);
      let blob = new Blob([json], { type: 'text/plain;charset=utf-8;' });
      let link = document.createElement('a');
      let url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', "decisions.json");
      link.style.display = 'none';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    },
    dslReplaceForReferences() {
      // Find set of all error messages which trigger a given status
      const dd = this.decisionDefinition;
      if (!dd) {
        return;
      }

      const das = this.decisionAspectSelected;
      if (das.startsWith('filter-')) {
        this.error = new Error('Cannot apply DSL to filter; must be output');
        return;
      }

      // Delete all existing "AutoFromRef" filters
      const filterPrefix = 'AutoFromRef';
      let nextGroup = 1;
      dd.filters = dd.filters.filter(x => !x.name.startsWith(filterPrefix));

      // Build file: message list
      const fileMessages = new Map<string, Set<string>>();
      for (const [k, files] of Object.entries(this.pdfGroups)) {
        for (const f of files) {
          let u = fileMessages.get(f);
          if (!u) {
            u = new Set<string>();
            fileMessages.set(f, u);
          }
          u!.add(k);
        }
      }

      for (const [k, v] of dd.outputs.entries()) {
        if (k !== das) {
          continue;
        }

        // Clear existing output
        for (const vv of v) {
          // Re-write vv[1] based on reference documents with this status
          const messages = new Set<string>();
          for (const pdfRef of this.pdfsReference) {
            if (pdfRef[das] === vv[0]) {
              const fm = fileMessages.get(pdfRef.testfile);
              if (!fm) continue;
              for (const m of fm) {
                let mAsRegex = m.replace(/[-/\\^$*+?.()|[\]{}]/g, '\\$&');
                if (mAsRegex.length > 128) {
                  // Javascript has a max regex length... Hm.
                  mAsRegex = mAsRegex.slice(0, 128) + '.*';
                }
                mAsRegex = '^' + mAsRegex + '$';
                messages.add(mAsRegex);
              }
            }
          }

          if (messages.size === 0) {
            vv[1] = null;
          }
          else {
            const filtName = filterPrefix + nextGroup++;
            dd.filters.push({
                name: filtName,
                all: true,
                patterns: Array.from(messages),
            });
            vv[1] = {type: 'id', id1: filtName};
          }
        }
      }

      // Finally, export the DSL to a text representation.
      const text: Array<string> = [];
      let indent = 0;
      const add = (t: string) => {
        for (let i = 0; i < indent; i++)
          text.push('  ');
        text.push(t);
        text.push('\n');
      };
      add('filters:');
      indent += 1;
      for (const k of dd.filters) {
        let header = k.name.slice();
        if (k.all)
          header += ' all';
        header += ':';
        add(header);
        indent += 1;
        for (const kk of k.patterns) {
          add(kk);
        }
        indent -= 1;
      }
      indent -= 1;
      add('outputs:');
      indent += 1;
      for (const [k, v] of dd.outputs) {
        add(k + ':');
        indent += 1;
        for (const vv of v) {
          let line = `"${vv[0]}"`;
          if (vv[1]) {
            if (vv[1].type === 'else') {
              line += ' else';
            }
            else {
              line += ' is ';
              const dumpExpr = (e: DslExpression): string => {
                if (e.type === 'or') {
                  return `(${dumpExpr(e.id1)} | ${dumpExpr(e.id2)})`;
                }
                else if (e.type === 'and') {
                  return `(${dumpExpr(e.id1)} & ${dumpExpr(e.id2)})`;
                }
                else if (e.type === 'id') {
                  return e.id1;
                }
                else if (e.type === 'not') {
                  return `!(${dumpExpr(e.id1)})`;
                }
                else {
                  this.error = new Error(`Not implemented: ${e.type}`);
                  throw this.error;
                }
              };
              line += dumpExpr(vv[1]);
            }
          }
          add(line);
        }
        indent -= 1;
      }
      indent -= 1;

      this.decisionCode = text.join('');
      this.decisionCodeUpdated_handled = true;
      this.decisionCodeUpdated_inner();
      this.asyncTry(async () => {this.reprocess()});
    },
    makeBaseline() {
      this.holdReferences = !this.holdReferences;
      if (this.holdReferences) {
        this.pdfsReference = this.pdfs.slice();
      }
    },
    pluginDecisionView(pluginKey: string, jsonArgs: {[key: string]: any}) {
      if (this.pluginDecIframeLoading) {
        return;
      }

      const loadKey = this.pluginDecIframeLoadingNext++;
      this.pluginDecIframeLast = pluginKey;
      this.pluginDecIframeLoading = loadKey;
      this.pluginDecIframeSrc = null;
      this.asyncTry(async () => {
        try {
          if (this.vuespaUrl === null) throw new Error('Websocket not connected?');

          // If not holding references, overwrite decisions BEFORE uploading them.
          // This behavior is easier to anticipate, basically.
          if (!this.holdReferences) {
            this.pdfsReference = this.pdfs;
          }

          // Reference decisions can be large, so only submit if needed
          let refDecs = null;
          if (this.config['decision_views'][pluginKey]['execStdin'].indexOf('<referenceDecisions>') !== -1) {
            refDecs = this.pdfsReference;
          }
          // Run plugin
          const r = await this.$vuespa.call('config_plugin_dec_run', pluginKey, 
              this.vuespaUrl, jsonArgs, refDecs);
          if (this.pluginDecIframeLoading !== loadKey) return;
          this.pluginDecIframeSrc = r.html;
          this.pdfs = r.decisions;
        }
        catch (e) {
          if (this.pluginDecIframeLoading !== loadKey) return;
          this.pluginDecIframeSrc = `<!DOCTYPE html><html><body style="white-space: pre-wrap">Error: ${e.replace(/&/g, '&amp;').replace(/</g, '&lt;')}</body></html>`;
        }
        finally {
          if (this.pluginDecIframeLoading === loadKey) {
            this.pluginDecIframeLoading = 0;
          }
        }
      });
    },
    pluginFileDetailView(pluginKey: string) {
      if (this.pluginIframeLoading) {
        return;
      }

      const loadKey = this.pluginIframeLoadingNext++;
      this.pluginIframeLoading = loadKey;
      this.pluginIframeSrc = null;
      this.asyncTry(async () => {
        try {
          const r = await this.$vuespa.call('config_plugin_run', pluginKey, this.decisionSelected.testfile);
          if (this.pluginIframeLoading !== loadKey) {
            // User aborted this load.
            return;
          }
          this.pluginIframeSrc = r.result;
          this.pluginIframeSrcMimeType = r.mimetype;
        }
        catch (e) {
          if (this.pluginIframeLoading !== loadKey) {
            // User aborted this load.
            return;
          }
          this.pluginIframeSrc = `<!DOCTYPE html><html><body style="white-space: pre-wrap">Error: ${e.replace(/&/g, '&amp;').replace(/</g, '&lt;')}</body></html>`;
          this.pluginIframeSrcMimeType = 'text/html';
        }
        finally {
          if (this.pluginIframeLoading === loadKey) {
            this.pluginIframeLoading = 0;
          }
        }
      });
    },
    resetDbErrors() {
      this.asyncTry(async () => {this.$vuespa.call('reset_db_errors');});
    },
    async reprocess() {
      /** Re-calculate decisions based on DSL */
      if (this.reprocessInnerInit) {
        // Loading... we'll be back here.
        return;
      }

      if (!this.reprocessInnerPdfGroups && this.pdfGroupsDirty) {
        this.pdfGroupsDirty = false;
        // Limit to one repeat of cleaning pdfGroups
        this.reprocessInnerPdfGroups = true;
        await this.$vuespa.update('pdfGroups', 'decisions_get');
        // watcher for pdfGroups will trigger reprocess.
        return;
      }
      const dd = this.decisionDefinition;
      if (dd === null) {
        throw new Error("reprocess() was triggered without valid decision spec?");
      }

      // OK, everything needed fetched, go ahead and run decisions.
      this.reprocessInnerPdfGroups = false;
      if (!this.holdReferences) {
        this.pdfsReference = this.pdfs;
      }

      // Build file list
      const newPdfs = [];
      const pdfMap = new Map<string, PdfDecision>();
      for (const [k, files] of Object.entries(this.pdfGroups)) {
        for (const f of files) {
          if (pdfMap.has(f)) continue;
          const dec = {
                testfile: f,
                info: '',
          };
          newPdfs.push(dec);
          pdfMap.set(f, dec);
        }
      }

      // Build sets of filter groups
      const pdfGroups = new Map<string, Set<string>>();
      const pdfGroupIsNegative = new Map<string, boolean>();
      for (const f of dd.filters) {
        const result = new Set<string>();
        pdfGroups.set(f.name, result);

        if (f.all) {
          pdfGroupIsNegative.set(f.name, true);
        }
        else {
          pdfGroupIsNegative.set(f.name, false);
        }

        const rs = f.patterns.map(p => new RegExp(p));
        for (const [k, files] of Object.entries(this.pdfGroups)) {
          let matched = false;
          for (const r of rs) {
            if (r.test(k)) {
              matched = true;
              break;
            }
          }

          if (f.all) {
            // If we're matching all, then we want to note the set of PDFs for
            // which any message did not match.
            if (!matched) {
              for (const file of files) {
                result.add(file);
                pdfMap.get(file)!.info += `'${f.name}' rejected '${k}'\n`;
              }
            }
          }
          else {
            // If we're matching any, then we're interested in PDFs where any
            // message did match.
            if (matched) {
              for (const file of files) {
                result.add(file);
                pdfMap.get(file)!.info += `'${f.name}' accepted '${k}'\n`;
              }
            }
          }
        }
      }

      // Apply DSL expressions to fill in status
      for (const [f, dec] of pdfMap.entries()) {
        const evalExpr = (node: DslExpression): boolean => {
          if (node.type === 'else') return true;
          if (node.type === 'not') return !evalExpr(node.id1);
          if (node.type === 'and') return evalExpr(node.id1) && evalExpr(node.id2);
          if (node.type === 'or') return evalExpr(node.id1) || evalExpr(node.id2);
          if (node.type === 'id') {
            let fileList = pdfGroups.get(node.id1);
            if (fileList === undefined) {
              throw new Error(`Undefined filter ${node.id1}`);
            }

            let r = fileList.has(f);
            let not = pdfGroupIsNegative.get(node.id1);
            if (not) r = !r;
            return r;
          }
          throw new Error(`Unknown node type ${(node as any).type}`);
        };

        for (const k of pdfGroups.keys()) {
          dec[`filter-${k}`] = evalExpr({type: 'id', id1: k});
        }

        for (const [oname, ocases] of dd.outputs.entries()) {
          dec[oname] = 'unspecified';
          for (const [value, expression] of ocases) {
            if (expression === null) continue;

            if (evalExpr(expression)) {
              dec[oname] = value;
              break;
            }
          }
        }
      }

      this.pdfs = newPdfs;
      this.pdfsDslLast = newPdfs;
      this.reprocessPost();
    },
    reprocessPost() {
      this.fileSelected = 0;
      this.updateFailReasons();

      // First time the page has been processed?
      if (!this.initReferences) {
        this.initReferences = true;
        this.pdfsReference = this.pdfs.slice();
      }
    },
    async reset() {
      /** Resets ALL processing. */
      this.pdfs = [];
      this.pdfsDslLast = [];
      this.pdfsReference = [];
      this.asyncTry(async () => {
        await this.$vuespa.call('clear_db');
      });
    },
    async resetParsers() {
      this.pdfs = [];
      this.pdfsDslLast = [];
      this.pdfsReference = [];
      this.asyncTry(async () => {
        await this.$vuespa.call('reparse_db');
      });
    },
    scrollToFileDetails() {
      // Show the user that a new file has been searched
      (this.$refs.fileDetailView as Vue).$el.scrollIntoView();
    },
    showFile(id: string) {
      this.pdfsSearchedUserAction = this.pdfs.filter(
        (p: PdfDecision) => p.testfile === id
      )[0];
    },
    /** For "All reasons files were rejected" section */
    updateFailReasons(){
      const failReasons = new Map<string, [Set<string>, Set<string>]>();
      for (const p of this.pdfs) {
        // Go through reasons failed, increment
        const re = /^rejected: (.*)$/gm;
        let r: RegExpExecArray | null;
        let only = true;
        let onlyKey: string | undefined = undefined;
        while ((r = re.exec(p.info)) !== null) {
          const k = r[1];
          if (onlyKey !== undefined && k !== onlyKey) {
            only = false;
          }

          let rr = failReasons.get(k);
          if (rr === undefined) {
            rr = [new Set(), new Set()];
            failReasons.set(k, rr);
          }
          onlyKey = k;
          rr[0].add(p.testfile);
        }

        if (only && onlyKey !== undefined) {
          failReasons.get(onlyKey)![1].add(p.testfile);
        }
      }
      this.failReasons = Array.from(failReasons.entries());
      this.failReasons.sort((a, b) => 1000000 * (b[1][1].size - a[1][1].size) + b[1][0].size - a[1][0].size);
 
    },
    updatePdfsChanged() {
      const refs = {} as any;
      const dca = this.decisionAspectSelected;
      for (const p of this.pdfsReference) {
        refs[p.testfile] = p[dca];
      }
      for (const p of this.pdfs) {
        p.changed = false;
        const d = refs[p.testfile];
        if (d) {
          p.changed = d !== p[dca];
        }
      }
      const dcaMap = this.decisionAspectSelectedOrdering;
      sortByReject(this.pdfs, this.pdfs.map(
          x => `${x.changed ? 'a' : 'b'},${dcaMap[x[dca]]},${x.testfile}`));

      const fileOrder = new Map<string, number>(this.pdfs.map((x, i) => [x.testfile, i]));
      const fileOrderGet = (x: string) => {
        const v = fileOrder.get(x);
        if (v === undefined) return 1e20;
        return v;
      };
      this.pdfsReference.sort((a, b) => fileOrderGet(a.testfile) - fileOrderGet(b.testfile));

      // Update display list
      this.updatePdfsToShow();
    },
    updatePdfsToShow() {
      if (!this.pdfsSearched) {
        this.pdfsToShow = this.pdfs.slice(0, this.pdfsToShowMax);
      } else {
        // Find matching testfile in most recent PDFs
        let p: null | PdfDecision = null;
        const testfile = this.pdfsSearched.testfile;
        for (const pp of this.pdfs) {
          if (pp.testfile === testfile) {
            p = pp;
            break;
          }
        }
        if (p === null) {
          this.error = "PDF selected not found?";
        }
        else {
          this.pdfsToShow = [p];
          this.pdfsSearched = p;
        }
      }
    },
    uploadPdfsReference() {
      const file = this.pdfsReferenceFile;
      if (!file) {
        if (!this.holdReferences) {
          this.pdfsReference = this.pdfs;
        }
      } else {
        var reader = new FileReader();
        reader.readAsText(file);
        reader.onload = () => {
          this.pdfsReferenceFileError = "";
          const contents = reader.result;
          if (typeof contents === "string") {
            const objs = JSON.parse(contents);
            if (objs) {
              // Add more checks here?
              // (testfile names match and objs is of type PdfReference[])
              this.pdfsReference = objs;
            } else this.pdfsReferenceFileError = "JSON does not match required format.";
          } else this.pdfsReferenceFileError = "Unable to read file contents.";
        };
      }
    },
  },
});
</script>
