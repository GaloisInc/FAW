<template lang="pug">
  .home
    div(
        v-if=" \
          loadingStatus.files_max === 0 \
          || loadingStatus.files_done !== loadingStatus.files_max \
          || loadingStatus.files_err"
        class="loadingStatusDialog"
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
        v-expansion-panel-header Overview
        v-expansion-panel-content
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
            v-dialog(v-model="resetDbDialog" persistent max-width="800")
              template(v-slot:activator="{on}")
                v-btn.resetdb(v-on="on") Reset Entire DB (may take awhile)
              v-card
                v-card-title Reset entire DB, re-running all tools and parsers?
                v-card-actions(:style={'flex-wrap': 'wrap'})
                  v-btn(@click="resetDbDialog=false") Cancel
                  v-btn(@click="resetDbErrors(); resetDbDialog=false") Reprocess DB errors
                  v-btn(@click="resetParsers(); resetDbDialog=false") Reset Most of DB, but not same-version tool invocations
                  v-btn(@click="reset(); resetDbDialog=false") Reset Entire DB

          AnalysisSetConfig(:currentId.sync="analysisSetId"
              :pipeCfg="config && config.pipelines"
              @update="pdfGroupsDirty = true")

          v-expansion-panels(inset :style="{'margin-top': '1em'}")
            v-expansion-panel
              v-expansion-panel-header(:class="{'grey lighten-2': true}")
                span
                  span Decision Plugins
                  span(v-if="fileFilters.length") {{' '}}(filtered)
              v-expansion-panel-content
                v-btn(v-for="[pluginKey, plugin] of Object.entries(uiPluginsDecision)"
                    :key="pluginKey"
                    @click="pluginDecisionView(pluginKey, {})") {{plugin.label}}
                v-btn(v-show="pluginDecIframeSrc != null || pluginDecIframeLoading" @click="pluginDecIframeSrc = null; pluginDecIframeLoading = 0") (Close current plugin)
                div(v-show="pluginDecIframeSrc != null || pluginDecIframeLoading" style="border: solid 1px #000; position: relative; height: 95vh")
                  v-progress-circular(v-show="pluginDecIframeLoading" :indeterminate="true")
                  iframe(v-show="pluginDecIframeSrc != null" style="width: 100%; height: 100%" ref="pluginDecIframe")

          v-sheet(:elevation="3" style="padding: 1em; margin: 1em")
            div(v-if="fileFilters.length")
              v-btn(v-for="f, fidx of fileFilters" :key="fidx" @click="fileFilterPopTo(fidx)") {{f[0]}}
              v-btn(@click="fileFilterInvert()") (Invert last)
            //- Allow selection of different things.
            div(v-if="decisionDefinition")
              v-radio-group(row v-model="decisionAspectSelected")
                v-radio(v-for="o of decisionAspectAvailable.filter(x => x[1] !== 'filter-faw-custom' && x[1] !== 'filter-faw-errors')" :key="o[1]"
                    :label="o[0]" :value="o[1]")
                v-radio(label="(Custom Search)" :value="'filter-faw-custom'")
                v-radio(label="(Workbench Errors)" :value="'filter-faw-errors'")
            div(v-if="decisionAspectSelected === 'filter-faw-custom'"
                style="display: flex; flex-direction: row; align-items: center")
              v-text-field(label="Search Regex (press enter to reprocess)" v-on:keyup.enter="reprocess"
                  v-model="decisionSearchCustom")
              v-checkbox(v-model="decisionSearchInsensitive" label="Case-insensitive" style="margin-left: 0.2em")
              v-btn(tile @click="reprocess" :disabled="!decisionDefinition" style="margin-left: 0.2em") Reprocess + Search

          //- Plot of file statuses
          v-sheet(:elevation="3" style="padding: 1em; margin: 1em")
            div
              v-checkbox(v-model="plotShow" label="Show plot?")
            .plot-div(v-if="plotShow")
              plot(v-if="pdfs.length && decisionDefinition"
                v-model="pdfsSearchedUserAction"
                :pdfs="pdfs"
                :pdfsReference="pdfsReference"
                :decisionDefinition="decisionDefinition"
                :decisionAspectSelected="decisionAspectSelected")
            ConfusionMatrix(v-if="pdfs.length"
              @view="showFile($event)"
              @filter-file-list="fileFilterAdd($event.name, new Set($event.files))"
              :pdfs="pdfs"
              :pdfsReference="pdfsReference"
              :decisionAspectSelected="decisionAspectSelected")

            //- Global listing of reasons files failed
            v-expansion-panels(:value="0" :popout="true" v-if="decisionAspectSelected.startsWith('filter-')")
              v-expansion-panel(:key="0")
                v-expansion-panel-header All reasons files affected filter: {{decisionAspectSelected.substring(7)}}
                v-expansion-panel-content
                  .decision-reasons(style="padding-bottom: 1em;") {{(failReasons.get(decisionAspectSelected) || []).length}} error messages: number of files rejected / uniquely rejected
                    v-virtual-scroll(
                        :bench="10"
                        :items="failReasons.get(decisionAspectSelected) || []"
                        height="200"
                        item-height="25"
                        )
                      template(v-slot="{item}")
                        v-menu(offset-y max-width="700" :key="item[0] + decisionAspectSelected + decisionSearchCustom")
                          template(v-slot:activator="{on}")
                            .decision-reason(v-on="on")
                              checkmark(status="rejected")
                              span {{item[0]}}: {{item[1][0].size + item[1][1].size}} / {{item[1][1].size}}
                          v-list
                            v-list-item(style="flex-wrap: wrap")
                              v-btn(@click="fileFilterAdd(item[0], new Set([...Array.from(item[1][0]), ...Array.from(item[1][1])]))") Filter in FAW
                              v-btn(v-clipboard="() => regexEscape(item[0])") (Copy regex to clipboard)
                              v-btn(v-clipboard="() => '^' + regexEscape(item[0]) + '$'") (with ^$)
                              v-btn(v-clipboard="() => JSON.stringify([...Array.from(item[1][1]), ...Array.from(item[1][0])])") (Copy file list as JSON)
                            v-list-item(v-for="ex of [...sliceIterable(item[1][1], 0, 10), ...sliceIterable(item[1][0], 0, 10)].slice(0, 10)" :key="ex" @click="showFile(ex)") {{ex}}

      v-expansion-panel(:key="1")
        //-
          NOTE: MUST BE VISIBLE on page load. Otherwise decisionCodeEditor has issues.
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

      //- File listing
      v-expansion-panel(:key="3")
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
                v-list-item
                  v-list-item-content
                    v-list-item-title
                      span(v-if="pdfs.length > pdfsToShowMax") ...{{pdfs.length - pdfsToShowMax}} other files processed
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
                v-list-item(v-for="p of pdfsToShowReference" :key="p.testfile")
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
          v-sheet(:elevation="3" style="margin-top: 1em; padding: 1em; margin-bottom: 50vh")
            v-subheader Results for {{decisionSelected.testfile}}
            v-tabs(v-model="dbView" grow)
              v-tab(:key="DbView.Decision") Decision
              v-tab(:key="DbView.Tools") Output - Tools
              v-tab(:key="DbView.Parsers") Output - Parser
            v-tabs-items(v-model="dbView" ref="detailView")
              v-tab-item(:key="DbView.Decision")
                v-expansion-panels(inset :style="{'margin-top': '1em'}")
                  v-expansion-panel
                    v-expansion-panel-header(:class="{'grey lighten-2': true}") File Detail Plugins
                    v-expansion-panel-content
                      v-btn(v-for="[pluginKey, plugin] of Object.entries(uiPluginsFileDetail)"
                          :key="pluginKey"
                          @click="pluginFileDetailView(pluginKey, {})") {{plugin.label}}
                      v-btn(v-show="pluginIframeSrc != null || pluginIframeLoading" @click="pluginIframeSrc = null; pluginIframeLoading = 0") (Close current plugin)
                      div(v-show="pluginIframeSrc != null || pluginIframeLoading" style="border: solid 1px #000; position: relative; height: 95vh")
                        v-progress-circular(v-show="pluginIframeLoading" :indeterminate="true")
                        iframe(v-show="pluginIframeSrc != null" style="width: 100%; height: 100%" ref="pluginIframe")
                FileFilterDetail(
                    :decisionDefinition="decisionDefinition"
                    :decisionSelected="decisionSelected"
                    :decisionSelectedDsl="decisionSelectedDsl"
                    :decisionReference="decisionReference"
                    :asOptions="_pdfGroupsSubsetOptions()"
                    )
              v-tab-item(:key="DbView.Tools")
                DbView(:pdf="decisionSelected.testfile" collection="rawinvocations")
              v-tab-item(:key="DbView.Parsers")
                DbView(:pdf="decisionSelected.testfile" collection="invocationsparsed")

</template>

<style lang="scss">
  @import '~vuetify/src/styles/settings/_colors.scss';

  .loadingStatusDialog {
    position: fixed;
    top: 6px;
    right: 6px;
    width: 300px;
    z-index: 999;
  }

  .v-card__actions {
    > .v-btn {
      margin: 0.1rem 1rem;
    }
  }

  .home {
    width: 100%;
    display: flex;
    flex-direction: column;
    align-content: flex-start;
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

import bus from '@/bus';

// Editor-related includes
import AceEditorComponent from 'vue2-ace-editor';
import 'brace/mode/yaml';
import 'brace/theme/chrome';
// Needed for ctrl+F
import 'brace/ext/searchbox';

import Vue from 'vue';

import {PdfDecision, sortByReject} from '@/components/common';
import {DslExpression, DslResult, dslParser, dslDefault} from '@/dsl';
import {AsData} from '@/interface/aset';

import AnalysisSetConfigComponent from '@/components/AnalysisSetConfig.vue';
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

export type FileFilterData = [string, Set<string>];
export type PdfGroups = {groups: {[message: string]: Array<[number, number]>},
    files: Array<string>};

export class LoadingStatus {
  config_mtime: number = 0;
  files_done: number = 0;
  files_max: number = 0;
  files_err: number = 0;
  message: string = '<Loading>';
}

export default Vue.extend({
  name: 'home',
  components: {
    AceEditor: AceEditorComponent,
    AnalysisSetConfig: AnalysisSetConfigComponent,
    checkmark: CheckmarkComponent,
    ConfusionMatrix: ConfusionMatrixComponent,
    DbView: DbViewComponent,
    FileFilterDetail: FileFilterDetailComponent,
    plot: CirclePlotComponent,
    Stats: StatsComponent,
  },
  data() {
    return {
      analysisSetId: null as null|string,
      asData: {asets: [], parsers: []} as AsData,
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
      decisionSearchCustom: '',
      decisionSearchInsensitive: true,
      decisionSelectedDsl: {} as PdfDecision,
      error: false as any,
      expansionPanels: [0, 1, 2],
      // failReasons points from a filter name to an Array of [relevant message, [files failing non-uniquely, files uniquely failing]
      failReasons: Object.freeze(new Map<string, Array<[string, [Set<string>, Set<string>]]>>()),
      fileFilters: new Array<FileFilterData>(),
      fileSelected: 0,
      holdReferences: true,
      initReferences: false,
      loadingStatus: new LoadingStatus(),
      pdfs: [] as readonly PdfDecision[],
      pdfsDslLast: [] as readonly PdfDecision[], // Specifically decisions calculated with DSL.
      pdfsReference: [] as readonly PdfDecision[],
      pdfsReferenceFile: null as Blob | null,
      pdfsReferenceFileError: "",
      // Selected PDF or null
      pdfsSearched: null as PdfDecision | null | undefined,
      // PDFs to show in UI
      pdfsToShow: [] as readonly PdfDecision[],
      pdfsToShowReference: [] as readonly PdfDecision[],
      // Number of PDFs to show in lists -- performance issue when too large.
      pdfsToShowMax: 20,
      // Defines files available to the FAW; constricted by fileFilters' last
      // entry.
      pdfGroups: {groups: {}, files: []} as PdfGroups,
      pdfGroupsDirty: false,
      plotShow: true,
      pluginIframeLast: '',
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
      if (this.pdfs === undefined) return {info: [], status: 'rejected',
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
    uiPluginsDecision(): {[key: string]: any} {
      return this.pluginsWithPipelines('decision_views');
    },
    uiPluginsFileDetail(): {[key: string]: any} {
      return this.pluginsWithPipelines('file_detail_views');
    },
  },
  watch: {
    analysisSetId() {
      this.pdfGroupsDirty = true;
    },
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
        decRef = {info: [], status: 'not found', testfile: testfile};
      }
      if (dslRef === null) {
        dslRef = {info: [], status: 'not found', testfile: testfile};
      }
      this.decisionReference = decRef!;
      this.decisionSelectedDsl = dslRef!;
    },
    fileFilters() {
      this.asyncTry(async () => {
        await this.reprocess();
      });
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
    const busOn = (event: string, callback: (event: any) => void) => {
      bus.$on(event, callback);
      return () => {
        bus.$off(event, callback);
      };
    };
    this.beforeDestroyFns.push(busOn('error', (error: any) => {
      this.error = error;
      console.log(error);
    }));

    this.beforeDestroyFns.push(busOn('analysisSetData', (data: AsData) => {
      this.asData = data;
    }));

    this.beforeDestroyFns.push(this.$vuespa.httpHandler(
        (url: string) => {this.vuespaUrl = url; console.log(url);},
        {
          'redecide': (args: {[key: string]: any}) => {
            this.pluginDecisionView(this.pluginDecIframeLast, args);
          },
          'redetail': (args: {[key: string]: any}) => {
            this.pluginFileDetailView(this.pluginIframeLast, args);
          },
          'showFile': (args: {id: string}) => {
            this.showFile(args.id);
          },
        },
    ));

    const checkLoad = async () => {
      const oldConfig = this.loadingStatus.config_mtime;
      const oldDone = this.loadingStatus.files_done;
      await this.$vuespa.update('loadingStatus', 'loading_get', {});
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
            this._pdfGroupsUpdate(),
            // Config provides information only for plugins.
            this.$vuespa.update('config', 'config_get'),
        ]);
        this.decisionCode = this.config.decision_default;
        this.decisionCodeUpdated_handled = true;
        await new Promise((resolve, reject) => {
          setTimeout(resolve, 1);
        });
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
        bus.error(e);
        throw e;
      }
    },
    decisionCodeEditorInit(editor: any) {
      editor.setOption('tabSize', 2);
    },
    decisionCodeUpdated() {
      if (this.decisionCodeUpdated_handled) {
        // This change to `decisionCode` was already handled.
        this.decisionCodeUpdated_handled = false;
        this.decisionCodeTimeout && clearTimeout(this.decisionCodeTimeout);
        return;
      }
      this.decisionCodeTimeout && clearTimeout(this.decisionCodeTimeout);
      this.decisionCodeTimeout = setTimeout(
          () => {this.decisionCodeUpdated_inner();},
          300,
      );
    },
    decisionCodeUpdated_inner() {
      // Step 1 -- resize container to appropriate height
      const editorContainer = this.$refs.decisionCodeEditor as any;
      if (!editorContainer) return;
      const editor = editorContainer.editor;
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

      // Filter out special FAW filters
      dd.filters = dd.filters.filter(x => !x.name.startsWith('faw-'));

      // Build file: message list
      const fileMessages = new Map<string, Set<string>>();
      for (const [k, files] of Object.entries(this.pdfGroups.groups)) {
        for (const f of files) {
          const ff = this.pdfGroups.files[f[0]];
          let u = fileMessages.get(ff);
          if (!u) {
            u = new Set<string>();
            fileMessages.set(ff, u);
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
                patterns: Array.from(messages).map(x => ({pat: x, check: null})),
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
          add(kk.pat);
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
    fileFilterAdd(name: string, files: Set<string>) {
      this.fileFilters.push([name, files]);
    },
    fileFilterInvert() {
      if (this.fileFilters.length === 0) {
        return;
      }

      const last = this.fileFilterLatest()!;
      let prev: FileFilterData;
      if (this.fileFilters.length > 1) {
        prev = this.fileFilters[this.fileFilters.length - 2];
      }
      else {
        prev = ['(all)', new Set(this.pdfGroups.files)];
      }

      this.fileFilters.push([last[0] + ' (inverted)',
          new Set([...prev[1]].filter(x => !last[1].has(x)))]);
    },
    fileFilterLatest(): undefined|FileFilterData {
      if (this.fileFilters.length === 0) return;
      return this.fileFilters[this.fileFilters.length - 1];
    },
    fileFilterPopTo(idx: number) {
      this.fileFilters.splice(idx, this.fileFilters.length);
    },
    makeBaseline() {
      this.holdReferences = !this.holdReferences;
      if (this.holdReferences) {
        this.pdfsReference = this.pdfs;
      }
    },
    pluginDecisionView(pluginKey: string, jsonArgs: {[key: string]: any}) {
      if (this.pluginDecIframeLoading && this.pluginDecIframeLast === pluginKey) {
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
          let pluginDef: any = null;
          if (pluginKey.indexOf('!') !== -1) {
            const [aset, pipeline, plugin] = pluginKey.split('!');
            pluginDef = this.config['pipelines'][pipeline]['decision_views'][plugin];
          }
          else {
            pluginDef = this.config['decision_views'][pluginKey];
          }
          if (pluginDef['execStdin'].indexOf('<referenceDecisions>') !== -1) {
            refDecs = this.pdfsReference;
          }
          // Run plugin
          const r = await this.$vuespa.call('config_plugin_dec_run', pluginKey,
              this.vuespaUrl, jsonArgs, refDecs, this._pdfGroupsSubsetOptions(true));
          if (this.pluginDecIframeLoading !== loadKey) return;
          this.pluginDecIframeSrc = r.html;

          // Build a lookup table to overwrite our decision data based on what
          // the plugin returned.
          const allTestFiles = new Set(this.pdfs.map(x => x.testfile));
          const decisions = [];
          for (const p of r.decisions) {
            allTestFiles.delete(p.testfile);
            decisions.push(p);
          }
          for (const a of allTestFiles) {
            decisions.push({info: [], testfile: a});
          }
          this.pdfs = Object.freeze(decisions);
        }
        catch (e) {
          if (this.pluginDecIframeLoading !== loadKey) return;
          const retryArgs = JSON.stringify(jsonArgs);
          this.pluginDecIframeSrc = `<!DOCTYPE html><html>
            <head>
            <script>
              function retry() {
                let args = '${retryArgs}';
                let req = new XMLHttpRequest();
                let url = '${this.vuespaUrl}/redecide';
                req.open('post', url, true);
                req.setRequestHeader('Content-Type', 'application/json');
                req.send(args);
              }
            ` + '<' + `/script>
            </head>
            <body style="white-space: pre-wrap">Error: ${e.toString().replace(/&/g, '&amp;').replace(/</g, '&lt;')}
            <br/><input type="button" value="Retry" onclick="retry()" />
            </body></html>`;
        }
        finally {
          if (this.pluginDecIframeLoading === loadKey) {
            this.pluginDecIframeLoading = 0;
          }
        }
      });
    },
    pluginFileDetailView(pluginKey: string, jsonArgs: {[key: string]: any}) {
      if (this.pluginIframeLoading && pluginKey === this.pluginIframeLast) {
        return;
      }

      const loadKey = this.pluginIframeLoadingNext++;
      this.pluginIframeLast = pluginKey;
      this.pluginIframeLoading = loadKey;
      this.pluginIframeSrc = null;
      this.asyncTry(async () => {
        try {
          const r = await this.$vuespa.call('config_plugin_run', pluginKey,
              this.vuespaUrl, jsonArgs, this.decisionSelected.testfile);
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
          this.pluginIframeSrc = `<!DOCTYPE html><html><body style="white-space: pre-wrap">Error: ${e.toString().replace(/&/g, '&amp;').replace(/</g, '&lt;')}</body></html>`;
          this.pluginIframeSrcMimeType = 'text/html';
        }
        finally {
          if (this.pluginIframeLoading === loadKey) {
            this.pluginIframeLoading = 0;
          }
        }
      });
    },
    /** Find a plugin category and return a flat list of plugins, including any
      analysis set + pipeline combinations.
      */
    pluginsWithPipelines(key: string): {[key: string]: any} {
      if (!this.config) return {};
      const o = Object.assign({}, this.config[key]);
      for (const aset of this.asData.asets) {
        for (const [pk, pv] of Object.entries(aset.pipelines)) {
          for (const [ppk, ppv] of Object.entries(this.config.pipelines[pk][key])) {
            const pluginKey = `${aset.id}!${pk}!${ppk}`;
            const ok: any = o[pluginKey] = Object.assign({}, ppv);
            ok.label = `${pk} -- ${ok.label} [${aset.id}]`;
          }
        }
      }
      return o;
    },
    regexEscape(v: string): string {
      return v.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, '\\$&');
    },
    resetDbErrors() {
      this.asyncTry(async () => {
        await this.$vuespa.call('reset_db_errors');
        this.pdfGroupsDirty = true;
      });
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
        await this._pdfGroupsUpdate();
        // watcher for pdfGroups will trigger reprocess.
        return;
      }
      const dd = this.decisionDefinition;
      if (dd === null) {
        throw new Error("reprocess() was triggered without valid decision spec?");
      }

      if (this.decisionAspectSelected === 'filter-faw-custom') {
        dd.filters = dd.filters.filter(x => x.name !== 'faw-custom');
        dd.filters.push({
          name: 'faw-custom',
          all: false,
          caseInsensitive: this.decisionSearchInsensitive,
          patterns: [{pat: this.decisionSearchCustom, check: null}],
        });
      }

      // Push workbench errors regardless
      dd.filters = dd.filters.filter(x => x.name !== 'faw-errors');
      dd.filters.push({
        name: 'faw-errors',
        all: false,
        caseInsensitive: true,
        patterns: [
          {pat: '_<<workbench: Exit code missing', check: null},
          {pat: '_<<workbench: Exit status: RuntimeError', check: null},
          {pat: '_<<workbench: unhandled', check: null},
        ],
      });

      // OK, everything needed fetched, go ahead and run decisions.
      this.reprocessInnerPdfGroups = false;
      if (!this.holdReferences) {
        this.pdfsReference = this.pdfs;
      }

      // Narrow down to only groups pertaining to selected files
      let groups: {[message: string]: Array<[number, number]>} = this.pdfGroups.groups;
      if (this.fileFilters.length > 0) {
        const fset = this.fileFilterLatest()![1];
        let okSet = new Set();
        for (const [fi, f] of this.pdfGroups.files.entries()) {
          if (fset.has(f)) okSet.add(fi);
        }

        groups = {};
        for (const [k, files] of Object.entries(this.pdfGroups.groups)) {
          let nfiles = files.filter(x => okSet.has(x[0]));
          if (nfiles.length === 0) continue;
          groups[k] = nfiles;
        }
      }

      // Build file list
      const newPdfs = [];
      const pdfMap = new Map<number, PdfDecision>();
      for (const [k, files] of Object.entries(groups)) {
        for (const f of files) {
          if (pdfMap.has(f[0])) continue;
          const dec = {
                testfile: this.pdfGroups.files[f[0]],
                info: [],
          };
          newPdfs.push(dec);
          pdfMap.set(f[0], dec);
        }
      }

      // Build sets of filter groups
      const pdfGroups = new Map<string, Set<number>>();
      const pdfGroupIsNegative = new Map<string, boolean>();
      for (const f of dd.filters) {
        const result = new Set<number>();
        pdfGroups.set(f.name, result);

        if (f.all) {
          pdfGroupIsNegative.set(f.name, true);
        }
        else {
          pdfGroupIsNegative.set(f.name, false);
        }

        const rs = f.patterns.map(p => ({
            pat: new RegExp(p.pat, f.caseInsensitive ? 'i' : undefined),
            check: p.check,
        }));
        for (const [k, files] of Object.entries(groups)) {
          let matched = false;
          // Do any of our filter's patterns match this message?
          let filesSubset = files;

          const evalCheck = (k: string, check: any): Set<number> => {
            const parts = new Map<string, Array<number>>();
            for (const [id, suffix] of [['sum', '_sum'], ['nan', '_nan'],
                ['count', '']]) {
              let filesWithMsg = groups[k + suffix];
              if (filesWithMsg === undefined) {
                if (['sum', 'nan'].indexOf(k.split('_').pop()!) !== -1) {
                  // If we can't find this, this is NOT a number, but likely a
                  // subfield of a number (e.g., we're looking at _nan_sum)
                  // So, act like no match.
                  return new Set();
                }
                throw new Error(`Could not find ${k + suffix}?`);
              }

              const p = new Array<number>();
              parts.set(id, p);
              for (const file of filesWithMsg) {
                p[file[0]] = file[1];
              }
            }

            const evalInner = (check: any): Array<number> => {
              if (check.type === '<') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l < right[i] ? 1 : 0);
              }
              else if (check.type === '>') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l > right[i] ? 1 : 0);
              }
              else if (check.type === '<=') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l <= right[i] ? 1 : 0);
              }
              else if (check.type === '>=') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l >= right[i] ? 1 : 0);
              }
              else if (check.type === 'and') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l && right[i] ? 1 : 0);
              }
              else if (check.type === 'or') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l || right[i] ? 1 : 0);
              }
              else if (check.type === 'id') {
                const r = parts.get(check.id1);
                if (r === undefined) {
                  throw new Error(`No such numeric quantity? ${check.id1}`);
                }
                return r;
              }
              else if (check.type === 'number') {
                // Inefficient, but that's OK.
                return parts.get('count')!.map(x => check.id1);
              }
              else if (check.type === 'neg') {
                return evalInner(check.id1).map(x => -x);
              }
              else if (check.type === '+') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l + right[i]);
              }
              else if (check.type === '-') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l - right[i]);
              }
              else if (check.type === '*') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l * right[i]);
              }
              else if (check.type === '/') {
                const left = evalInner(check.id1);
                const right = evalInner(check.id2);
                return left.map((l, i) => l / right[i]);
              }

              console.log(check);
              throw new Error(`Check type ${check.type}`);
            };

            const r = evalInner(check);
            return new Set(r.map((x, i) => [x, i]).filter(x => !!x[0]).map(x => x[1]));
          };

          for (const r of rs) {
            if (r.pat.test(k)) {
              // Do we need to constrain the matching set based on an auxiliary
              // check?
              if (r.check !== null) {
                const group = evalCheck(k, r.check);
                if (group.size > 0) filesSubset = filesSubset.filter(x => group.has(x[0]));
                else continue;
              }
              matched = true;
              break;
            }
          }

          if (f.all) {
            // If we're matching all, then we want to note the set of PDFs for
            // which any message did not match.
            if (!matched) {
              for (const file of filesSubset) {
                result.add(file[0]);
                pdfMap.get(file[0])!.info.push(`'${f.name}' rejected '${k}'`);
              }
            }
          }
          else {
            // If we're matching any, then we're interested in PDFs where any
            // message did match.
            if (matched) {
              for (const file of filesSubset) {
                result.add(file[0]);
                pdfMap.get(file[0])!.info.push(`'${f.name}' accepted '${k}'`);
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

      // Object.freeze() important for efficiency with large datasets
      this.pdfs = Object.freeze(newPdfs);
      this.pdfsDslLast = Object.freeze(newPdfs);
      this.reprocessPost();
    },
    reprocessPost() {
      this.fileSelected = 0;
      this.updateFailReasons();

      // First time the page has been processed?
      if (!this.initReferences) {
        this.initReferences = true;
        this.pdfsReference = this.pdfs;
      }
    },
    async reset() {
      /** Resets ALL processing. */
      this.pdfs = [];
      this.pdfsDslLast = [];
      this.pdfsReference = [];
      this.asyncTry(async () => {
        await this.$vuespa.call('clear_db');
        this.pdfGroupsDirty = true;
      });
    },
    async resetParsers() {
      this.pdfs = [];
      this.pdfsDslLast = [];
      this.pdfsReference = [];
      this.asyncTry(async () => {
        await this.$vuespa.call('reparse_db');
        this.pdfGroupsDirty = true;
      });
    },
    scrollToFileDetails() {
      // Show the user that a new file has been searched
      (this.$refs.detailView as Vue).$el.scrollIntoView();
    },
    showFile(id: string) {
      this.pdfsSearchedUserAction = this.pdfs.filter(
        (p: PdfDecision) => p.testfile === id
      )[0];
    },
    /** Unfortunately, iterators in javascript don't have a slice() method.
        But, we have very large sets comprising thousands of files. For UI
        updates, it's silly to cast that whole set to an array to select the
        first, arbitrary elements. Instead, use this function.
        */
    sliceIterable<T>(set: Set<T>, start: number, end: number) {
      let i = -1;
      const r = new Array<T>();
      if (start >= end) return r;

      const m = end - 1;
      for (const el of set) {
        i++;
        if (i < start) continue;

        r.push(el);
        if (i === m) break;
      }
      return r;
    },
    /** For "All reasons files were rejected" section */
    updateFailReasons(){
      const failReasons = new Map<string, Map<string, [Set<string>, Set<string>]>>();
      for (const p of this.pdfs) {
        // Go through reasons failed, increment
        const re = /^'(.*)' (accepted|rejected) '(.*)'$/gm;
        let r: RegExpExecArray | null;
        let only = false;
        let onlyKey: string | undefined = undefined;
        let onlyFilter: string | undefined = undefined;
        const logOnly = () => {
          if (!only || !onlyFilter || !onlyKey) return;
          // Reaching here means this file had only one message causing it to
          // pass a filter.
          failReasons.get(onlyFilter)!.get(onlyKey)![1].add(p.testfile);
          failReasons.get(onlyFilter)!.get(onlyKey)![0].delete(p.testfile);
        };
        for (const line of p.info) {
          while ((r = re.exec(line)) !== null) {
            const filt = r[1];
            const msg = r[3];
            if (filt !== onlyFilter) {
              logOnly();
              only = true;
              onlyKey = msg;
              onlyFilter = filt;
            }

            if (msg !== onlyKey) {
              only = false;
            }

            let filtMap = failReasons.get(filt);
            if (filtMap === undefined) {
              filtMap = new Map<string, [Set<string>, Set<string>]>();
              failReasons.set(filt, filtMap);
            }

            let rr = filtMap.get(msg);
            if (rr === undefined) {
              rr = [new Set(), new Set()];
              filtMap.set(msg, rr);
            }
            rr[0].add(p.testfile);
          }
        }
        logOnly();
      }
      const frMap = new Map<string, Array<[string, [Set<string>, Set<string>]]>>();
      for (const [filt, data] of failReasons.entries()) {
        const filtData = Array.from(data.entries());
        filtData.sort((a, b) => 1000000 * (b[1][1].size - a[1][1].size) + b[1][0].size - a[1][0].size);
        frMap.set(`filter-${filt}`, filtData);
      }
      this.failReasons = Object.freeze(frMap);
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

      // Update display list
      this.updatePdfsToShow();
    },
    updatePdfsToShow() {
      if (!this.pdfsSearched) {
        const pdfsToSort = this.pdfs.slice();
        const dca = this.decisionAspectSelected;
        const dcaMap = this.decisionAspectSelectedOrdering;
        sortByReject(pdfsToSort, pdfsToSort.map(
            x => `${x.changed ? 'a' : 'b'},${dcaMap[x[dca]]},${x.testfile}`));

        this.pdfsToShow = Object.freeze(pdfsToSort.slice(0, this.pdfsToShowMax));
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
          // Probably a PDF that was in the working subset but is no longer
          // there. Simply unset the search
          this.pdfsSearched = null;
        }
        else {
          this.pdfsToShow = [p];
          this.pdfsSearched = p;
        }
      }

      const refIds: {[key: string]: number} = {};
      for (let i = 0, m = this.pdfsToShow.length; i < m; i++) {
        refIds[this.pdfsToShow[i].testfile] = i;
      }
      const pdfsToShowReference = this.pdfsToShow.slice();
      for (const pp of this.pdfsReference) {
        const u = refIds[pp.testfile];
        if (u === undefined) continue;
        pdfsToShowReference[u] = pp;
      }
      this.pdfsToShowReference = Object.freeze(pdfsToShowReference);
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
              this.pdfsReference = Object.freeze(objs);
            } else this.pdfsReferenceFileError = "JSON does not match required format.";
          } else this.pdfsReferenceFileError = "Unable to read file contents.";
        };
      }
    },
    async _pdfGroupsUpdate() {
      if (this.analysisSetId === null) return;
      // Clear old data ; makes it apparent to user that data is being loaded.
      this.pdfGroups = {groups: {}, files: []};
      this.fileFilters = [];

      const opts = this._pdfGroupsSubsetOptions();
      const start = window.performance.now();
      // Avoid reactivity pause by freezing large object before assigning
      this.pdfGroups = Object.freeze(
          await this.$vuespa.call('decisions_get', opts));
      const tdelta = (window.performance.now() - start) / 1000;
      console.log(`Refreshing files took ${tdelta.toFixed(2)}s`);
    },
    _pdfGroupsSubsetOptions(filter: boolean=false) {
      const r: any = {analysis_set_id: this.analysisSetId};
      if (filter) {
        // Include restricted list of file ids, if needed
        if (this.fileFilters.length > 0) {
          r.file_ids = Array.from(this.fileFilterLatest()![1]);
        }
      }
      return r;
    },
  },
});
</script>
