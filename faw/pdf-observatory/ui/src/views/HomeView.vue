<template lang="pug">
mixin decision-criterion-selector
  div(v-if="decisionDefinition")
    v-select(
      label="Decision Criterion"
      v-model="decisionAspectSelected"
      :items=`
        decisionAspectAvailable.filter(
          x => x[1] !== 'filter-faw-custom' && x[1] !== 'filter-faw-errors'
        ).map(x => ({'title': x[0], 'value': x[1]})).concat([
          {'title': '(Custom Search)', 'value': 'filter-faw-custom'},
          {'title': '(Workbench Errors)', 'value': 'filter-faw-errors'}
        ])
      `
      item-text="title" /* In newer Vuetify versions this is spelled item-title */
      item-value="value"
    )
  //- Custom Search Box
  div(
    v-if="decisionAspectSelected === 'filter-faw-custom'"
    style="display: flex; flex-direction: row; align-items: center; gap: 1em"
  )
    v-text-field(
      label="Search Regex (press enter to reprocess)"
      v-on:keyup.enter="reprocess"
      v-model="decisionSearchCustom"
    )
    v-checkbox(v-model="decisionSearchInsensitive" label="Case-insensitive")
    v-btn(tile @click="reprocess" :disabled="!decisionDefinition") Reprocess + Search


mixin reprocess-button
  v-tooltip(bottom :disabled="!!decisionDefinition")
    template(v-slot:activator="{on}")
      //- Wrap disabled button in div -> show tooltip when disabled.
      div(v-on="on")
        v-btn.reprocess(
          @click="reprocess"
          :disabled="!decisionDefinition"
          color="primary"
        ) Reprocess decisions
    span(v-if="!decisionDefinition") Fix filter definition first.

mixin stale-decisions-alert
  v-alert(
    dense
    :type="(filtersModified || pdfGroupsDirty || pdfGroupsLoading) ? 'warning' : 'success'"
  )
    span(v-if="filtersModified") Filters have been modified; 'Reprocess' to update decisions
    span(v-else-if="pdfGroupsDirty") Data is stale; 'Reprocess' to download fresh data
    span(v-else-if="pdfGroupsLoading") Data is loading...
    span(v-else) Data is up-to-date

mixin confusion-matrix
  ConfusionMatrix(
    v-if="pdfs.length"
    @view="showFile($event)"
    @filter-file-list="fileFilterAdd($event.name, new Set($event.files))"
    :pdfs="pdfs"
    :pdfsReference="pdfsReference"
    :decisionAspectSelected="decisionAspectSelected"
    :decisionAspectSelectedName="decisionAspectSelected === 'filter-faw-custom' ? 'Search: ' + decisionSearchCustom : decisionAspectSelected"
  )

.home
  .loadingStatusDialog(
    v-if=`
      loadingStatus.files_max === 0
      || loadingStatus.files_parsing
      || loadingStatus.files_err
    `
  )
    v-card(color="grey darken-1" dark)
      v-card-text(style="padding-top: 0.5em;") {{loadingStatus.message}}
        v-progress-linear(
          :height="8"
          :indeterminate="loadingStatus.files_parsing !== 0"
          rounded
          :color="loadingStatus.files_err === 0 ? 'white' : 'red'"
          :value="loadingStatus.files_parsing ? 100 : 0"
        )
  .error(v-if="error" style="font-size: 4em; white-space: pre-wrap") ERROR - SEE CONSOLE

  .page-container
    v-expansion-panels.colored.expansion-panels(:multiple="true" :value="expansionPanels")
      v-expansion-panel(:key="0")
        v-expansion-panel-header Setup
        v-expansion-panel-content

          //- Analysis set config
          AnalysisSetConfig(
            :currentId.sync="analysisSetId"
            :pipeCfg="config && config.pipelines"
            @update="pdfGroupsDirty = true"
          )

          //- Filter DSL
            NOTE: MUST BE VISIBLE on page load. Otherwise decisionCodeEditor has issues.
          v-sheet(:elevation="3" style="padding: 1em; margin-block: 1em;")
            div
              AceEditor(
                ref="decisionCodeEditor"
                v-model="decisionCode"
                lang="yaml" /* for highlighting */
                @init="decisionCodeEditorInit"
                style="font-size: 1em"
              )
            div
              checkmark(:status="decisionDefinition ? 'valid' : 'rejected'")
              span compilation {{decisionDefinition ? 'succeeded' : 'failed'}}

      v-expansion-panel(:key="1", ref="resultsPanel")
        v-expansion-panel-header Results
        v-expansion-panel-content

          //- Action Buttons
          div(style="display: flex; flex-direction: row; flex-wrap: wrap; gap: 1em; margin-block: 1em")
            +reprocess-button
            v-btn.download(@click="downloadDecisions") Download decisions
            v-tooltip(bottom)
              template(v-slot:activator="{on}")
                div(v-on="on")
                  v-btn.download(@click="downloadFeatures") Download features
              span Downloads all features loaded in UI as a matrix of features x files; blank values indicate that a file does NOT have that feature.
            v-dialog(
              v-if="debugModeEnabled"
              v-model="resetDbDialog"
              persistent
              max-width="800"
            )
              template(v-slot:activator="{on}")
                v-btn.resetdb(v-on="on") Reset Entire DB
              v-card
                v-card-title Reset entire DB, re-running all tools and parsers?
                v-card-actions(:style="{'flex-wrap': 'wrap'}")
                  v-btn(@click="resetDbDialog=false") Cancel
                  v-btn(@click="reset(); resetDbDialog=false") Reset Entire DB (may take awhile)
          +stale-decisions-alert

          v-sheet(:elevation="3" style="margin-block: 1em; padding: 1em")
            div(v-if="fileFilters.length")
              span File Set Filters (Cumulative)
              v-btn(
                v-if="(fileFilters.length < 2) || !fileFilters[fileFilters.length - 2].skipped"
                @click="fileFilterInvert()"
                style="margin-inline: 1em"
              ) Invert last filter
              .file-filters
                v-btn.file-filter(
                  v-for="{name, skipped, files}, filterIndex in fileFilters"
                  :key="filterIndex"
                  @click="fileFilterPopTo(filterIndex)"
                  :title="(skipped ? 'Filter skipped due to later inversion;\\n' : '') + 'Click to delete this and subsequent filters'"
                )
                  v-icon mdi-delete-outline
                  span(
                    :class="{'file-filter-skipped': skipped}"
                  ) {{name}}
            //- Decision criterion selector (and search)
            +decision-criterion-selector
            //- Listing of reasons files failed
            v-expansion-panels(:value="0" v-if="decisionAspectSelected.startsWith('filter-')")
              v-expansion-panel(:key="0")
                v-expansion-panel-header.grey.lighten-2 All reasons files affected filter: {{decisionAspectSelected.substring(7)}}
                v-expansion-panel-content
                  .decision-reasons
                    v-radio-group(v-model="failReasonsSort" row :label="(failReasons.get(decisionAspectSelected) || []).length + ' error messages, sorted by'")
                      v-radio(value="total" label="number of files rejected")
                      v-radio(value="unique" label="uniquely rejected")
                    v-virtual-scroll(
                      :bench="10"
                      :items="failReasons.get(decisionAspectSelected) || []"
                      style="max-height: 200px"
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
                              v-btn(@click="fileFilterAdd(item[0], new Set([...Array.from(item[1][0]), ...Array.from(item[1][1])]))") Add File Set Filter
                              v-btn(v-clipboard="() => regexEscape(item[0])") (Copy regex to clipboard)
                              v-btn(v-clipboard="() => '^' + regexEscape(item[0]) + '$'") (with ^$)
                              v-btn(v-clipboard="() => JSON.stringify([...Array.from(item[1][1]), ...Array.from(item[1][0])])") (Copy file list as JSON)
                            v-list-item(
                              v-for="ex of [...sliceIterable(item[1][1], 0, 10), ...sliceIterable(item[1][0], 0, 10)].slice(0, 10)"
                              :key="ex"
                              @click="showFile(ex)"
                            ) {{ex}}

          //- Plot and table of file statuses
          v-sheet(:elevation="3" style="padding: 1em; margin-block: 1em;")
            div
              v-checkbox(v-model="plotShow" label="Show plot?")
            .plot-div(v-if="plotShow")
              plot(
                v-if="pdfs.length && decisionDefinition"
                v-model="pdfsSearchedUserAction"
                :pdfs="pdfs"
                :pdfsReference="pdfsReference"
                :decisionDefinition="decisionDefinition"
                :decisionAspectSelected="decisionAspectSelected"
              )
            +confusion-matrix

          //- Decision plugins
          v-expansion-panels
            v-expansion-panel
              v-expansion-panel-header.grey.lighten-2
                span
                  span Decision Plugins
                  span(v-if="fileFilters.length") {{' '}}(filtered)
              v-expansion-panel-content
                div(style="display: flex; align-items: center; gap: 1em")
                  v-select(
                    label="Decision Plugin"
                    v-model="selectedDecisionPlugin"
                    :items="uiPluginsDecision"
                    item-text="title" /* In newer Vuetify versions this is spelled item-title */
                    item-value="value"
                    @input="pluginDecisionView"
                  )
                  v-btn(
                    v-show="pluginDecIframeSrc != null || pluginDecIframeLoading"
                    @click="pluginDecIframeSrc = null; pluginDecIframeLoading = 0; selectedDecisionPlugin = null"
                    size="large"
                  ) Close Plugin
                div(v-show="pluginDecIframeSrc != null || pluginDecIframeLoading" style="border: solid 1px #000; position: relative; height: 95vh")
                  v-progress-circular(v-show="pluginDecIframeLoading" :indeterminate="true")
                  iframe(v-show="pluginDecIframeSrc != null" style="width: 100%; height: 100%" ref="pluginDecIframe")
                details(v-show="pluginDecIframeSrc != null")
                  summary Debugging stats
                  JsonTree(:data="pluginDecDebug" :level="2")

      v-expansion-panel(:key="2")
        v-expansion-panel-header Files
        v-expansion-panel-content
          .file-lists
            v-list(dense)
              v-subheader
                span(style="white-space: pre-wrap") Current decisions -!{' '}
                Stats(:pdfs="pdfs" :pdfsReference="pdfsReference" :decisionAspectSelected="decisionAspectSelected")
              v-list-item-group(mandatory)
                v-list-item(
                  v-for="p, ip of pdfsToShow"
                  :key="p.testfile"
                  @click="fileSelected = ip; scrollToFileDetails()"
                  :class=`{changed: p.changed,
                    'v-item--active': fileSelected === ip,
                    'v-list-item--active': fileSelected === ip}`
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
                        placeholder="Show specific file..."
                      )
            div(style="display: inline-block")
              v-btn(
                title="By default, the 'Reference Decisions' are initial decisions from page load.  Click this to switch to showing prior decision, and again to use the current results as the baseline."
                @click="makeBaseline"
                :color="holdReferences ? 'primary' : ''"
              )
                v-icon mdi-arrow-right
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
            v-btn(
              @click="dslReplaceForReferences" color="primary"
              style="grid-area: 3/3"
              title="Replace DSL with closest approximation of reference decisions based on available tools.  Note that order of outputs determines which of false positives or false negatives are minimized."
            ) Rationalizer (replaces DSL)

          //- Big margin-bottom to prevent scroll-back when changing file selection
          v-sheet(:elevation="3" style="margin-top: 1em; padding: 1em; margin-bottom: 50vh")
            v-subheader
              span Results for {{decisionSelected.testfile}}
              span(v-if="decisionSelected.testfile")
                span &nbsp;
                a(:href="'/file_download/' + decisionSelected.testfile") (download)
            v-expansion-panels(style="margin-bottom: 1em")
              v-expansion-panel
                v-expansion-panel-header.grey.lighten-2 File Detail Plugins
                v-expansion-panel-content
                  div(style="display: flex; align-items: center; gap: 1em")
                    v-select(
                      label="File Plugin"
                      v-model="selectedFilePlugin"
                      :items="uiPluginsFileDetail"
                      item-text="title" /* In newer Vuetify versions this is spelled item-title */
                      item-value="value"
                      @input="pluginFileDetailView"
                    )
                    v-btn(
                      v-show="pluginIframeSrc != null || pluginIframeLoading"
                      @click="pluginIframeSrc = null; pluginIframeLoading = 0; selectedFilePlugin = null"
                      size="large"
                    ) Close Plugin
                  div(
                    v-show="pluginIframeSrc != null || pluginIframeLoading"
                    style="border: solid 1px #000; position: relative; height: 95vh"
                  )
                    v-progress-circular(v-show="pluginIframeLoading" :indeterminate="true")
                    iframe(v-show="pluginIframeSrc != null" style="width: 100%; height: 100%" ref="pluginIframe")
            v-tabs(v-model="dbView" grow)
              v-tab(:key="DbView.Decision") Decision
              v-tab(:key="DbView.Combined") Output
            v-tabs-items(v-model="dbView" ref="detailView")
              v-tab-item(:key="DbView.Decision")
                FileFilterDetail(
                  :decisionDefinition="decisionDefinition"
                  :decisionSelected="decisionSelected"
                  :decisionSelectedDsl="decisionSelectedDsl"
                  :decisionReference="decisionReference"
                  :asOptions="_pdfGroupsSubsetOptions()"
                )
              v-tab-item(:key="DbView.Combined")
                CombinedDbView(:pdf="decisionSelected.testfile")
    //- Decisions Focus Panel
    v-expansion-panels(style="max-height: 50%")
      v-expansion-panel.fixed(ref="decisionsFocusPanel")
        v-expansion-panel-header.grey.lighten-2
          span
            v-icon(
              v-if="filtersModified || pdfGroupsDirty || pdfGroupsLoading"
              color="orange"
              style="margin-right: 1ch"
              small
            ) mdi-alert
            span Decisions
        v-expansion-panel-content
          div(style="display: flex; gap: 1em; margin-block: 1em")
            +reprocess-button
            +stale-decisions-alert(style="display: inline-block; margin: 0")
            v-btn(@click="scrollToResults") More under Results
          div(style="display: flex; align-items: flex-start; gap: 1em; margin-block: 1em")
            v-sheet(:elevation="3" style="padding: 1em")
              +decision-criterion-selector
            +confusion-matrix
</template>

<style lang="scss">
  @import '~vuetify/src/styles/settings/_colors.scss';

  html, body, #app {
    height: 100%;
    overflow-y: auto;
    overflow-x: auto;
  }

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
    height: 100%;
    display: flex;
    flex-direction: column;
    align-content: flex-start;
    justify-content: center;

    .page-container {
      width: 100%;
      height: 100%;
      display: flex;
      flex-direction: column;
    }

    .v-expansion-panels {
      flex-flow: column nowrap;
      justify-content: flex-start;
    }
    .v-expansion-panel {
      flex: none;
    }

    .expansion-panels { /* Only matches top-level expansion panels container */
      flex: 1 1 auto;
      overflow-y: scroll;
    }

    .v-expansion-panels.colored > .v-expansion-panel > .v-expansion-panel-header {
      color: #fff;
      background-color: var(--v-primary-base);
    }

    .v-expansion-panel-header.grey.v-expansion-panel-header--active {
      min-height: 48px; /* Same as inactive expansion panels */
    }

    .v-expansion-panel.fixed .v-expansion-panel-content__wrap {
      max-height: min(352px, 45vh);
      overflow-y: auto;
    }
    .v-expansion-panel.fixed[aria-expanded=false] > .v-expansion-panel-header {
      position: fixed;
      right: 1em;
      bottom: 1em;
      width: auto;
      box-shadow: /* Copied from Vuetify .elevation-3 */
        rgba(0, 0, 0, 0.2) 0px 3px 3px -2px,
        rgba(0, 0, 0, 0.14) 0px 3px 4px 0px,
        rgba(0, 0, 0, 0.12) 0px 1px 8px 0px;
    }
    .v-expansion-panel.fixed[aria-expanded=true] > .v-expansion-panel-header {
      width: 100%;
      box-shadow: rgba(0, 0, 0, 0.2) 0px -3px 3px -3px;
    }

    .v-input {
      margin-top: 0; /* Override an ugly default */
    }

    details {
      cursor: pointer;
      user-select: none;
      padding: 0.25em 0.1em;

      > summary {
      }
    }

    .file-filters {
      max-height: 10em;
      overflow-y: auto;
      padding: 1em;
      display: grid;
      grid-template-columns: 1fr;
      gap: 0.25em;
      .file-filter {
        justify-self: start;
        margin-left: 20px;  /* Size of icon */
        position: relative;
        cursor: pointer;
        .v-btn__content {
          position: revert;
        }
        .v-icon {
          position: absolute;
          right: 100%;
          color: transparent;
        }
      }
      .file-filter-skipped {
        opacity: 0.5;
      }
      .file-filter:hover,
      .file-filter:hover ~ .file-filter {
        span, .v-icon {
          color: #a00;
        }
      }
    }

    .v-alert {
      margin: 0;  /* Override an ugly default */
    }

    .reprocessdb {
      color: #fff !important;
      background-color: map-get($cyan, 'base') !important;
    }

    .resetdb {
      color: #fff !important;
      background-color: map-get($red, 'base') !important;
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

    .plot-div {
      overflow-x: auto;
    }
  }
</style>

<script lang="ts">
// @ is an alias to /src
//import HelloWorld from '@/components/HelloWorld.vue'

import bus from '@/bus';
import {regexEscape} from '@/util';

// Editor-related includes
import AceEditorComponent from 'vue2-ace-editor';
import 'brace/mode/yaml';
import 'brace/theme/chrome';
// Needed for ctrl+F
import 'brace/ext/searchbox';

import Vue from 'vue';

import {PdfDecision, sortByReject} from '@/common/common';
import {DslExpression, DslResult, dslParser, dslDefault} from '@/common/dsl';
import {AsData} from '@/interface/aset';

import AnalysisSetConfigComponent from '@/components/AnalysisSetConfig.vue';
import CheckmarkComponent from '@/components/CheckmarkComponent.vue';
import CirclePlotComponent from '@/components/circle-plot.vue';
import ConfusionMatrixComponent from '@/components/HomeConfusionMatrix.vue';
import StatsComponent from '@/components/HomeStats.vue';
import DbViewComponent from '@/components/DbView.vue';
import CombinedDbViewComponent from '@/components/CombinedDbView.vue';
import FileFilterDetailComponent from '@/components/FileFilterDetail.vue';

import { PdfGroups, FileFilterData, reprocess as reprocessCommon } from '@/common/processor'

export enum DbView {
  Decision = 0,
  Tools = 1,
  Parsers = 2,
  Stats = 3,
  Combined = 4,
}


export class LoadingStatus {
  config_mtime: number = 0;
  files_parsing: number = 0;
  files_max: number = 0;
  files_err: number = 0;
  message: string = '<Loading>';
}

export default Vue.extend({
  name: 'HomeView',
  components: {
    AceEditor: AceEditorComponent,
    AnalysisSetConfig: AnalysisSetConfigComponent,
    checkmark: CheckmarkComponent,
    ConfusionMatrix: ConfusionMatrixComponent,
    DbView: DbViewComponent,
    CombinedDbView: CombinedDbViewComponent,
    FileFilterDetail: FileFilterDetailComponent,
    plot: CirclePlotComponent,
    Stats: StatsComponent,
  },
  data() {
    return {
      analysisSetId: null as null | string,
      asData: {asets: [], parsers: []} as AsData,
      beforeDestroyFns: [] as Array<{(): any}>,
      config: null as any,
      dbView: DbView.Decision,
      DbView: DbView,
      debugModeEnabled: (process.env.VUE_APP_DEBUG == 'true') as boolean,
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
      failReasonsSort: 'total',
      fileFilters: new Array<FileFilterData>(),
      fileSelected: 0,
      filtersModified: false as boolean,
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
      pdfGroupsLoading: false,
      plotShow: true,
      pluginIframeLast: '',
      pluginIframeLoading: 0,
      pluginIframeLoadingNext: 1,
      pluginIframeSrc: null as string|null,
      pluginIframeSrcMimeType: 'text/html',
      pluginDecDebug: null as any,
      pluginDecIframeLast: '',
      pluginDecIframeLoading: 0,
      pluginDecIframeLoadingNext: 1,
      pluginDecIframeSrc: null as string|null,
      reprocessInnerInit: true,
      reprocessInnerPdfGroups: true,
      resetDbDialog: false,
      selectedDecisionPlugin: null as any,
      selectedFilePlugin: null as any,
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
    uiPluginsDecision(): Array<{"title": string, "value": string}> {
      return this.pluginOptionsInCategory('decision_views');
    },
    uiPluginsFileDetail(): Array<{"title": string, "value": string}> {
      return this.pluginOptionsInCategory('file_detail_views');
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
    failReasonsSort() {
      this.updateFailReasonsSort();
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
        bus.error!(e);
        throw e;
      }
    },
    decisionCodeEditorInit(editor: any) {
      editor.setOption('tabSize', 2);
    },
    decisionCodeUpdated() {
      this.filtersModified = true;
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
      catch (e: any) {
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
    downloadDecisions() {
      const json = JSON.stringify(this.pdfs);
      this._downloadFile(json, 'decisions.json');
    },
    downloadFeatures() {
      const csvLines = [];
      csvLines.push(['feature'].concat(this.pdfGroups.files));
      for (const [ft, files] of Object.entries(this.pdfGroups.groups)) {
        const csvLine = [ft];
        const okSet = new Map(files);
        for (let i = 0, j = this.pdfGroups.files.length; i < j; i++) {
          const v = okSet.get(i);
          if (v === undefined) csvLine.push('');
          else csvLine.push(v.toString());
        }
        csvLines.push(csvLine);
      }
      // CSV Writer
      const csv = [];
      let first = true;
      let cellCount;
      for (const line of csvLines) {
        if (first) {
          cellCount = line.length;
          first = false;
        }
        else csv.push('\n');

        if (cellCount !== line.length) {
          throw new Error(`Invalid CSV lengths? ${cellCount} != ${line.length}`);
        }

        let firstCell = true;
        for (const cell of line) {
          if (firstCell) firstCell = false;
          else csv.push(',');

          let sCell = cell.toString();
          if (/['"\n,;\t]/.test(sCell)) {
            sCell = sCell.replace(/"/g, '""');
            sCell = `"${sCell}"`;
          }
          csv.push(sCell);
        }
      }
      this._downloadFile(csv.join(''), 'features.csv');
    },
    _downloadFile(content: string, name: string) {
      let blob = new Blob([content], { type: 'text/plain;charset=utf-8;' });
      let link = document.createElement('a');
      let url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', name);
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
        if (k.caseInsensitive)
          header += '/i';
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
      this.fileFilters.push({'name': name, 'skipped': false, 'files': files});
    },
    fileFilterInvert() {
      if (this.fileFilters.length === 0) {
        return;
      }

      const last = this.fileFilterLatest()!;
      let prev: FileFilterData;
      if (this.fileFilters.length > 1) {
        prev = this.fileFilters[this.fileFilters.length - 2];
        if (prev.skipped) {
          // last is the inversion of prev. We can just drop last instead of adding a new filter
          this.fileFilters.pop()
          prev.skipped = false;
          return;
        }
      }
      else {
        prev = {
          'name': '(all)',
          'skipped': true,
          'files': new Set(this.pdfGroups.files)
        };
      }

      this.fileFilters.push(
        {
          "name": last.name + ' (inverted)',
          "skipped": false,
          "files": new Set([...prev.files].filter(x => !last.files.has(x)))
        }
      );
      last.skipped = true;
    },
    fileFilterLatest(): undefined|FileFilterData {
      if (this.fileFilters.length === 0) return;
      return this.fileFilters[this.fileFilters.length - 1];
    },
    fileFilterPopTo(idx: number) {
      this.fileFilters.splice(idx, this.fileFilters.length);
      if (this.fileFilters.length > 0) {
        const last = this.fileFilters[this.fileFilters.length - 1]
        last.skipped = false;
      }
    },
    makeBaseline() {
      this.holdReferences = !this.holdReferences;
      if (this.holdReferences) {
        this.pdfsReference = this.pdfs;
      }
    },
    pluginDecisionView(pluginKey: string, jsonArgs: {[key: string]: any} = {}) {
      if (this.pluginDecIframeLoading && this.pluginDecIframeLast === pluginKey) {
        return;
      }

      const loadKey = this.pluginDecIframeLoadingNext++;
      this.pluginDecIframeLast = pluginKey;
      this.pluginDecIframeLoading = loadKey;
      this.pluginDecIframeSrc = null;
      this.pluginDecDebug = null;
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
            const [, pipeline, plugin] = pluginKey.split('!');
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
          this.pluginDecDebug = r.debug;

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
        catch (e: any) {
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
        catch (e: any) {
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
    /** Return a list of readable plugin names and keys in a given category,
     * including any analysis set + pipeline combinations.
     */
    pluginOptionsInCategory(category: string): Array<{"title": string, "value": string}> {
      if (!this.config) return [];
      const pluginOptions: Array<{"title": string, "value": string}> = [];
      //Object.entries(uiPluginsDecision).map(([pluginKey, plugin]) => ({'title': plugin.label, 'value': pluginKey}))"
      for (const [pluginKey, plugin] of Object.entries(this.config[category]) as [string, any][]) {
        pluginOptions.push({"title": plugin.label, "value": pluginKey});
      }
      for (const aset of this.asData.asets) {
        for (const [pk] of Object.entries(aset.pipelines)) {
          // Renamed / deleted
          if (!this.config.pipelines[pk]) continue;
          for (const [ppk, ppv] of Object.entries(this.config.pipelines[pk][category]) as [string, any][]) {
            const pluginKey = `${aset.id}!${pk}!${ppk}`;
            pluginOptions.push({"title": `${pk} -- ${ppv.label} [${aset.id}]`, "value": pluginKey});
          }
        }
      }
      // Sort alphabetically by display name
      pluginOptions.sort((a, b) => {
        if (a.title < b.title) {
          return -1;
        } else if (a.title > b.title) {
          return 1;
        } else {
          return 0;
        }
      })
      return pluginOptions;
    },
    regexEscape(v: string): string {
      return regexEscape(v);
    },
    async reprocess() {
      /** Re-calculate decisions based on DSL */
      if (this.reprocessInnerInit) {
        // Loading... we'll be back here.
        return;
      }
      
      if (!this.reprocessInnerPdfGroups && this.pdfGroupsDirty) {
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
      this.filtersModified = false;

      // OK, everything needed fetched, go ahead and run decisions.
      this.reprocessInnerPdfGroups = false;
      if (!this.holdReferences) {
        this.pdfsReference = this.pdfs;
      }
    
      // Narrow down to only groups pertaining to selected files
      let groups: {[message: string]: Array<[number, number]>} = this.pdfGroups.groups;
      if (this.fileFilters.length > 0) {
        const fset = this.fileFilterLatest()!.files;
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
      
      let pdfGroups: PdfGroups = {
        groups: groups,
        files: this.pdfGroups.files
      };
      
      let newPdfs = reprocessCommon(dd, pdfGroups);
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
    scrollToFileDetails() {
      // Show the user that a new file has been searched
      (this.$refs.detailView as Vue).$el.scrollIntoView();
    },
    scrollToResults() {
      // Jump to the Results panel. Ideally we'd also open it, and close
      // the decisions focus panel, but there don't seem to be hooks for
      // those actions.
      const resultsPanel = (this.$refs.resultsPanel as Vue).$el;
      resultsPanel.scrollIntoView();
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
        frMap.set(`filter-${filt}`, filtData);
      }
      this.failReasons = Object.freeze(frMap);
      this.updateFailReasonsSort();
    },
    updateFailReasonsSort() {
      const frMap = new Map<string, Array<[string, [Set<string>, Set<string>]]>>();
      for (const [filt, data] of this.failReasons.entries()) {
        const filtData = data.slice();
        if (this.failReasonsSort === 'total') {
          filtData.sort((a, b) => b[1][0].size - a[1][0].size);
        }
        else if (this.failReasonsSort === 'unique') {
          filtData.sort((a, b) => 1000000 * (b[1][1].size - a[1][1].size) + b[1][0].size - a[1][0].size);
        }
        else {
          throw new Error(`Bad fail reason sort: ${this.failReasonsSort}`);
        }
        frMap.set(filt, filtData);
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
      this.pdfGroupsDirty = false;
      this.pdfGroups = {groups: {}, files: []};
      this.fileFilters = [];

      this.pdfGroupsLoading = true;
      try {
        const opts = this._pdfGroupsSubsetOptions();
        const start = window.performance.now();
        // Avoid reactivity pause by freezing large object before assigning
        this.pdfGroups = Object.freeze(
            await this.$vuespa.call('decisions_get', opts));
        const tdelta = (window.performance.now() - start) / 1000;
        console.log(`Refreshing files took ${tdelta.toFixed(2)}s`);
      }
      finally {
        this.pdfGroupsLoading = false;
      }
    },
    _pdfGroupsSubsetOptions(filter: boolean=false) {
      const r: any = {analysis_set_id: this.analysisSetId};
      if (filter) {
        // Include restricted list of file ids, if needed
        if (this.fileFilters.length > 0) {
          r.file_ids = Array.from(this.fileFilterLatest()!.files);
        }
      }
      return r;
    },
  },
});
</script>
