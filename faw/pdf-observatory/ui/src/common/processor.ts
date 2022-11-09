/* TODO: Move DSL in to this package */
import { DslResult, DslExpression } from '@/dsl'

/* These exports should go in to a different file */
export type PdfGroups = {
  groups: {[message: string]: Array<[number, number]>},
  files: Array<string>
};

export type FileFilterData = [string, Set<string>];

// Class mimicking Kudu expected output
export type PdfDecision = {
  info: string[],
  testfile: string,

  [outputGroup: string]: any,

  changed?: boolean,
};


export class Processor {
  public reprocessInnerPdfGroups: boolean ;
  pdfGroupsDirty: boolean ; // Maybe external state
  
  constructor(
    private decisionDefinition: DslResult, private decisionAspectSelected: string, 
    private decisionSearchInsensitive: boolean, private decisionSearchCustom: string,
    private pdfGroups: PdfGroups, private fileFilters: Array<FileFilterData>
  ) {
    this.reprocessInnerPdfGroups = false;
    this.pdfGroupsDirty = false;
  }
  
  fileFilterLatest(): undefined|FileFilterData {
    if (this.fileFilters.length === 0) return;
    return this.fileFilters[this.fileFilters.length - 1];
  }
  
  reprocess(): PdfDecision[] {
    const dd = this.decisionDefinition;
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
    //TODO:
    // if (!this.holdReferences) {
    //   this.pdfsReference = this.pdfs;
    // }

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
    for (const [, files] of Object.entries(groups)) {
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

    return newPdfs as unknown as PdfDecision[];
  }
}