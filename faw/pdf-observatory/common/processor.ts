import { DslResult, DslExpression } from './dsl'
import { PdfDecision } from './common';

/* These exports should go in to a different file */
export type PdfGroups = {
  groups: {[message: string]: Array<[number, number]>},
  files: Array<string>
};

export type FileFilterData = {
  name: string,
  skipped: boolean,
  files: Set<string>,
};


export function reprocess(decisionDefinition: DslResult, reprocessPdfGroups: PdfGroups): PdfDecision[] 
{ 
  // Build file list
  const newPdfs = [];
  const pdfMap = new Map<number, PdfDecision>();
  for (const [, files] of Object.entries(reprocessPdfGroups.groups)) {
    for (const f of files) {
      if (pdfMap.has(f[0])) continue;
      const dec = {
            testfile: reprocessPdfGroups.files[f[0]],
            info: [],
      };
      newPdfs.push(dec);
      pdfMap.set(f[0], dec);
    }
  }
  
  // Build sets of filter groups
  const pdfGroups = new Map<string, Set<number>>();
  const pdfGroupIsNegative = new Map<string, boolean>();
  for (const f of decisionDefinition.filters) {
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
    for (const [k, files] of Object.entries(reprocessPdfGroups.groups)) {
      let matched = false;
      // Do any of our filter's patterns match this message?
      let filesSubset = files;

      const evalCheck = (k: string, check: any): Set<number> => {
        const parts = new Map<string, Array<number>>();
        for (const [id, suffix] of [['sum', '_sum'], ['nan', '_nan'],
            ['count', '']]) {
          let filesWithMsg = reprocessPdfGroups.groups[k + suffix];
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

    for (const [oname, ocases] of decisionDefinition.outputs.entries()) {
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