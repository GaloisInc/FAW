import { DslResult, DslExpression, DslFilterPattern } from './dsl'
import { PdfDecision } from './common';

export type PdfGroups = {
  /*
   * message - string output from a parser (may be post-processed)
   * Values are pairs:
   *    1. Index into `files` of file that produced this message
   *    2. Numeric feature value, or 1 for binary features
   */
  groups: {[message: string]: Array<[number, number]>},
  /*
   * File names
   */
  files: Array<string>
};

export type ReprocessResult = {
  decisions: PdfDecision[],
  extraFeaturesByFile: Map<string, string[]>,
}

export function reprocess(
  decisionDefinition: DslResult,
  reprocessPdfGroups: PdfGroups
): ReprocessResult {
  // Build file list first
  const newDecisions: Array<PdfDecision> = [];
  const decisionsByFileIndex = new Map<number, PdfDecision>();
  for (const [, files] of Object.entries(reprocessPdfGroups.groups)) {
    for (const [fileIndex, featureValue] of files) {
      if (decisionsByFileIndex.has(fileIndex)) continue;
      const decision: PdfDecision = {
        testfile: reprocessPdfGroups.files[fileIndex],
        info: [],
      };
      newDecisions.push(decision);
      decisionsByFileIndex.set(fileIndex, decision);
    }
  }

  // Build sets of filter groups
  const filesWithMessages = Object.entries(reprocessPdfGroups.groups);

  function evalCheck(message: string, check: any, identifier: string): Set<number> {
    const parts = new Map<string, Array<number>>();
    for (const [id, suffix] of [
      ['sum', '_sum'],
      ['nan', '_nan'],
      ['count', '']
    ]) {
      const filesWithMsg = reprocessPdfGroups.groups[message + suffix];
      if (filesWithMsg === undefined) {
        if (['sum', 'nan'].indexOf(message.split('_').pop()!) !== -1) {
          // If we can't find this, this is NOT a number, but likely a
          // subfield of a number (e.g., we're looking at _nan_sum)
          // So, act like no match.
          return new Set();
        }
        throw new Error(
          `Could not evaluate check for \`${identifier}\` matching non-numeric message: ${message}`
        );
      }

      const p = new Array<number>();
      parts.set(id, p);
      for (const [fileIndex, featureValue] of filesWithMsg) {
        p[fileIndex] = featureValue;
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
      else if (check.type === '==') {
        const left = evalInner(check.id1);
        const right = evalInner(check.id2);
        return left.map((l, i) => l == right[i] ? 1 : 0);
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
          throw new Error(
            `Could not evaluate check for ${identifier}: No such `
            + `quantity ${check.id1} for message: ${message}`
          );
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
      throw new Error(
        `Could not evaluate check for ${identifier}: Unrecognized check type ${check.type}`
      );
    };

    const r = evalInner(check);
    return new Set(r.map((x, i) => [x, i]).filter(x => !!x[0]).map(x => x[1]));
  };

  function fileIndicesMatchingFilter(
    patterns: Array<DslFilterPattern>,
    caseInsensitive: boolean,
    conjunction: boolean,
    // Only used for saving info
    identifier: string,
  ): Set<number> {
    const rules = patterns.map(p => ({
      pat: new RegExp(p.pat, caseInsensitive ? 'i' : undefined),
      check: p.check,
    }));
    if (!conjunction) {
      // OR rule; can nicely short-circuit when iterating message-major
      // set of files matching this filter (so far)
      const fileIndices = new Set<number>();

      for (const [message, files] of filesWithMessages) {
        // Do any of our filter's patterns match this message?
        const filesSubset = new Set<number>();

        for (const r of rules) {
          if (r.pat.test(message)) {
            // Do we need to constrain the matching set based on an auxiliary
            // check?
            if (r.check === null) {
              // Accept all matches; short-circuit
              for (const [fileIndex, ] of files) {
                filesSubset.add(fileIndex);
              }
              break;
            } else {
              const filesPassingCheck = evalCheck(message, r.check, identifier);
              // Merge `filesPassingCheck` into `filesSubset`
              filesPassingCheck.forEach(filesSubset.add, filesSubset);
            }
          }
        }
        const acceptanceMessage = `'${identifier}' accepted '${message}'`
        for (const fileIndex of filesSubset) {
          decisionsByFileIndex.get(fileIndex)!.info.push(acceptanceMessage);
          fileIndices.add(fileIndex);
        }
      }
      return fileIndices;
    } else {
      const fileIndices = new Set<number>(decisionsByFileIndex.keys());

      const acceptancesByFile = new Map<number, string[]>();
      for (const fileIndex of fileIndices) {
        const acceptances: string[] = [];
        acceptancesByFile.set(fileIndex, acceptances);
      }

      rules.forEach((r, i) => {
        // files matching `r`
        const filesSubset = new Set<number>();

        for (const [message, files] of filesWithMessages) {
          if (r.pat.test(message)) {
            const acceptanceMessage = `'${identifier}' accepted '${message}'`
            if (r.check === null) {
              for (const [fileIndex, ] of files) {
                filesSubset.add(fileIndex);
                acceptancesByFile.get(fileIndex)?.push(acceptanceMessage);
              }
              continue;  // can't short circuit here like above due to rule-major iteration
            }
            const filesPassingCheck = evalCheck(message, r.check, identifier);
            for (const fileIndex of filesPassingCheck) {
              filesSubset.add(fileIndex);
              acceptancesByFile.get(fileIndex)?.push(acceptanceMessage);
            }
          }
        }
        // Now remove files that _didn't_ match `r` from consideration
        for (const fileIndex of fileIndices) {
          if (!filesSubset.has(fileIndex)) {
            fileIndices.delete(fileIndex);
          }
        }
      })
      for (const fileIndex of fileIndices) {
        const decisionReasonsToAdd = acceptancesByFile.get(fileIndex)!;
        const decisionReasons = decisionsByFileIndex.get(fileIndex)!.info;
        for (const s of decisionReasonsToAdd) {
          decisionReasons.push(s);
        }
      }
      return fileIndices;
    }
  }

  const fileIndicesByExtraFeatureText = new Map<string, Set<number>>();
  for (const extraFeature of decisionDefinition.extraFeatures) {
    const fileIndices = fileIndicesMatchingFilter(
      extraFeature.patterns,
      extraFeature.caseInsensitive,
      extraFeature.conjunction,
      extraFeature.featureText,
    );
    fileIndicesByExtraFeatureText.set(
      extraFeature.featureText, fileIndices
    );
  }

  // Add the new messages so that filters can use them
  for (const [featureText, fileIndices] of fileIndicesByExtraFeatureText.entries()) {
    filesWithMessages.push([featureText, Array.from(fileIndices, fileIndex => [fileIndex, 1])]);
  }

  const fileIndicesByFilterName = new Map<string, Set<number>>();
  for (const filter of decisionDefinition.filters) {
    const fileIndices = fileIndicesMatchingFilter(
      filter.patterns,
      filter.caseInsensitive,
      filter.conjunction,
      filter.name,
    );
    fileIndicesByFilterName.set(filter.name, fileIndices);
  }

  // Populate decision objects with filter results (boolean) and outputs (string)
  for (const [fileIndex, decision] of decisionsByFileIndex.entries()) {
    const evalExpr = (node: DslExpression): boolean => {
      if (node.type === 'else') return true;
      if (node.type === 'not') return !evalExpr(node.id1);
      if (node.type === 'and') return evalExpr(node.id1) && evalExpr(node.id2);
      if (node.type === 'or') return evalExpr(node.id1) || evalExpr(node.id2);
      if (node.type === 'id') {
        let fileList = fileIndicesByFilterName.get(node.id1);
        if (fileList === undefined) {
          throw new Error(`Undefined filter ${node.id1}`);
        }
        return fileList.has(fileIndex);
      }
      throw new Error(`Unknown node type ${(node as any).type}`);
    };

    for (const k of fileIndicesByFilterName.keys()) {
      decision[`filter-${k}`] = evalExpr({type: 'id', id1: k});
    }

    for (const [oname, ocases] of decisionDefinition.outputs.entries()) {
      decision[oname] = 'unspecified';
      for (const [value, expression] of ocases) {
        if (expression === null) continue;

        if (evalExpr(expression)) {
          decision[oname] = value;
          break;
        }
      }
    }
  }

  const extraFeaturesByFile = new Map<string, string[]>();
  for (const [featureText, fileIndices] of fileIndicesByExtraFeatureText.entries()) {
    for (const fileIndex of fileIndices) {
      let extraFeatures = extraFeaturesByFile.get(reprocessPdfGroups.files[fileIndex]);
      if (extraFeatures === undefined) {
        extraFeatures = [];
        extraFeaturesByFile.set(reprocessPdfGroups.files[fileIndex], extraFeatures);
      }
      extraFeatures.push(featureText);
    }
  }

  return {
    decisions: newDecisions,
    extraFeaturesByFile: extraFeaturesByFile
  };
}
