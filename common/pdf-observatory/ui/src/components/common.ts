
// Class mimicking Kudu expected output
export type PdfDecision = {
  info: string,
  testfile: string,

  [outputGroup: string]: any,

  changed?: boolean,
};

/** Sort the given data based on the second element, which is presumed
  to be the 'valid'|'rejected' status.
  */
export function sortByReject(data: Array<any>, key: Array<string>) {
  const idx = [];
  for (let i = 0, m = data.length; i < m; i++) {
    idx.push(i);
  }
  // Sort based on the given keys
  idx.sort((a, b) => {
    return key[a].localeCompare(key[b]);
  });

  const odata = data.slice();
  for (let i = 0, m = data.length; i < m; i++) {
    data[i] = odata[idx[i]];
  }
}

