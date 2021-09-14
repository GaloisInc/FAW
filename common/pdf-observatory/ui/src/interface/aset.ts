
/** Filter requirement for feature */
export interface AsFeature {
  parser: string;
  ft: string;
  ft_case?: boolean;
}

export interface AsRule {
  parser: string;
  filesets?: Array<string>;
  src: string;
  src_case?: boolean;
  dst: string;
}

export interface AsParser {
  id: string;
  size_doc: number;
  pipeline: boolean;
  filesets?: Array<string>;
}

export interface AsPipeline {
  done: boolean;
}

/** Contains information required to regenerate the analysis set
  */
export interface AsDefinition {
  files: string;
  files_case: boolean;
  features?: Array<AsFeature>;
  sample: number;
  rules: Array<AsRule>;
}

export interface AsStatus {
  id: string;
  size_docs: number;
  size_disk: number;
  status: string;
  status_done_time: number;
  pipelines: {[key: string]: AsPipeline},
  definition: AsDefinition,
}
export namespace AsStatus {
  export function makeEmpty(id: string, parsers?: Array<AsParser>): AsStatus {
    const r: AsStatus = {
      id,
      size_docs: 0,
      size_disk: 0,
      status: '',
      status_done_time: 0,
      pipelines: {},
      definition: {
        files: '',
        files_case: false,
        sample: 0,
        rules: [],
      },
    };

    if (parsers) {
      for (const p of parsers) {
        // Include anything adding 10 or fewer bytes per file by default
        if (p.size_doc > 10) continue;
        r.definition.rules.push({parser: p.id, src: '', dst: ''});
      }
    }

    return r;
  }
}

export interface AsData {
  asets: Array<AsStatus>;
  parsers: Array<AsParser>;
}

