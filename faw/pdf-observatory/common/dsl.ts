import peg from 'pegjs';

// Parse statistics descriptions like:
// Now overwritten by .json5 configuration file.
export const dslDefault = `# Loading...`;

export interface DslResult {
  extraFeatures: Array<DslExtraFeature>;
  filters: Array<DslFilter>;
  outputs: Map<string, DslOutput>;
}

export interface DslExtraFeature {
  featureText: string;
  all: boolean;
  caseInsensitive: boolean;
  patterns: Array<DslFilterPattern>;
}

export interface DslFilter {
  name: string;
  all: boolean;
  caseInsensitive: boolean;
  patterns: Array<DslFilterPattern>;
}

export interface DslFilterPattern {
  pat: string;
  check: any | null;
}

export type DslOutput = Array<[string, DslExpression | null]>;

export type DslExpressionOr = {
  type: 'or',
  id1: DslExpression,
  id2: DslExpression,
};
export type DslExpressionAnd = {
  type: 'and',
  id1: DslExpression,
  id2: DslExpression,
};
export type DslExpressionId = {
  type: 'id',
  id1: string,
};
export type DslExpressionNot = {
  type: 'not',
  id1: DslExpression,
};
export type DslExpressionElse = {
  type: 'else',
};
export type DslExpression = DslExpressionAnd | DslExpressionElse
    | DslExpressionId | DslExpressionNot | DslExpressionOr;


function generateDslParser() {
  try {
    return peg.generate(`
      {
        let fileIndent = 0;
        let indentStack = [];

        function indentPush() {
          indentStack.push(fileIndent);
          return true;
        }
        function indentPop() {
          fileIndent = indentStack.pop();
        }
      }

      start
        = WS_INIT extraFeatures:extra_features_expr WS_LINES filters:filters_expr WS_LINES outputs:outputs_expr WS_LINES WS_MAYBE COMMENT?
            { return { extraFeatures: extraFeatures, filters: filters, outputs: outputs }; }

      extra_features_expr
        = (
            "features:" WS_LINES
            &{ return indentPush(); }
            inner:(INDENT extra_features_def+)?
            &{ indentPop(); return inner; }
            { return inner[1]; }
          ) / ( "" { return []; } )

      filters_expr
        = "filters:" WS_LINES &{ return indentPush(); } inner:(INDENT filters_def+)? &{ indentPop(); return inner; }
            { return inner[1]; }

      regex_flags
        = flags:("/" regex_flag*)? {
          return flags && flags[1].length ? Object.assign.apply(null, flags[1]) : {}; }

      regex_flag
        = "i" {
          return { caseInsensitive: true }; }

      extra_features_def
        = INDENT_CHECK featureText:feature_text re:regex_flags all:(WS "all")? ":"
            WS_LINES
            &{ return indentPush(); }
            inner:(INDENT filters_pattern+)?
            &{ indentPop(); return inner; } {
              return { featureText: featureText, all: !!all, caseInsensitive: !!re.caseInsensitive, patterns: inner[1] }; }

      feature_text
        = $[A-Za-z0-9_\\-=+<>()\\[\\],.?; ]+

      filters_def
        = INDENT_CHECK name:filter_name re:regex_flags all:(WS "all")? ":"
            WS_LINES
            &{ return indentPush(); }
            inner:(INDENT filters_pattern+)?
            &{ indentPop(); return inner; } {
              return { name: name, all: !!all, caseInsensitive: !!re.caseInsensitive, patterns: inner[1] }; }

      filter_name
        = leader:[A-Z] trailer:ID_CHARS { return leader + trailer; }

      filters_pattern
        = INDENT_CHECK pattern:[^\\n]+ WS_LINES patternCheck:filters_pattern_check? {
          let pat = pattern.join('').trim();
          try { new RegExp(pat); }
          catch (e) {
            expected('Valid regular expression ((' + e + '))');
          }
          return {pat: pat, check: patternCheck};
        }

      filters_pattern_check
        = &{ return indentPush(); } inner:(INDENT INDENT_CHECK filters_pattern_check_expr)? &{ indentPop(); return inner; } WS_LINES
            { return inner[2]; }

      filters_pattern_check_expr
        = left:filters_pattern_check_expr_and WS* "|" WS* right:filters_pattern_check_expr { return {type: 'or', id1: left, id2: right}; }
        / filters_pattern_check_expr_and

      filters_pattern_check_expr_and
        = left:filters_pattern_check_expr_compare WS* "&" WS* right:filters_pattern_check_expr_and { return {type: 'and', id1: left, id2: right}; }
        / filters_pattern_check_expr_compare

      filters_pattern_check_expr_compare
        = left:filters_pattern_check_expr_add WS* symb:(">" / ">=" / "<" / "<=" / "==") WS* right:filters_pattern_check_expr_add
            { return {type: symb, id1: left, id2: right}; }

      filters_pattern_check_expr_add
        = left:filters_pattern_check_expr_mul WS* symb:("+" / "-") WS* right:filters_pattern_check_expr_add
            { return {type: symb, id1: left, id2: right}; }
        / filters_pattern_check_expr_mul

      filters_pattern_check_expr_mul
        = left:filters_pattern_check_expr_neg WS* symb:("*" / "/") WS* right:filters_pattern_check_expr_mul
            { return {type: symb, id1: left, id2: right}; }
        / filters_pattern_check_expr_neg

      filters_pattern_check_expr_neg
        = symb:"-" left:filters_pattern_check_expr_id
            { return {type: 'neg', id1: left}; }
        / filters_pattern_check_expr_id

      filters_pattern_check_expr_id
        = name:("sum" / "count") { return {type: 'id', id1: name}; }
        / val:("-"? [0-9]+ ("." [0-9]+)? ("e" "-"? [0-9]+)?) { return {type: 'number', id1: parseFloat(text())}; }
        / "(" WS* left:filters_pattern_check_expr_add WS* ")" { return left; }

      outputs_expr
        = INDENT_CHECK "outputs:" WS_LINES
            &{ return indentPush(); }
            inner:(INDENT outputs_group+)?
            &{ indentPop(); return inner; } {
              let res = new Map();
              for (let group of inner[1]) {
                res.set(group.name, group.values)
              }
              return res;
            }

      outputs_group
        = INDENT_CHECK
          name:outputs_group_name
          ":"
          WS_LINES
          &{ return indentPush(); }
          inner:(INDENT outputs_group_values+)?
          &{ indentPop(); return inner; } {
            return {name: name, values: inner[1]};
          }

      outputs_group_values
        = INDENT_CHECK
          ["] value:[^"]+ ["]
          opt_assign:outputs_group_values_assign?
          WS_LINES
          { return [value.join(''), opt_assign]; }

      outputs_group_values_assign
        = WS "is" WS
          expr:expr
          { return expr; }
        / WS "else"
          { return {type: "else"}; }

      ID_CHARS = cs:[A-Za-z0-9_-]* { return cs.join('') }

      outputs_group_name
        = head:[a-z] tail:ID_CHARS { return head + tail; }

      expr
        = left:expr_and WS* "|" WS* right:expr { return {type: 'or', id1: left, id2: right}; }
        / expr_and

      expr_and
        = left:expr_not WS* "&"  WS* right:expr_and { return {type: 'and', id1: left, id2: right}; }
        / expr_not

      expr_not
       = "!" id:expr_not { return {type: 'not', id1: id}; }
       / expr_paren

      expr_paren
       = "(" WS_MAYBE e:expr WS_MAYBE ")" { return e; }
       / id:filter_name { return {type: 'id', id1: id}; }

      WS_INIT
        = WS_LINES indent:" "* {
            fileIndent = indent.length;
            return null;
          }

      INDENT
        = &{ fileIndent += 2; return true; }

      INDENT_CHECK "appropriate indentation"
        = spaces:" "* &{ return spaces.length === fileIndent; }

      WS
        = WS_CHARS

      WS_MAYBE
        = WS_CHARS?

      WS_LINES "whitespace and comments"
        = (WS_MAYBE COMMENT? NEWLINE)*

      COMMENT "comment"
        = "#" [^\\n]*

      WS_CHARS "whitespace"
        = [ \\t]+

      NEWLINE
        = [\\n]
    `);
  }
  catch (e) {
    console.log(e);
    throw e;
  }
}
export const dslParser = generateDslParser();
