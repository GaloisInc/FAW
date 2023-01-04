
//import {strict as assert } from 'assert';
import { readFileSync, writeFile } from 'fs'
import { stdin as input, stdout as output, argv } from 'process'

import { DslResult, DslExpression, dslParser } from './common/dsl'
import { FileFilterData, reprocess } from './common/processor';

//Parse the incoming data in to JSON and check for keys
var data = readFileSync(input.fd, 'utf-8');
let parsedData = JSON.parse(data);

if (!('groups' in parsedData)) {
  console.error("Input data does not contain key `groups`");
  throw new Error("Invalid input");
}
if (!('dsl' in parsedData)) {
  console.error("Input data does not contain key `dsl`");
  throw new Error("Invalid input");
}

let pdfGroups = parsedData.groups

//Parse the DSL
var dslText = parsedData.dsl ;
console.error("DEBUG: START DSL\n");
console.error(dslText);
console.error("DEBUG: END DSL\n");
var dsl = dslParser.parse(dslText) as DslResult;

//Reprocess decisions
console.error('Reprocessing decisions ...');
var decisions = reprocess(dsl, pdfGroups)
console.error('Reprocessing decisions ... Completed');

//Write it out
var result = JSON.stringify(decisions);
console.error('Streaming out decisions ...');
writeFile(output.fd, result, (err) => {
  if (err) throw err;
  console.error('Streaming out decisions ... Completed');
});