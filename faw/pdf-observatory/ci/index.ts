
//import {strict as assert } from 'assert';
import { readFileSync, writeFile } from 'fs'
import { stdin as input, stdout as output, argv } from 'process'

import { DslResult, DslExpression, dslParser } from './common/dsl'
import { FileFilterData, reprocess } from './common/processor';

//Parse the incoming data in to JSON and check for keys
// Do this safely, per https://stackoverflow.com/a/40363010/160205
function streamToPromise(stream: any) {
  return new Promise((resolve, reject) => {
    let chunks: Array<Uint8Array> = [];

    function onData(chunk: Uint8Array) {
        chunks.push(chunk);
    };

    function onEnd() {
        unbind();
        resolve(Buffer.concat(chunks).toString('utf8'));
    };

    function onError(error: any) {
        unbind();
        reject(error);
    };

    function unbind() {
        stream.removeListener('data', onData);
        stream.removeListener('end', onEnd);
        stream.removeListener('error', onError);
    }

    stream.on('data', onData);
    stream.on('end', onEnd);
    stream.on('error', onError);
  });
}

streamToPromise(input).then((inputData: any) => {
  let parsedData = JSON.parse(inputData as string);

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
});
