#! /usr/bin/env python3

# Identify a set of rows from a binary matrix that are likely misclassified
#
# Usage:
#  bernoulli_test <threshold> <file1.txt> <file2.txt>
#
# The program will dump a list of 0-based row indices from <file1.txt> to stdout

# Copyright (c) 2020 Michael Robinson
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
#
# This material is based upon work supported by the Defense Advanced Research Projects Agency (DARPA) SafeDocs program under contract HR001119C0072.
# Any opinions, findings and conclusions or recommendations expressed in this material are those of the author and do not necessarily reflect the views of DARPA.

import numpy as np
import argparse

def run_bernoulli_test(threshold,set1_relation_mat,set2_relation_mat):
    '''
    Run the Bernoulli test statistic on two matrices to identify columns of one matrix that ought to be columns in a second matrix because they fit its statistics better.

    Input: 
      threshold         : Percentage of columns to accept as being "within bounds", and therefore not misclassification
      set1_relation_mat : matrix of rows (variables) and columns (observations)
      set2_relation_mat : matrix of rows (variables) and columns (observations)
    Output:
       np.array of integer indices of columns of set1_relation_mat (observations) that better match the statistics of set2_relation_mat
       np.array of float[num interesting, num feature] relative importance of each feature for each interesting file. Try e.g.
            np.argpartition(vals, -4)[-4:] for the top 4 features.

    Note: Lower thresholds result in more columns being returned.
    '''

    # Compute row-wise probabilities for each column
    set1_row_prob=np.mean(set1_relation_mat,axis=1)
    s1p=np.expand_dims(set1_row_prob,1)

    if set2_relation_mat is not None:
        set2_row_prob=np.mean(set2_relation_mat,axis=1)
        s2p=np.expand_dims(set2_row_prob,1)
    else:
        s2p = s1p

    # Bernoulli likelihood functions for each file and under both correct and misclassified hypotheses
    set1_bern_likely_correct=np.log(1e-30 + s1p*set1_relation_mat+(1-s1p)*(1-set1_relation_mat))
    set1_bern_likely_incorrect=np.log(1e-30 + s2p*set1_relation_mat+(1-s2p)*(1-set1_relation_mat))

    # Compute likelihood ratio
    bern_lr=-set1_bern_likely_correct
    if set2_relation_mat is not None:
        bern_lr += set1_bern_likely_incorrect

    bern_detect = np.sum(bern_lr, axis=0)

    # Normalize the threshold based on percentage
    thres=np.sort(bern_detect)[int(threshold*len(bern_detect))]
    # Postprocessing/thresholding
    idx=np.where(np.logical_and(np.isfinite(bern_detect),bern_detect>thres))[0]
    return idx, bern_lr.T[idx]

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Identify rows in file1 that that probably should have been in file2.  Both files need to have the same number of entries in each row.')
    parser.add_argument('threshold',type=float,help='Percentile threshold for decision')
    parser.add_argument('file1',help='Test file, consists of whitespace-separated rows of "0" or "1"')
    parser.add_argument('file2',help='Training file, consists of whitespace-separated rows of "0" or "1"',
            nargs='?')

    args = parser.parse_args()
    
    # Ingest the input files
    set1_relation_mat=np.transpose(np.loadtxt(args.file1))
    if args.file2 is not None:
        set2_relation_mat=np.transpose(np.loadtxt(args.file2))
    else:
        set2_relation_mat = None

    # Run the test
    idx,fts=run_bernoulli_test(args.threshold,set1_relation_mat,set2_relation_mat)

    # Dump to stdout
    for i, i_fts in zip(idx, fts):
        print(i)
        topk = np.argpartition(i_fts, -3)[-3:]
        print(f'* {topk} / {i_fts[topk]}')
