#! /usr/bin/env python3

"""FAW plugin for Safedocs Hackathon 2022-11-[1-3].

This is a variation on the Dowker plot, where rather than a single edge jump,
we look at the minimum jump from the extant set of groups.

Using Michael Robinson's desire to build a Dowker tree based on complete feature
sets, rather than nebulous, unspecified features.
"""

import argparse
import json
import math
import numpy as np
import os
import pypugjs
import re
import sys

# For memory, we compress a lot to int8. But, sums need more
INT_TYPE = np.int32

def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('api_url')
    ap.add_argument('json_arguments')
    ap.add_argument('html_out')
    args = ap.parse_args()

    dec_args = json.loads(args.json_arguments)
    dec_args.setdefault('feature_regex', ': exit code (?!0)')

    ft_regex = re.compile(dec_args['feature_regex'], flags=re.I)

    file_names = []
    file_names_backward = {}
    ft_names = []
    ft_lookup = {}
    matrix = []

    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue

        obj = json.loads(line)

        file_names.append(obj.pop('_id'))
        file_names_backward[file_names[-1]] = len(file_names) - 1
        matrix.append({})
        for k, v in obj.items():
            if ft_regex.search(k) is None:
                continue
            ft_i = ft_lookup.get(k)
            if ft_i is None:
                ft_i = ft_lookup[k] = len(ft_names)
                ft_names.append(k)
            matrix[-1][ft_i] = 1.

    # Keep original ordering
    mat = np.zeros((len(matrix), len(ft_names)), dtype=np.int8)
    for m_i, m in enumerate(matrix):
        for k, v in m.items():
            assert v in (0, 1), v
            mat[m_i, k] = v

    ft_inverted = (mat.sum(0, dtype=INT_TYPE) > 0.5 * mat.shape[0])
    mat[:, ft_inverted] = 1 - mat[:, ft_inverted]

    nodes, edges = learn_dowker_family(mat)
    if False:
        # Nodes view
        visual_html = []
        visual_html.append('Nodes')
        for ni, n in enumerate(nodes):
            visual_html.append(f'{ni} -- {n}')
        visual_html.append('Edges')
        for ni, n in enumerate(edges):
            visual_html.append(f'{ni} -- {n}')
        visual_html = '<br/>'.join(visual_html)
    else:
        # Very janky HTML view
        def to_feats(row):
            r = []
            assert len(row.shape) == 1, row.shape
            for i in range(row.shape[0]):
                rf = row[i]
                if rf == 2:
                    continue
                if rf >= 3:
                    rf = rf - 3
                    pre = '(dup) '
                else:
                    pre = ''
                if ft_inverted[i]:
                    rf = 1 - rf
                r.append(f'{pre}{"NOT " if not rf else ""}{ft_names[i]}')
            return r
        vnodes = [[to_feats(n[0]), len(n[1]), int((n[0] != 2).sum(dtype=INT_TYPE))] for n in nodes]
        vedges = [[int(e[0]), int(e[1]), to_feats(e[2]), float(e[3])] for e in edges]
        visual_html = []
        visual_html.append(f'<div id="d3_vis">D3 did not load correctly</div>')
        visual_html.append(f'<script lang="javascript">')
        visual_html.append(f'const nodes = {json.dumps(vnodes)}; const edges = {json.dumps(vedges)};')
        visual_html.append(
r'''
(function() {
    // Heartily from https://observablehq.com/@d3/mobile-patent-suits

    const snode = nodes.map((x, xi) => ({id: xi, fts: x[0], count: x[1], fx: x[2] * 100}));
    const sedge = edges.map((x, xi) => ({id: `e${xi}`, source: x[0], target: x[1], fts: x[2], len: x[3]}));
    const sim = d3.forceSimulation(snode)
            .force('link', d3.forceLink(sedge))
            .force('charge', d3.forceManyBody().strength(-800))
            .force('x', d3.forceX())
            .force('y', d3.forceY())
            ;

    const width = 712, height = 512;
    const domBase = d3.select('#d3_vis');
    domBase.html(null);
    const svg = domBase.append('svg')
        .attr('width', '100%')
        .attr('height', height)
        .attr('viewBox', [0/2, -height/2, width, height])
        ;
    const tooltip = domBase.append('div');
    const root = svg.append('g');

    const zoom = d3.zoom()
        .on('zoom', (e) => root.attr('transform', e.transform))
        ;
    svg.call(zoom);

    // Build out graphics
    const vedge = root.append("g")
        .attr('fill', 'none')
        .selectAll("g")
        .data(sedge)
        .enter()
        .append("g")
        ;
    const vnode = root.append("g")
        .selectAll("g")
        .data(snode)
        .enter()
        .append("g")
            //.call(drag(sim))
        ;
    const mouseover = function(d) {
        tooltip.html(null);
        const data = d3.select(this).data()[0];
        const escape = v => v.replaceAll('&', '&amp;').replaceAll('<', '&lt;').replaceAll('>', '&gt;');
        tooltip.html('<ul><li>' + data.fts.map(escape).join('</li><li>') + '</li></ul>');
        /*
        const tt = d3.select(this)
            .append('g')
            .attr('class', 'tooltip')
            .attr('transform', 'translate(8, 16)')
            ;
        tt
            .append('text')
            .text(d3.select(this).data()[0].fts.join(' ;; '))
            ;
        */
    };
    vnode.on('mouseover', mouseover);
    vedge.on('mouseover', mouseover);

    vedge
        .append("path")
            .attr("stroke", d => d3.color('blue'))
            .attr("stroke-width", 3)
            .attr("pointer-events", "visibleStroke")
        ;
    vnode.append("circle")
        .attr("r", d => 4 + Math.log(Math.max(1, d.count)))
        ;
    vnode.append("text")
        .attr("x", 8)
        .text(d => `${d.fts.length} fts // ${d.count} files`)
        .clone(true).lower()
            .attr('fill', 'none')
            .attr('stroke', 'white')
            .attr('stroke-width', 3)
        ;

    function linkArc(d) {
      const r = Math.hypot(d.target.x - d.source.x, d.target.y - d.source.y);
      return `
        M${d.source.x},${d.source.y}
        A${r},${r} 0 0,1 ${d.target.x},${d.target.y}
      `;
    }
    sim.on('tick', () => {
        vedge.select('path').attr('d', linkArc);
        vnode.attr('transform', d =>`translate(${d.x}, ${d.y})`);
    });
})();
''')
        visual_html.append(f'</script>')
        visual_html = '\n'.join(visual_html)

    parser = pypugjs.parser.Parser(r'''
!!! 5
html
    head
        title Dowker tree
        script(src="https://d3js.org/d3.v7.min.js")
        script!= "let api_url = '" + api_url + "'; let old_args = JSON.parse('" + old_args_str.replace('\\', '\\\\').replace("'", "\\'") + "');"
        script.
            function callUrl(fn, args) {
                let req = new XMLHttpRequest();
                let url = api_url + fn;
                req.open('post', url, true);
                req.setRequestHeader('Content-Type', 'application/json');
                req.send(JSON.stringify(args));
            }

            function reprocess(new_args) {
                let args = Object.assign({}, old_args, new_args);
                callUrl('redecide', args);
            }
    body
        p Feature regex:
            input(id="featureField" type="text" value=old_args_obj['feature_regex'])
            input(type="button" value="Change" onclick="reprocess({feature_regex: document.getElementById('featureField').value})")
        div!= visual_html
            ''')
    block = parser.parse()
    compiler = pypugjs.ext.html.Compiler(block)
    with open(args.html_out, 'w') as f:
        compiler.global_context = {
                'api_url': args.api_url,
                'old_args_obj': dec_args,
                'old_args_str': json.dumps(dec_args),
                'visual_html': visual_html,
        }
        f.write(compiler.compile())


def learn_dowker_family(mat):
    """Given a files x features matrix, produce a dowker.

    Unlike above, learn it by an inclusive (rather than exact) metric.

    Note that `2` means a
    """
    nodes = []  # [(minimal matrix row, indices of files in set)]
    edges = []  # [(node src (less features), node dst (more features), labels row, node length)]

    # Seed nodes with a special case, to detect all root dialects.
    root_fts = np.empty_like(mat[0])
    root_fts.fill(2)
    nodes.append((root_fts, np.arange(mat.shape[0])))

    seen = np.zeros_like(mat[0], dtype=np.bool_)
    ft_counts = mat.sum(0, dtype=INT_TYPE)
    while seen.sum(dtype=INT_TYPE) != seen.shape[0] and len(nodes) < 50:
        # Thoughts: constrain search to only those that exist in the DB.
        # Keeps becoming an NP complete hyper graph. Heuristic?
        # Steps:
        # 1. From unseen features, pick one in most files (pre-inversion will
        #    equate to Bernoulli max)
        # 2. For all ~~leaf~~ nodes, compute information gained (entropy) of
        #    adding this split. Cannot use leaves because we want multiple
        #    splits off of a parent feature.
        #    We want to avoid adding new children to the root that are already
        #    covered by an existing child. Potentially information gain could
        #    be computed against all candidate siblings and complete set; take
        #    minimum.
        #    For any info gain above some threshold, add a new node + edge.
        # NOTE we encode duplicates as 3, 4 s.t. (value % 3) is 0 for NOT, 1 for
        # SET, and 2 for DONTCARE
        sel = ~seen
        row_ft_max = ft_counts[sel].argmax()
        ft_idx = np.arange(ft_counts.shape[0])[sel][row_ft_max]
        seen[ft_idx] = True

        node_to_ig = {}
        node_to_fileset = {}
        for ni, n in enumerate(nodes):
            # Compute info gain; store in node_to_ig
            nr = n[0].copy()
            nr[ft_idx] = 1
            nrs = (nr < 2).sum(dtype=INT_TYPE)
            nf = [f for f in n[1] if (nr == mat[f]).sum(dtype=INT_TYPE) == nrs]
            nnf = len(n[1])  # guaranteed non-zero
            p = len(nf) / nnf
            if min(p, 1 - p) < 1e-8:
                ig = 0.
            else:
                ig = -(p * math.log(p) + (1 - p) * math.log(1 - p))
            node_to_ig[ni] = (ig, p)
            node_to_fileset[ni] = nf

        for ni, n in enumerate(nodes[:]):
            # Look at all possible igs for this node
            # FIXME negation is important here, and unimplemented. Probably
            # want a true binary tree.
            # FIXME low information gains should expand extant labeled edges.
            # FIXME unoptimal
            n_edges = [e for e in edges if e[0] == ni]
            ig_nodes = [n] + [nodes[e[1]] for e in n_edges]
            ig_edges = [None] + n_edges
            ig_set = [node_to_ig[ni]] + [node_to_ig[e[1]] for e in n_edges]
            ig = min([i[0] for i in ig_set])

            if ig > 0.5:  # 0.5 ~= 20% threshold  TODO variable
                nr = n[0].copy()
                nr[ft_idx] = 1
                nf = node_to_fileset[ni]
                nodes.append((nr, nf))
                er = np.empty_like(nr)
                er.fill(2)
                er[ft_idx] = 1
                edges.append((ni, len(nodes) - 1, er, 1))

                # Add reverse
                nr = n[0].copy()
                nr[ft_idx] = 0
                nf = list(set(n[1]).difference(nf))
                nodes.append((nr, nf))
                er = np.empty_like(nr)
                er.fill(2)
                er[ft_idx] = 0
                edges.append((ni, len(nodes) - 1, er, -1))
            elif ig == 0.:
                # Add feature to the node with the least information gain
                for (n_ig, n_p), n, e in zip(ig_set, ig_nodes, ig_edges):
                    if n_ig != ig:
                        continue
                    fs = 1 if n_p > 0.5 else 0
                    # Weird hack for labels
                    fs += 3
                    n[0][ft_idx] = fs
                    if e is not None:
                        e[2][ft_idx] = fs

                ## FIXME should update up full tree, maybe... there's a
                # broken bit where node annotations are not updated

    return nodes, edges


if __name__ == '__main__':
    main()

