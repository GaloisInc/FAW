#! /usr/bin/env python3

"""FAW plugin for Safedocs Hackathon 2022-11-[1-3].

This is a variation on the Dowker plot, where rather than a single edge jump,
we look at the minimum jump from the extant set of groups.

Using Michael Robinson's desire to build a Dowker tree based on complete feature
sets, rather than nebulous, unspecified features.
"""

import argparse
import json
import numpy as np
import os
import pypugjs
import re
import sys

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

    # Sparse to dense
    mat = np.zeros((len(matrix), len(ft_names)), dtype=np.bool_)
    ft_i_sorted = sorted(range(len(ft_names)), key=lambda m: ft_names[m].lower())
    ft_i_backward = [0 for _ in ft_i_sorted]  # goes from original to sorted
    for i_back, i in enumerate(ft_i_sorted):
        ft_i_backward[i] = i_back
    for m_i, m in enumerate(matrix):
        for k, v in m.items():
            assert v in (0, 1), v
            mat[m_i, ft_i_backward[k]] = v

    ft_inverted = (mat.sum(0) > 0.5 * mat.shape[0])
    mat[:, ft_inverted] = ~mat[:, ft_inverted]

    nodes, edges = learn_dowker(mat)
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
                if row[i]:
                    r.append(f'{"NOT " if ft_inverted[i] else ""}{ft_names[i]}')
            return r
        vnodes = [[to_feats(n[0]), float(n[1])] for n in nodes]
        vedges = [[int(e[0]), int(e[1]), float(e[2])] for e in edges]
        visual_html = []
        visual_html.append(f'<div id="d3_vis">D3 did not load correctly</div>')
        visual_html.append(f'<script lang="javascript">')
        visual_html.append(f'const nodes = {json.dumps(vnodes)}; const edges = {json.dumps(vedges)};')
        visual_html.append(
r'''
(function() {
    // Heartily from https://observablehq.com/@d3/mobile-patent-suits

    const snode = nodes.map((x, xi) => ({id: xi, fts: x[0], count: x[1], fx: x[0].length * 200}));
    const sedge = edges.map((x, xi) => ({id: `e${xi}`, source: x[0], target: x[1], len: x[2]}));
    const sim = d3.forceSimulation(snode)
            .force('link', d3.forceLink(sedge))
            .force('charge', d3.forceManyBody().strength(-4000))
            .force('x', d3.forceX())
            .force('y', d3.forceY())
            ;

    const width = 1024, height = 768;
    const domBase = d3.select('#d3_vis');
    domBase.html(null);
    const svg = domBase.append('svg')
        .attr('viewBox', [-width/2, -height/2, width, height])
        ;
    const root = svg.append('g');

    const zoom = d3.zoom()
        .on('zoom', (e) => root.attr('transform', e.transform))
        ;
    svg.call(zoom);

    // Build out graphics
    const vedge = root.append("g")
        .attr('fill', 'none')
        .selectAll("path")
        .data(sedge)
        .join("path")
            .attr("stroke", d => d3.color('blue'))
        ;
    const vnode = root.append("g")
        .selectAll("g")
        .data(snode)
        .enter()
        .append("g")
            //.call(drag(sim))
        .on('mouseover', function(d) {
            const tt = d3.select(this)
                .append('g')
                .attr('class', 'tooltip')
                .attr('transform', 'translate(8, 16)')
                ;
            tt
                .append('text')
                .text('tooltip')
                ;
        })
        .on('mouseout', function(d) {
            d3.select(this).select('g.tooltip').remove();
        })
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
        vedge.attr('d', linkArc);
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


def learn_dowker(mat):
    """Given a files x features matrix, produce a dowker.
    """
    nodes = []  # [(matrix row, indices of files in set)]
    edges = []  # [(node src (less features), node dst (more features), labels row, node length)]

    # Seed nodes with a special case, to detect all root dialects.
    nodes.append((np.zeros_like(mat[0]), []))

    # Convert to boolean -- no superposition here
    seen = np.zeros_like(mat[:, 0], dtype=np.bool_)
    linked = np.zeros_like(mat[:, 0], dtype=np.bool_)  # nodes, not files
    mat_row_ft = mat.sum(1)
    while seen.sum() != seen.shape[0]:
        # Steps:
        # 1. Find minimum features not yet seen
        # 2. Group, create new nodes
        # 3. Find minimum gap distances from extant nodes _that have not yet
        #    been linked_ to new nodes
        # 4. Create edges linking, record seen
        nodes_len_last = len(nodes)
        sel = ~seen
        row_ft_min = mat_row_ft[~seen].min()
        sel &= (mat_row_ft == row_ft_min)
        row_idx = np.arange(seen.shape[0])[sel]

        # 2.
        row_seen = []
        for ri in row_idx:
            rr = mat[ri]
            for rs in row_seen:
                if (rs == rr).all():
                    break
            else:
                sel = (mat == rr[None]).all(1)
                seen[sel] = True
                row_seen.append(rr)
                if True or sel.nonzero()[0].shape[0] > 10:
                    nodes.append((rr, sel.nonzero()[0].tolist()))

        if len(nodes) == nodes_len_last:
            # Nothing new added this round; rows still marked seen
            continue

        # 3.
        old_nodes_ids = [i for i in range(nodes_len_last) if not linked[i]]
        old_nodes = np.stack([nodes[i][0] for i in old_nodes_ids])
        new_nodes = np.stack([n[0] for n in nodes[nodes_len_last:]])
        #dists = np.matmul(old_nodes, new_nodes.T, dtype=np.int32)
        dists = (old_nodes[:, None] != new_nodes[None]).sum(-1)
        dists_min = dists.min(0)

        # 4.
        links = (dists == dists_min[None])
        for ii in range(links.shape[0]):
            i = old_nodes_ids[ii]
            for jj in range(links.shape[1]):
                j = nodes_len_last + jj
                if links[ii, jj]:
                    linked[i] = True
                    ef = np.empty_like(mat[0])
                    ef.fill(2)
                    edges.append((i, j, ef, dists[ii, jj]))

    # Convert for parity with non-exact format
    for n in nodes:
        v = n[0]
        v += 2 * (v == 0).astype(mat.dtype)

    return nodes, edges


if __name__ == '__main__':
    main()

