// Settings; TODO make more of these editable (esp. alignment)
let height = 900;
const maxHeight = 10000; // arbitrary. Could probably go higher
const width = 720;
const nodeWidth = 15;
const padding = 10;
const align = 'left';
const defaultColor = "#dddddd";

window.onload = function () {
    const heightSelector = document.getElementById("height-selector")
    heightSelector.value = height;
    heightSelector.addEventListener(
        "input",
        function () {
            let newHeight = parseInt(heightSelector.value, 10);
            if (newHeight && newHeight > 0) {
                newHeight = Math.min(newHeight, maxHeight);
                heightSelector.value = newHeight;
                height = newHeight;
                renderSankeyDiagram();
            }
        }
    );
    renderSankeyDiagram();
}

let allLinks;
let allNodes;

function renderSankeyDiagram() {
    sankey = d3.sankey()
        .nodeId(d => d.name)
        .nodeAlign(d3[`sankey${align[0].toUpperCase()}${align.slice(1)}`])
        .nodeSort(null) // keep order as per CSV data!!
        .nodeWidth(nodeWidth)
        .nodePadding(padding)
        .extent([[0, 5], [width, height - 5]]);


    const links_ = d3.csvParseRows(
        sankeyCsvText,
        ([source, target, value, linkColor = defaultColor]) =>
            (
                source && target
                ? {
                    source,
                    target, 
                    value: !value || isNaN(value = +value) ? 1 : value,
                    color: linkColor
                }
                : null
            )
    );
    const nodeByName = new Map;
    for (const link of links_) {
    if (!nodeByName.has(link.source)) nodeByName.set(link.source, {name: link.source});
    if (!nodeByName.has(link.target)) nodeByName.set(link.target, {name: link.target});
    }
    const data = {nodes: Array.from(nodeByName.values()), links: links_};

    d3.select("body").selectAll("svg").remove();
    const svg = d3.select("body").append("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("preserveAspectRatio", "xMinYMin")
        .attr("viewBox", `0 0 ${width} ${height}`)
        .style("background", "#fff")
        .style("width", "100%")
        .style("height", "auto");

    const {nodes, links} = sankey({
    nodes: data.nodes.map(d => Object.assign({}, d)),
    links: data.links.map(d => Object.assign({}, d))
    });

    allNodes = svg.append("g")
        .selectAll("rect")
        .data(nodes)
        .join("rect")
        .attr("id", function (d, i) {
            d.id = i;
            return "node-" + d.id;
        })
        .attr("x", d => d.x0 + 1)
        .attr("y", d => d.y0)
        .attr("height", d => d.y1 - d.y0)
        .attr("width", d => d.x1 - d.x0 - 2)
        .attr("fill", d => {
            let c;
            for (const link of d.sourceLinks) {
                if (c === undefined) 
                    c = link.color;
                else if (c !== link.color) 
                    c = null;
            }
            if (c === undefined) { 
                for (const link of d.targetLinks) {
                    if (c === undefined) 
                        c = link.color;
                    else if (c !== link.color) 
                        c = null;
                }
            }
            return (d3.color(c) || d3.color(defaultColor)).darker(0.5);
        })
        .on("click", highlightLinks)
        .style("cursor", "pointer")

    allNodes.append("title")
        .text(d => `${d.name}\n${d.value.toLocaleString()}`);

    allLinks = svg.append("g")
        .attr("fill", "none")
        .style("pointer-events", "none")
        .selectAll("g")
        .data(links)
        .join("g")
        .attr("id", function (d, i) {
            d.id = i;
            return "link-" + d.id;
        })
        .attr("stroke", d => d3.color(d.color) || defaultColor)
        .style("mix-blend-mode", "multiply");

    allLinks.append("path")
        .attr("d", d3.sankeyLinkHorizontal())
        .attr("stroke-width", d => Math.max(1, d.width));

    allLinks.append("title")
        .text(d => `${d.source.name} → ${d.target.name}\n${d.value.toLocaleString()}`);

    svg.append("g")
        .style("font", "10px sans-serif")
        .style("text-shadow", "white 0 0 2px, white 0 0 1px")
        .selectAll("text")
        .data(nodes)
        .join("text")
        .attr("x", d => d.x0 < width / 2 ? d.x1 + 6 : d.x0 - 6)
        .attr("y", d => (d.y1 + d.y0) / 2)
        .attr("dy", "0.35em")
        .attr("text-anchor", d => d.x0 < width / 2 ? "start" : "end")
        .text(d => d.name)
        .append("tspan")
        .attr("fill-opacity", 0.7)
        .text(d => ` ${d.value.toLocaleString()}`);
}

let clickedNode = null;

function highlightLinks(event, node) {
    // Based on https://observablehq.com/@iashishsingh/sankey-diagram-path-highlighting
    if (node === clickedNode) {
        resetHighlight();
        clickedNode = null;
        return;
    }
    clickedNode = node;
    // fade everything out a bit
    allLinks.style("stroke-opacity", 0.4);
    allNodes.style("fill-opacity", 0.4);

    // Restore ancestors and descendents to normal opacity
    let currentLevel = [];
    let nextLevel;

    setNodeOpacity(node, 1.0);
    [
        {linkType: "sourceLinks", nodeType: "target"},
        {linkType: "targetLinks", nodeType: "source"}
    ].forEach(function ({linkType, nodeType}) {
        node[linkType].forEach(function (link) {
            currentLevel.push(link[nodeType]);
            setLinkOpacity(link, 1.0);
        });
        while (currentLevel.length) {
            nextLevel = [];
            currentLevel.forEach(function (node) {
                setNodeOpacity(node, 1.0);
                node[linkType].forEach(function (link) {
                    nextLevel.push(link[nodeType]);
                    setLinkOpacity(link, 1.0);
                });
            });
            currentLevel = nextLevel;
        }
    });
}

function setLinkOpacity(link, opacity) {
    d3.select("#link-" + link.id).style("stroke-opacity", opacity);
}

function setNodeOpacity(node, opacity) {
    d3.select("#node-" + node.id).style("fill-opacity", opacity);
}

function resetHighlight() {
    allLinks.style("stroke-opacity", 1.0);
    allNodes.style("fill-opacity", 1.0);
}
