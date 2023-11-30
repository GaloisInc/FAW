# FAW Dowker Generation 

The Dowker visualization script for FAW generates a HTML file to be displayed in the FAW window. 

The Dowker complex appears as a 3d graph of nodes, where each node represents a set of files with the same 'features', where a feature is a binary value, such as indicating presence or absence of a certain error message. Each node has a link to adjacent nodes, where adjacent nodes are defined as a node with a single additional feature. 

This generation allows user customization in the form of coloring, sizing, filtering, and layout of nodes.

# Additional Files

A FAW Plugin by default includes main.py and requirements.txt. Dowker generation also requires additional files in the same folder:
- keys.txt: A listing of all names with binary error regexes or features.
- dowker_faw.html: The html file containing the dowker graph rendered in plotly, which is then parsed and passed as the display for the FAW.
- errorMatrixToDowker.py: Actual dowker generation, which takes as input a numpy matrix and generates dowker_faw.html

# FAW Data Processing

Raw FAW data must be converted into an error matrix as input into errorMatrixDowker. The matrix has one row per file and one column per feature (defined by keys.txt).

As the FAW data is streamed in with sys.stdin, the main wrapper (main.py) looks for lines of mode 'files', which appear as a json. The json for each file contains the filename, and feature:value pairs. Each file corresponds to a single line in the error matrix, and its values are entered into the column corresponding to the feature.

# FAW Display

Plotly generates a 3d network graph given a set of node and edge location coordinates, developed with https://plotly.com/python/v3/3d-network-graph/ as a guide. 

Additional customization can include coloring, sizing, and hover-text display labels on each node.

All of these properties are passed in as a list of attributes, 1 per node. For example, there is one list for node x position, node y position, node z position, color (as an integer which is colored based on relative position in the color scale), etc.

In addition, each edge has a position, and color, either green if it is a consistent edge (we expect that nodes with fewer features have more associated files than adjacent nodes with more features), or red if it is inconsistent


## Error Matrix Processing

There will be 1 node per unique row in the error matrix. We iterate through and generate maps of node:display name, node: count (# of files with that attribute), etc

Each node's attributes are then entered into the lists mentioned above.

## Display

Display involves first creating 2 traces with Scatter3d, one to create node and the other to create edges. Then, graph appearance is created with axis, title, etc. The traces and graph are passed to create the final plotly graph with
```
  fig=go.Figure(data=data, layout=layout)
```

Plotly graphs can be displayed interactively, or in our case, offline with plotly.offline.plot(fig, file loc).

## FAW output

Finally, the generated html file containing the plotly graph is copied into "html_out", which is then passed back to main to be rendered in the plugin display.
