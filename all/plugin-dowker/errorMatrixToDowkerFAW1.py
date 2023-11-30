import csv
import json
import numpy
import networkx as nx
import matplotlib.pyplot as plt
from random import seed
import random 
import scipy
import time
import math

import plotly.graph_objs as go
import plotly
import igraph as ig
from plotly.offline import iplot
pg_cache = {}


#Change this if an upper bound on # of layers is desired
MAXLEN=1000



def create_string_pos_map(all_nodes, length_node_map, node_edge_map):
    """ Takes a list of nodes, a mapping of length: [nodes], and a mapping of node: [connected node]
    and returns a map of node: (xpos, ypos, zpos)

    Each of the following functions can be uncommented to select the desired layout
    """
    string_pos_map = dict()
   #Default layout: Original circular layout places nodes of the same length in a circle
    string_pos_map = original_circular_layer_layout(length_node_map)

    #Use 2D spring layout for each pair of layers, while separating layers by setting z coordinate to length
    # string_pos_map = layer2_spring_layout(length_node_map, node_edge_map)

    #Graph all nodes together with a KK (or fr) layout
    #  string_pos_map = full_force_directed(all_nodes, node_edge_map)

    #Graph all nodes together, but separate layers by length as z value
   # string_pos_map = full_force_directed_layers(all_nodes, node_edge_map)    
    return string_pos_map



def original_circular_layer_layout(length_node_map):
    """
    Default layout: Original circular layout places nodes of the same
    length (# of features) in a circle, ordered by list of feature name. The result is a stack of
    circles, positioned with the nodes with fewest features at the botoom
    """
    string_pos_map = dict()
    #Iterate through each feature length
    for length in length_node_map:
        length_node_map[length].sort()
        n = len(length_node_map[length])
        k=0
        for grouping_str in length_node_map[length]:
            #Angular position in radians 
            tk= 2.0*math.pi*k/n
            #Radius varies based on number of elements in this layer
            r=math.sqrt(n)
            string_pos_map[grouping_str]=(r*math.cos(tk), r*math.sin(tk), length)
            k+=1
    return string_pos_map
  
def layer2_spring_layout(length_node_map, node_edge_map):
    """ Use 2D-spring layout for each pair of layers with the same # of features,
    while separating layers by setting z coordinate to length
    """
    string_pos_map = dict()
    #Currently we can filter to only show a limited number of layers, defined
    #as number of features in a grouping
    max_len = max(list(length_node_map.keys()))
    max_len = min(max_len, MAXLEN)
 
    for length in range(0, max_len, 2):
        G = nx.Graph()
        #Add all groupings in layer1
        if length in length_node_map:
            for node in length_node_map[length]:
                G.add_node(node)
        #Add all groupings in layer2
        if (length+1) in length_node_map:
            for node in length_node_map[length+1]:
                G.add_node(node)
        #Add all edges to graph
        if length in length_node_map:   
            for node in length_node_map[length]:
                for conn_node in node_edge_map[node]:
                    G.add_edge(node, conn_node)
        #Try network x force-directed graph, returning all positions in pos
        pos = nx.spring_layout(G)
        #Fill out string pos map for each pair of layers with the x and y positions from pos, and z from length
        if length in length_node_map:
            for grouping_str in length_node_map[length]:
                string_pos_map[grouping_str]=(pos[grouping_str][0], pos[grouping_str][1], length+1)
        if (length+1) in length_node_map:
            for grouping_str in length_node_map[length+1]:
                string_pos_map[grouping_str]=(pos[grouping_str][0], pos[grouping_str][1], length+1)

    return string_pos_map    


def full_force_directed(all_nodes, node_edge_map):
    """    Graph all nodes together with a KK (or fr) layout """
    string_pos_map = dict()
    #Build a ig graph
    G = ig.Graph()
    #Add all nodes to ig graph
    for node in all_nodes:
        G.add_vertex(str(node))
    #Add edges to ig graph
    for node in node_edge_map:
        for conn_node in node_edge_map[node]:
            if conn_node in all_nodes and node in all_nodes:
                G.add_edge(str(node), str(conn_node))
    
    layt=G.layout('kk', dim=3)
    i=0
    for node in all_nodes:
        z = len(node.split(","))+1
        string_pos_map[node]=(layt[i][0], layt[i][1], layt[i][2])
        i+=1
  
    return string_pos_map


def full_force_directed_layers(all_nodes, node_edge_map):
    string_pos_map = dict()
    #Build a ig graph
    G = ig.Graph()
    #Add nodes to ig graph
    for node in all_nodes:
        G.add_vertex(str(node))
    #Add edges to ig graph
    for node in node_edge_map:
        for conn_node in node_edge_map[node]:
            if conn_node in all_nodes and node in all_nodes:
                G.add_edge(str(node), str(conn_node))
    #kk for Kamada-Kawai, fr for Fruchterman-Reingold
    layt=G.layout('fr', dim=3)
    i=0
    for node in all_nodes:
        z = len(node.split(","))+1
        string_pos_map[node]=(layt[i][0], layt[i][1], z)
        i+=1
  
    return string_pos_map

#Gets the error/parser index names in a more dowker-friendly format
def process_grouping_cached(group, row_hash_str):
    if row_hash_str in pg_cache:
        return pg_cache[row_hash_str]
    else:
        name=""
        for index in range(len(group)):
            if (group[index]==1):
                name=name+str(index)+","

        name=name.rstrip(",")

        return name


def process_grouping(group):
    """ Gets the error/parser index names in a more dowker-friendly format
    by converting a binary row in the error matrix to a list of features.
    For example, [0, 0, 1] would convert to "2", as features are 0-indexed
    """
    name=""
    for index in range(len(group)):
        if (group[index]==1):
            name=name+str(index)+","

    name=name.rstrip(",")

    return name

def binary_row_hash( row ):
    """A rapid conversion of an error matrix row [0, 1, 0 ...] into a unique string """
    return numpy.packbits(row.astype(numpy.bool_)).tobytes().hex()

def errorMatrixToDowker(error_matrix, filenames, html_out):
    """
    Called by main.py to generate the from an input error_matrix and filename list,
    then returns the dowker plot

    A "grouping" is defined as a unique set of features, and there is one grouping
    for each unique error row
    """

    #Create dictionary of { unique_groupings: [list of files] }
    grouping_filelist_map = dict()
    #Create dictionary of unique_groupings:weight
    grouping_count_map = dict()

    #Create dictionary for readability of grouping:"feature1,..."
    #which contains a full list of all features present in this grouping
    grouping_to_feature_list_map = dict()

    #Create dictionary of nodeString: connected nodeString. For example, 2,3 connects to 3 and 2, 1,2,3 connects to 2,3; 1,3; and 1,2
    node_edge_map = dict()

    #Create dictionary of node length: list of grouping strings
    length_grouping_map = dict()


    i=0
    #iterate over all columns representing each file's error vector, and complete the dictionaries declared above    
    for file_row in error_matrix:#.transpose():#.A:
        #Note, there is no file 0, so the first file has index 1
        if i==0:
            i+=1
            continue
        #For speed, we use a hash of each row [0,1,...] as the grouping in the above dicts, instead of a string of 0s and 1s
        row_hash_str = binary_row_hash(file_row)
        #pg_row_str is then the human-readable "feature1, ...featurex" string
        pg_row_str = process_grouping_cached(file_row, row_hash_str)
       
        if row_hash_str in grouping_filelist_map:
            #The filenames start at index 0 where error matrix starts at index 1,
            #so the corresponding 
            grouping_filelist_map[row_hash_str].append(filenames[i-1])
            #Increment # of files with this grouping
            grouping_count_map[row_hash_str]= grouping_count_map[row_hash_str]+1
        else:
            #Initialize dictionaries with this new key
            grouping_filelist_map[row_hash_str]=list()
            grouping_count_map[row_hash_str]=1
            grouping_to_feature_list_map[row_hash_str] = pg_row_str
            grouping_filelist_map[row_hash_str].append(filenames[i-1])
   
            #Create all edges for complex, where an edge exists between each set of nodes
            #with a difference of 1 error.
            node_edge_map[ pg_row_str ] = set()
            for index in range(len(file_row)):
                if file_row[index]==1:
                    #We find all adjacent nodes by iterating through the row in the
                    #error matrix and setting a single 1 to 0
                    conn_node = file_row.tolist()
                    conn_row = file_row.copy()
                    conn_row[index]=0
                    row_hash_str2 = binary_row_hash(conn_row)
                    conn_str = process_grouping_cached(conn_row, row_hash_str2) 
                    node_edge_map[ pg_row_str ].add(conn_str)
        i+=1
    #all_nodes will contain only nodes with feature length less than MAXLEN
    all_nodes = list()
    
    #Build up length grouping map for node positioning
    for key in grouping_to_feature_list_map:
        grouping_str = grouping_to_feature_list_map[key]
        group_len=len(grouping_str.split(","))+1
        
        if group_len<MAXLEN:
            all_nodes.append(grouping_str)
        else:
            print("SKIPPING")
            continue
        if grouping_str=="":
            group_len=0
        if group_len in length_grouping_map:
            length_grouping_map[group_len].append(grouping_str)
        else:
            length_grouping_map[group_len]= list()
            length_grouping_map[group_len].append(grouping_str)

    #position all nodes based on a selected algorithm
    string_pos_map = create_string_pos_map(all_nodes, length_grouping_map, node_edge_map) 

    #Start filling out fields for dowker display
    #Each of these lists should contain one value per node, in order
    #Node x, y, and z position respectively
    Xn=[]
    Yn=[]
    Zn=[]
    #Display label, such as feature list as string.
    labels=[]
    #For each node, # of files at that node as integer
    weights=[]
    #For each node, a list of all files as a string
    filelist=[]
    #For each node, the display size, in case we wish to make some nodes larger than others
    sizes=[]
    #Same as weight, but as separate list in case we wish to add different coloring scheme to nodes than log(weights)
    group=[]

    
    #A list of all strings used for index lookup when generating the edges
    groupings=list()

    Edges=[]
    #Iterate through nodes to fill out list
    for node in grouping_count_map:
        grouping_str = grouping_to_feature_list_map[node]
        #If there was an error and no position was entered for this node
        #or this node was filtered out, drop it
        if grouping_str in string_pos_map:
            #Fill in each list for dowker display
            groupings.append(grouping_str)
            weights.append(grouping_count_map[node])
            filelist.append(str(grouping_filelist_map[node]))
            #Current label is feature list_# of files at this node
            labels.append(grouping_str + "_" + str(grouping_count_map[node]))
            #A
            sizes.append(10)
            #To minimize 
            group.append(math.log(int(grouping_count_map[node])))
            #Append x,y,z locations to lists
            Zn.append(string_pos_map[grouping_str][2]-1)
            Yn.append(string_pos_map[grouping_str][1])
            Xn.append(string_pos_map[grouping_str][0])

    #Reverse dictionary to map list of feature strings to row_hash_str
    featurelist_to_grouping_map = {v: k for k, v in grouping_to_feature_list_map.items()}
    #For each pair of node, conn_node, draw an edge between them.
    for node in grouping_count_map:
        grouping_str = grouping_to_feature_list_map[node]
        if grouping_str in node_edge_map:
            for conn_node in node_edge_map[grouping_str]:
                if conn_node in groupings and grouping_str in groupings:
                    #Check if the lower level node (node with fewer features) has more files than the higher
                    #level node. If so, color the edge green. Otherwise, color the edge red (inconsistent)
                    high_count= grouping_count_map[node]
                    if conn_node in featurelist_to_grouping_map:
                        if featurelist_to_grouping_map[conn_node] in grouping_count_map:
                            low_count= grouping_count_map[featurelist_to_grouping_map[conn_node]]
                            #Create edge as a tuple(xcoordinate, ycoordinate, color)
                            if high_count> low_count:
                                Edges.append((groupings.index(grouping_str), groupings.index(conn_node), 'red'))
                            else:
                                Edges.append((groupings.index(grouping_str), groupings.index(conn_node), 'lightgreen'))

    L=len(Edges)
    N=len(labels)


    Xe=[]
    Ye=[]
    Ze=[]
    edgeColors=[]

    #Populate edges: 
    for e in Edges:
        Xe+=[Xn[e[0]],Xn[e[1]], None]# x-coordinates of edge ends
        Ye+=[Yn[e[0]],Yn[e[1]], None]
        Ze+=[Zn[e[0]],Zn[e[1]], None]
      
        edgeColors.append(e[2])
        edgeColors.append(e[2])
        edgeColors.append(e[2])
    

    #Plotly graph generation
    
    trace1=go.Scatter3d(x=Xe,
                   y=Ye,
                   z=Ze,
                   mode='lines',
                   line=dict(color=edgeColors, width=1),
                   hoverinfo='none'
                   )

    trace2=go.Scatter3d(x=Xn,
                   y=Yn,
                   z=Zn,
                   mode='markers',
                   name='',
                   marker=dict(symbol='circle',
                                 size=sizes,
                            cmax=3,
                                cmin=0,
                                 color=group,
                                 line=dict(color='rgb(50,50,50)', width=0.5)
                                 ),
                   text= ['Grouping {}<br> File weight {} <br>Files {}'.format(groupings[i][:50],weights[i],filelist[i][:50]) for i in range(0,N)],
                    hovertemplate =
        '<i>%{text}'+
        '<br>Length: %{z}<br>',
                   hoverinfo='text',

                   )

    axis=dict(showbackground=False,
              showline=False,
              zeroline=False,
              showgrid=False,
              showticklabels=False,
              title=''
              )

    layout = go.Layout(
             title="Dowker complex",
             width=1000,
             height=1000,
             showlegend=False,
             scene=dict(
                 xaxis=dict(axis),
                 yaxis=dict(axis),
                 zaxis=dict(axis),
            ),
         margin=dict(
            t=100
        ),
        hovermode='closest',
       )
    data=[trace1, trace2]
    fig=go.Figure(data=data, layout=layout)


    return plotly.offline.plot(fig, output_type='div')
