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

from plotly.offline import iplot
pgCache = {}


#Gets the error/parser index names in a more dowker-friendly format
def processGroupingCached(group, rowHashStr):
    if rowHashStr in pgCache:
        return pgCache[rowHashStr]
    else:
        name=""
        for index in range(len(group)):
            if (group[index]==1):
                name=name+str(index)+","

        name=name.rstrip(",")

        return name

#Gets the error/parser index names in a more dowker-friendly format
def processGrouping(group):
    name=""
    for index in range(len(group)):
        if (group[index]==1):
            name=name+str(index)+","

    name=name.rstrip(",")

    return name

def binaryRowHash( row ):
    return numpy.packbits(row.astype(numpy.bool_)).tobytes().hex()


def errorMatrixToDowker(error_matrix, filenames, html_out):
    start_time = time.time()

    #output_directory of error matrix and place to save all python dictionaries
    #output_directory="C:/Users/letitia.li/Documents/Safedocs/TA1/Eval/"

    #Load sparse error matrix and turn into reguar matrix
    #error_matrix_file=directory+"badMatrix.npz"
    #error_matrix = scipy.sparse.load_npz(errorMatrixFile)
    #error_matrix = error_matrix.todense()
    print(error_matrix.shape)


    #Create dictionary of unique_groupings: [list of files]
    groupingFilelistMap=dict()
    #Create dictionary of unique_groupings:weight
    groupingCountMap= dict()

    #Create dictionary for readability of grouping:"parser1, parser2" where we assume the first parser is "0"
    groupingToParsersMap = dict()

    #Create dictionary of nodeString: connected nodeString. For example, 2,3 connects to 3 and 2, 1,2,3 connects to 2,3; 1,3; and 1,2
    nodeEdgeMap = dict()

    #Create dictionary of node length: list of grouping strings
    lengthGroupingMap = dict()

    #There is no file 0, so the first file is index 1

    i=0
    #iterate over all columns representing each file's error vector
    for fileRow in error_matrix.A:
        if i==0:
            i+=1
            continue 
        rowHashStr = binaryRowHash(fileRow)
        
        pgRowStr = processGroupingCached(fileRow, rowHashStr)


       
        if rowHashStr in groupingFilelistMap:
            #groupingFilelistMap[rowHashStr].append(i)
            #if i<len(filenames):
            groupingFilelistMap[rowHashStr].append(filenames[i-1])
            groupingCountMap[rowHashStr]= groupingCountMap[rowHashStr]+1
        else:
            groupingFilelistMap[rowHashStr]=list()
            groupingCountMap[rowHashStr]=1
            groupingToParsersMap[rowHashStr] = pgRowStr
            #if i<len(filenames):
            groupingFilelistMap[rowHashStr].append(filenames[i-1])
           # groupingFilelistMap[rowHashStr].append(i)  
        i+=1
        #Create all edges for complex, where an edge exists between each set of nodes
        #with a difference of 1 error
        nodeEdgeMap[ pgRowStr ] = set()
        for index in range(len(fileRow)):
            if fileRow[index]==1:
                connNode = fileRow.tolist()
                connRow = fileRow.copy()
                connRow[index]=0
                # Since connNode is just fileRow as list
                # processGrouping(connNode) and processGrouping(fileRow) should be identical
                rowHashStr2 = binaryRowHash(connRow)
                connStr = processGroupingCached(connRow, rowHashStr2) 
            #  print("edge from " + str(fileRow) + " " +  processGrouping(fileRow) + " " + connStr)
                nodeEdgeMap[ pgRowStr ].add(connStr)

    #Build up length grouping map for node positioning
    for key in groupingToParsersMap:
        groupingStr = groupingToParsersMap[key]
        groupLen=len(groupingStr.split(","))+1
        if groupingStr=="":
            groupLen=0
        if groupLen in lengthGroupingMap:
            lengthGroupingMap[groupLen].append(groupingStr)
        else:
            lengthGroupingMap[groupLen]= list()
            lengthGroupingMap[groupLen].append(groupingStr)

    #position all nodes from shorter length of errors to longer, and sorted
    stringPosMap = dict()
    for length in lengthGroupingMap:
        lengthGroupingMap[length].sort()
        n = len(lengthGroupingMap[length])
        k=0
        for groupingStr in lengthGroupingMap[length]:
            tk= 2.0*math.pi*k/n
            r=math.sqrt(n)
            stringPosMap[groupingStr]=(r*math.cos(tk), r*math.sin(tk), length)
            k+=1
    
    

    labels=[]
    groupings=list()

    goodGroupingSet=set()
    badGroupingSet=set()
    weights=[]
    filelist=[]
    #badweights=[]
    group=[]
    Edges=[]
    sizes=[]
    Xn=[]
    Yn=[]
    Zn=[]



    groupingRows=list()
    
    for node in groupingCountMap:
        groupingStr = groupingToParsersMap[node]
        if groupingStr in stringPosMap: 
            groupingRows.append(node)
            
           
            groupings.append(groupingStr)
            goodGroupingSet.add(node)
            weights.append(groupingCountMap[node])
            filelist.append(str(groupingFilelistMap[node]))

            labels.append(groupingStr+"_"+ str(groupingCountMap[node]))
            
            sizes.append(10)
      
            group.append(math.log(int(groupingCountMap[node])))
            if stringPosMap[groupingStr][2]==0:
                Zn.append(stringPosMap[groupingStr][2])
            else:
                Zn.append(stringPosMap[groupingStr][2]-1)

            Yn.append(stringPosMap[groupingStr][1])
            Xn.append(stringPosMap[groupingStr][0])

    #print(nodeEdgeMap)
    parsersToGroupingMap = {v: k for k, v in groupingToParsersMap.items()}
    for node in groupingCountMap:
        groupingStr = groupingToParsersMap[node]
        if groupingStr in nodeEdgeMap:
                
            for connNode in nodeEdgeMap[groupingStr]:
                if connNode in groupings and groupingStr in groupings:
                    highCount= groupingCountMap[node]
                    if connNode in parsersToGroupingMap:
                        if parsersToGroupingMap[connNode] in groupingCountMap:
                            lowCount= groupingCountMap[parsersToGroupingMap[connNode]]
                            if highCount> lowCount:
                                Edges.append((groupings.index(groupingStr), groupings.index(connNode), 'red'))
                            else:
                                Edges.append((groupings.index(groupingStr), groupings.index(connNode), 'lightgreen'))

    L=len(Edges)
    N=len(labels)
    #Edges=[(data['links'][k]['source'], data['links'][k]['target']) for k in range(L)]

    #G=ig.Graph(Edges, directed=True)
    print(max(group))

    #change up nodes and edges
    ##Xn=[layt[k][0] for k in range(N)]# x-coordinates of nodes
    ##Yn=[layt[k][1] for k in range(N)]# y-coordinates
    ##Zn=[layt[k][2] for k in range(N)]# z-coordinates
    Xe=[]
    Ye=[]
    Ze=[]
    edgeColors=[]
    for e in Edges:
        Xe+=[Xn[e[0]],Xn[e[1]], None]# x-coordinates of edge ends
        Ye+=[Yn[e[0]],Yn[e[1]], None]
        Ze+=[Zn[e[0]],Zn[e[1]], None]
      
        edgeColors.append(e[2])
        edgeColors.append(e[2])
        edgeColors.append(e[2])
    print(len(Xe))
    print(len(edgeColors))
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
                   text= ['Grouping {}<br> File weight {} <br>Files {}'.format(groupings[i][:50],weights[i],filelist[i]) for i in range(0,N)],
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

 #   directory="C:/Users/letitia.li/Documents/"
    plotly.offline.plot(fig, filename='dowker_faw.html')
    with open("dowker_faw.html", 'r') as f1:
        with open(html_out, 'w') as f:
           # f.write("TEXT\n")
            #f.write(vars['file_names']);
            for line in f1:
                f.write(line)
#    with open("dowker_faw.html", 'r') as f1:
#    print("--- %s seconds ---" % (time.time() - start_time))

#errorMatrixToDowker("C:/Users/letitia.li/Documents/Safedocs/TA1/Eval/errorMatrix_internet.npz")

