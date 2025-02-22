import networkx as nx

#----------Reading adj matrix--------------------------------
#---------------input file------------------------------
f1=open('adj_matrix_1.0.dat','r')
#---------------output file-------------------------------
f2=open('shrt_path.dat','w')
#---------------output format----------------------------

#------------------creating graph-----------------------------
G=nx.Graph()
i=0
j=0
N=0
edges=[]
for line in f1:
    N+=1
    j=0
    for word in line.split():
        if (int(word)==1):
            edges.append([i,j])
        j+=1
    i+=1
G.add_edges_from(edges)

#---------------------------calculating shortest path of all pairs--------
path=nx.all_pairs_shortest_path(G)

#-----------writings in file---------------------------------
for s1 in range(N):
    for s2 in range(s1,N):
        if (s1!=s2):
            f2.write(str(s1)+'\t'+str(s2)+'\t'+str(len(path[s1][s2])-1)+'\t'+str(path[s1][s2])+'\n')
            
        
f1.close()
f2.close()
