#**Code for generating network properties **# 

import csv
import networkx as nx
import matplotlib.pyplot as plt

#----------Create Graph-----------#
G = nx.Graph()

adj_list = open("adjacency.txt",'rb') #opens adjacency file
G = nx.read_adjlist(adj_list)

        

N=len(nx.nodes(G))
deg = []

for d in G.degree().values():
    deg.append(d)
avg_deg = sum(deg)/N
edge = nx.number_of_edges(G)
avg_cc = nx.average_clustering(G)
cor_coef = nx.degree_pearson_correlation_coefficient(G)

results = open("results.txt", 'w')
writer = csv.writer(results)
row = []
row.append(avg_cc)
#--------write in output file-------#
for row in results:
    writer.writerows(cc)
results.write("The average cc is %.2f" % avg_cc)
results.write("\nthe average deg is %.2f " % avg_deg)
results.close()
