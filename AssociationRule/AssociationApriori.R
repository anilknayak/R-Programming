# library(igraph)
# library(arules)
# library(arulesViz)
par(mfrow=c(1,2))
# network = read.graph("C:\\Users\\MyLife\\Dropbox\\R Programs\\graph.txt")
node_edge_data <- read.table("C:\\Users\\MyLife\\Dropbox\\R Programs\\graph.txt")

network <- graph.data.frame(node_edge_data, directed=TRUE)
plot(network,edge.arrow.size=0.5)
# print("edges")
# print(E(network)[ from(2) ])

g <- as.matrix(network, "edgelist")
print(g[1])

# apriori(as(network, "transactions"), parameter=list(support=0.01, confidence=0.5))

uniquenode = as.numeric(unique(V(network)))
print("unique nodes")
print(sort(uniquenode))
numberofuniquenode = length(uniquenode)+1
matrixofnodes = matrix(0, nrow = numberofuniquenode, ncol = numberofuniquenode)
print("all edges")



# rules = apriori(matrixofnodes,parameter=list(support=0.01, confidence=0.5))

# print(matrixofnodes)

#identify those vertices part of less than three edges
# vertices_with_support<-V(network)[degree(network)<2] 

# print(vertices_with_support)

#exclude them from the graph
# network_after_support<-delete.vertices(network, vertices_with_support)

# print(network_after_support)

#here the size of the vertices is specified by the degree of the vertex
# V(network)$size<-degree(network_after_support)/10

# plot(network_after_support)

# g=graph.adjacency(m,mode="undirected",weighted=NULL)
# rules = apriori(data = Adult, parameter=list(support=0.01, confidence=0.5))