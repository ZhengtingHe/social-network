#### Task 1: Describe and plot a social network
#### Group: Roman Kracht, Zhengting He, Seokwon Choi, Siyu Jiang

# install.packages("drat")
# drat::addRepo("schochastics")
# install.packages("networkdata")
rm(list = ls())
library(networkdata)

library(igraph)
library(ggforce)
library(ggraph)

# data(package = "networkdata") # to view the whole database

data.list <- starwars #igraph

# larger data set by merging the data of episodes 4-6
data.sublist <- data.list[c(4,5,6)]

# first convert the list of igrapahs to list of data.frames
subgraph_list_df <- lapply(data.sublist, as_data_frame)
# then combine all the data.frames in the list into one data.frame
subgraph_df <- do.call(rbind, subgraph_list_df)

# star wars: episode 4-6 in one igragh
datalarge.igraph <- graph_from_data_frame(subgraph_df , directed = FALSE)
# data5.igraph <- data.list[[5]] # star wars: episode 5

autograph(datalarge.igraph)

is.weighted(datalarge.igraph)# weight: number of the scenes where they appear together
is.directed(datalarge.igraph)

edge.attributes(datalarge.igraph)
as.data.frame(vertex_attr(datalarge.igraph))

set.seed(8)

#### Task 1a: visualization ####
# ggraph(data5.igraph,
#        layout = 'fr')+
#   geom_edge_link(color = "grey", aes(width = weight))+
#   geom_node_point()+
#   geom_node_text(aes(label = name), color = "red", size = 3)+
#   theme_graph()


ggraph(datalarge.igraph, #data
       layout = 'fr')+ # layout algorithm
  geom_edge_link(color = "grey", aes(width = weight))+ # edges as straight lines (rather than e.g. curves, or bended edges)
  geom_node_point()+
  geom_node_text(aes(label = name), color = "red", size = 3)+
  theme_graph()


#### Task 2a: community detection ####
# Edge-betweenness algorithm
eb <- cluster_edge_betweenness(datalarge.igraph)
modularity(eb)
membership(eb)
plot(eb, datalarge.igraph,
     vertex.size = 5,
     vertex.label.font = 2,
     vertex.label.cex = 0.6,
     vertex.label.dist = 0.8)

# walktrap algorithm
wt <- cluster_walktrap(datalarge.igraph)
modularity(wt)
membership(wt)
plot(wt, datalarge.igraph,
     vertex.size = 5,
     vertex.label.font = 2,
     vertex.label.cex = 0.6,
     vertex.label.dist = 0.8)

plot_dendrogram(wt)

# Louvain
lv <- cluster_louvain(datalarge.igraph)
modularity(lv)
membership(lv)
plot(lv, datalarge.igraph,
     vertex.size = 5,
     vertex.label.font = 2,
     vertex.label.cex = 0.6,
     vertex.label.dist = 0.8)

