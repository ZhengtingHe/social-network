# install.packages("drat")
# drat::addRepo("schochastics")
# install.packages("networkdata")
rm(list = ls())
library(networkdata)

library(igraph)
library(ggforce)
library(ggraph)

data(package = "networkdata")

data.list <- starwars #igraph
data.igraph <- data.list[[5]]
autograph(data.igraph)

is.weighted(data.igraph)# number of the scenes where they appear together
is.directed(data.igraph)

edge.attributes(data.igraph)
vertex_attr(data.igraph)

set.seed(8)

#### Task 1a: visualization ####
ggraph(data.igraph,
       layout = 'fr')+
  geom_edge_link(color = "grey", aes(width = weight))+
  geom_node_point()+
  geom_node_text(aes(label = name), color = "red", size = 3)+
  theme_graph()

# larger data set by merging the data of episodes 4-6
data.sublist <- data.list[c(4,5,6)]
#first convert the list of igrapahs to list of data.frames
subgraph_list_df <- lapply(data.sublist, as_data_frame)
# then combine all the data.frames in the list into one data.frame
subgraph_df <- do.call(rbind, subgraph_list_df)
datalarge.igraph <- graph_from_data_frame(subgraph_df , directed = FALSE)

ggraph(datalarge.igraph, #data
       layout = 'fr')+ # layout algorithm
  geom_edge_link(color = "grey", aes(width = weight))+ # edges as straight lines (rather than e.g. curves, or bended edges)
  geom_node_point()+
  geom_node_text(aes(label = name), color = "red", size = 3)+
  theme_graph()


#### Task 2a: community detection ####
fg <- cluster_walktrap(datalarge.igraph)
modularity(fg)
membership(fg)
plot(fg, datalarge.igraph)

plot_dendrogram(fg)
