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


g <- ggraph(datalarge.igraph, layout = 'fr') + # layout algorithm
  geom_edge_link(color = "gray89", aes(width = weight)) +
  geom_node_point(color = "plum4", size = 2) +
  geom_node_text(aes(label = name), color = "plum4", size = 2.5, vjust = -0.5) +
  theme_graph()  +
  labs(title = "The co-emergence network of characters in Star Wars 4-6 episodes") +
  scale_edge_width_continuous("The times of co-emergence")

g

#### Task 2a: community detection ####
# Edge-betweenness algorithm ---------------------------------------------------
eb <- cluster_edge_betweenness(datalarge.igraph)
length(eb) # the number of community: 2
sizes(eb) # community sizes
member.eb <- membership(eb)

modularity(eb)

g +
  geom_node_point(aes(color = factor(member.eb)), size = 2) +
  geom_node_text(aes(label = name, color = factor(member.eb)),
                 size = 2.5, vjust = -0.5) +
  labs(title = "The community detection in Star Wars 4-6: Edge-Betweenness") +
  scale_color_discrete("Communities") +
  guides(color = guide_legend(order = 1), 
         size = guide_legend(order = 2))
# plot(eb, datalarge.igraph,
#      vertex.size = 5,
#      vertex.label.font = 2,
#      vertex.label.cex = 0.6,
#      vertex.label.dist = 0.8)

plot_dendrogram(eb)

# walktrap algorithm -----------------------------------------------------------
wt <- cluster_walktrap(datalarge.igraph)
length(wt) # the number of community: 3
sizes(wt) # community sizes
member.wt <- membership(wt)

modularity(wt)

g +
  geom_node_point(aes(color = factor(member.wt)), size = 2) +
  geom_node_text(aes(label = name, color = factor(member.wt)),
                 size = 2.5, vjust = -0.5) +
  labs(title = "The community detection in Star Wars 4-6: Walktrap") +
  scale_color_discrete("Communities") +
  guides(color = guide_legend(order = 1), 
         size = guide_legend(order = 2))

# plot(wt, datalarge.igraph,
#      vertex.size = 5,
#      vertex.label.font = 2,
#      vertex.label.cex = 0.6,
#      vertex.label.dist = 0.8)

plot_dendrogram(wt)

# Louvain ----------------------------------------------------------------------
lv <- cluster_louvain(datalarge.igraph)
length(lv) # the number of community: 5
sizes(lv) # community sizes
member.lv <- membership(lv)

modularity(lv)

g +
  geom_node_point(aes(color = factor(member.lv)), size = 2) +
  geom_node_text(aes(label = name, color = factor(member.lv)),
                 size = 2.5, vjust = -0.5) +
  labs(title = "The community detection in Star Wars 4-6: Lauvain") +
  scale_color_discrete("Communities") +
  guides(color = guide_legend(order = 1), 
         size = guide_legend(order = 2))
# plot(lv, datalarge.igraph,
#      vertex.size = 5,
#      vertex.label.font = 2,
#      vertex.label.cex = 0.6,
#      vertex.label.dist = 0.8)

