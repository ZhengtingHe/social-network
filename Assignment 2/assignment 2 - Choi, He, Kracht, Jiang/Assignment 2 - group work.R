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

# autograph(datalarge.igraph)

is.weighted(datalarge.igraph)# weight: number of the scenes where they appear together
is.directed(datalarge.igraph)

datalarge.igraph

#density
density.manual <- 170 / ((39*(39-1))/2)
density.manual

edge.attributes(datalarge.igraph)
vertex_attr(datalarge.igraph)

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
  geom_node_text(aes(label = name), color = "plum4", size = 3.5, vjust = -0.5) +
  theme_graph()  +
  labs(title = "The co-appearance network of characters in Star Wars IV-VI episodes") +
  scale_edge_width_continuous("The times of co-appearance")

g

#### Task 2a: community detection ####
### Just episode V -------------------------------------------------------------
data5.igraph <- data.list[[5]]

set.seed(13)

g5 <- ggraph(data5.igraph, layout = 'fr') + # layout algorithm
  geom_edge_link(color = "gray89", aes(width = weight)) +
  geom_node_point(color = "blue4", size = 2) +
  geom_node_text(aes(label = name), color = "blue4", size = 3.5, vjust = -0.5) +
  theme_graph()  +
  labs(title = "The co-appearance network of characters in Star Wars V") +
  scale_edge_width_continuous("The times of co-appearance")
g5

eb5 <- cluster_edge_betweenness(data5.igraph)
length(eb5) # the number of community: 4
sizes(eb5) # community sizes
member.eb5 <- membership(eb5)

modularity(eb5)

g5 +
  geom_node_point(aes(color = factor(member.eb5)), size = 2) +
  geom_node_text(aes(label = name, color = factor(member.eb5)),
                 size = 3.5, vjust = -0.5) +
  labs(title = "The community detection in Star Wars V by Edge-Betweenness") +
  scale_color_discrete("Communities") +
  guides(color = guide_legend(order = 1), 
         size = guide_legend(order = 2))

plot_dendrogram(eb5)


# Edge-betweenness algorithm ---------------------------------------------------
## episode 4-6
# eb <- cluster_edge_betweenness(datalarge.igraph)
# length(eb) # the number of community: 2
# sizes(eb) # community sizes
# member.eb <- membership(eb)
# 
# modularity(eb)
# 
# g +
#   geom_node_point(aes(color = factor(member.eb)), size = 2) +
#   geom_node_text(aes(label = name, color = factor(member.eb)),
#                  size = 3.5, vjust = -0.5) +
#   labs(title = "The community detection in Star Wars IV-VI by Edge-Betweenness") +
#   scale_color_discrete("Communities") +
#   guides(color = guide_legend(order = 1), 
#          size = guide_legend(order = 2))

# plot(eb, datalarge.igraph,
#      vertex.size = 5,
#      vertex.label.font = 2,
#      vertex.label.cex = 0.6,
#      vertex.label.dist = 0.8)

# plot_dendrogram(eb)

# walktrap algorithm -----------------------------------------------------------
# wt <- cluster_walktrap(datalarge.igraph)
# length(wt) # the number of community: 3
# sizes(wt) # community sizes
# member.wt <- membership(wt)
# 
# modularity(wt)
# 
# g +
#   geom_node_point(aes(color = factor(member.wt)), size = 2) +
#   geom_node_text(aes(label = name, color = factor(member.wt)),
#                  size = 3.5, vjust = -0.5) +
#   labs(title = "The community detection in Star Wars IV-VI by Walktrap") +
#   scale_color_discrete("Communities") +
#   guides(color = guide_legend(order = 1), 
#          size = guide_legend(order = 2))

# plot(wt, datalarge.igraph,
#      vertex.size = 5,
#      vertex.label.font = 2,
#      vertex.label.cex = 0.6,
#      vertex.label.dist = 0.8)

# plot_dendrogram(wt)

# Louvain ----------------------------------------------------------------------
# lv <- cluster_louvain(datalarge.igraph)
# length(lv) # the number of community: 5
# sizes(lv) # community sizes
# member.lv <- membership(lv)
# 
# modularity(lv)
# 
# g +
#   geom_node_point(aes(color = factor(member.lv)), size = 2) +
#   geom_node_text(aes(label = name, color = factor(member.lv)),
#                  size = 3.5, vjust = -0.5) +
#   labs(title = "The community detection in Star Wars IV-VI by Lauvain") +
#   scale_color_discrete("Communities") +
#   guides(color = guide_legend(order = 1), 
#          size = guide_legend(order = 2))

# plot(lv, datalarge.igraph,
#      vertex.size = 5,
#      vertex.label.font = 2,
#      vertex.label.cex = 0.6,
#      vertex.label.dist = 0.8)

#### Task 2b: conditional uniform graph tests ####
### Conditional uniform graph test---------------------------------------------
# Create the original graph using the provided dataset
datalarge.igraph <- graph_from_data_frame(subgraph_df , directed = FALSE)

# Calculate local clustering coefficient and average path length of the original graph
g_transitivity <- transitivity(datalarge.igraph, type = "local")
g_mean_path_length <- mean_distance(datalarge.igraph)

# Generate a random graph using the Erdős-Rényi model, where p is the edge generation probability
p <- 2 * edge_density(datalarge.igraph)
random_g <- erdos.renyi.game(n = vcount(datalarge.igraph), p.or.m = p, type = "gnp", directed = FALSE)

# Calculate local clustering coefficient and average path length of the random graph
random_g_transitivity <- transitivity(random_g, type = "local")
random_g_mean_path_length <- mean_distance(random_g)

# Compare the average values of local clustering coefficients
mean_g_transitivity <- mean(g_transitivity, na.rm = TRUE)
mean_random_g_transitivity <- mean(random_g_transitivity, na.rm = TRUE)

# Calculate modularity of the original graph
g_community <- cluster_louvain(datalarge.igraph)
g_modularity <- modularity(g_community)


cat("Mean local transitivity of original graph:", mean_g_transitivity, "\n")
cat("Mean local transitivity of random graph:", mean_random_g_transitivity, "\n")

# Compare the average path lengths
cat("Mean path length of original graph:", g_mean_path_length, "\n")
cat("Mean path length of random graph:", random_g_mean_path_length, "\n")

# Repeatedly generate random graphs and calculate the differences in statistics
n_iterations <- 50000
mean_trans_diff <- numeric(n_iterations)
mean_path_diff <- numeric(n_iterations)
modularity_random <- numeric(n_iterations)

for (i in 1:n_iterations) {
  random_g <- erdos.renyi.game(n = vcount(datalarge.igraph), p.or.m = p, type = "gnp", directed = FALSE)
  
  random_g_transitivity <- transitivity(random_g, type = "local")
  mean_random_g_transitivity <- mean(random_g_transitivity, na.rm = TRUE)
  mean_trans_diff[i] <- mean_g_transitivity - mean_random_g_transitivity
  
  random_g_mean_path_length <- mean_distance(random_g)
  mean_path_diff[i] <- g_mean_path_length - random_g_mean_path_length
  
  random_g_community <- cluster_louvain(random_g)
  modularity_random[i] <- modularity(random_g_community)
}

# Calculate the p-value for local clustering coefficient
p_value_trans <- sum(mean_trans_diff >= mean_g_transitivity) / n_iterations
cat("p-value for local transitivity:", p_value_trans, "\n")

# Calculate the p-value for average path length
p_value_path <- sum(mean_path_diff >= g_mean_path_length) / n_iterations
cat("p-value for mean path length:", p_value_path, "\n")

# Calculate the p-value for modularity
p_value_modularity <- sum(modularity_random >= g_modularity) / n_iterations
cat("p-value for modularity:", p_value_modularity, "\n")

### Plot hist
library(ggplot2)

# Local clustering coefficient histogram
hist_trans_data <- data.frame(Local_Clustering_Coefficient_Difference = mean_trans_diff)
ggplot(hist_trans_data, aes(x = Local_Clustering_Coefficient_Difference)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean_random_g_transitivity),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Local Clustering Coefficient Differences",
       x = "Difference in Local Clustering Coefficient",
       y = "Frequency") +
  theme_minimal()

# Average path length histogram
hist_path_data <- data.frame(Average_Path_Length_Difference = mean_path_diff)
ggplot(hist_path_data, aes(x = Average_Path_Length_Difference)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = g_mean_path_length),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Average Path Length Differences",
       x = "Difference in Average Path Length",
       y = "Frequency") +
  theme_minimal()

# Modularity histogram
hist_modularity_data <- data.frame(Modularity_Difference = modularity_random)
ggplot(hist_modularity_data, aes(x = Modularity_Difference)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = g_modularity),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Modularity in Random Networks",
       x = "Modularity",
       y = "Frequency") +
  theme_minimal()
