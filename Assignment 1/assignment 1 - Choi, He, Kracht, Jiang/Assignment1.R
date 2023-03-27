#### Task 1: Describe and plot a social network
#### Group: Siyu Jiang, Roman Kracht, Zhengting He, Seokwon Choi

library(sna)
library(igraph)
library(ggraph)
library(rstudioapi)

# set the working directory
setwd(dirname(getActiveDocumentContext()$path)) 
getwd()

# (a) load the affective network and the gender data ----
affective_network_data <- read.csv("2400_affective_w1.csv", encoding="UTF-8", header=FALSE)
class(affective_network_data)
affective_network_data.friendship <- data.matrix(affective_network_data)[-1,-1]
gender_data <- read.csv('2400_sex.csv', encoding="UTF-8")

# (b) recode the affective network wave 1 into a friendship network ----
affective_network_data.friendship[affective_network_data.friendship == 1] <- 0
affective_network_data.friendship[affective_network_data.friendship == -1] <- 0
affective_network_data.friendship[affective_network_data.friendship == -2] <- 0
affective_network_data.friendship[is.na(affective_network_data.friendship)] <- 0
affective_network_data.friendship[affective_network_data.friendship == 2] <- 1
class(affective_network_data.friendship)
dim(affective_network_data.friendship)


# (c) basic network descriptives for this friendship network ----
affective_network.igraph <- graph_from_adjacency_matrix(affective_network_data.friendship,
                                                        mode = "directed")
affective_network.igraph

autograph(affective_network.igraph)

set.seed(56)
layout.graph <- create_layout(affective_network.igraph, layout = 'fr')

# network size (i.e., number of nodes)
count.nodes <- dim(affective_network_data.friendship)[1]
count.nodes

# number of network ties
count.ties <- sum(affective_network_data.friendship)
count.ties

# density
density.manual <- count.ties/(count.nodes*(count.nodes-1))
density.manual
gden(affective_network_data.friendship)

# average degree
sum(affective_network_data.friendship, na.rm = T)/count.nodes

# outdegree of each node
outdegree <- rowSums(affective_network_data.friendship) 
outdegree

# indegree of each node
indegree <- colSums(affective_network_data.friendship)
indegree

hist(indegree)
hist(outdegree)
psych::pairs.panels(data.frame(outdegree, indegree))

#reciprocity ratio
grecip(affective_network_data.friendship, measure = "dyadic.nonnull")

# gender composition in class 
sum(gender_data$sex == 1)
sum(gender_data$sex == 2)

#count of same gender ties (i.e., both nodes have the same gender)
gender_data$gender <- (gender_data$sex == 1)

temp <- matrix(kronecker(gender_data$sex, gender_data$sex, FUN="=="), nrow=count.nodes)

same_gender <- temp * affective_network_data.friendship

same_gender.igraph <- graph_from_adjacency_matrix(same_gender, mode = "directed")
same_gender.igraph

set.seed(56)
same_gender.graph <- create_layout(same_gender.igraph, layout = 'fr')

edges <- geom_edge_link2(alpha = .7,
                         arrow = arrow(length = unit(1, 'mm'), 
                                       type = "closed"),
                         end_cap = circle(1, 'mm'))
ggraph(same_gender.graph)+
  edges +
  geom_edge_link(alpha = 0.5)+
  geom_node_point(
    aes(colour = as.factor(gender_data$sex),
        size = gender_data$centrality))+ 
  theme_graph()+
  labs(title = "The same gender ties", node = "gender, 1=male, 2=female")

sum(same_gender)

# add a measure: degree centrality, betweenness centrality
gender_data$c.degree <- sna::degree(affective_network_data.friendship, cmode = 'freeman')
max(gender_data$c.degree)
min(gender_data$c.degree)
mean(gender_data$c.degree)

gender_data$c.betw <- igraph::betweenness(affective_network.igraph)
max(gender_data$c.betw)

# (d) plot the friendship network ----
edges <- geom_edge_link2(alpha = .7,
                         arrow = arrow(length = unit(1.2, 'mm'), 
                                       type = "closed"),
                         end_cap = circle(2, 'mm'))
ggraph(layout.graph)+
  edges +
  geom_edge_link()+
  geom_node_point(
    aes(colour = as.factor(gender_data$sex),
        size = gender_data$c.degree))+ 
  theme_graph() +
  labs(title = "The friendship network") +
  scale_color_discrete("Gender", labels = c("Male", "Female")) +
  scale_size_continuous("Degree centrality") +
  guides(color = guide_legend(order = 1), 
         size = guide_legend(order = 2))

# (e) plot trust network and friendship network together in one plot ----
trust_data <- read.csv("2400_trust_w1.csv", encoding="UTF-8", header=FALSE)
trust_data.matrix <- data.matrix(trust_data)[-1,-1]
mut_matrix <- affective_network_data.friendship
mut_matrix[trust_data.matrix == 1] <- 2
mut_matrix[trust_data.matrix == 1 & affective_network_data.friendship == 1] <- 3

mut.igraph <- graph_from_adjacency_matrix(mut_matrix, mode = "directed",
                                          weighted = TRUE)
mut.graph <- create_layout(mut.igraph, layout = 'fr')

edges <- geom_edge_link2(alpha = .7,
                         arrow = arrow(length = unit(1.2, 'mm'), 
                                       type = "closed"),
                         end_cap = circle(2, 'mm'))
ggraph(mut.graph)+
  edges +
  geom_edge_link(aes(colour = as.factor(weight))) +
  geom_node_point(aes(colour = as.factor(gender_data$sex),
                      size = gender_data$c.degree)) + 
  theme_graph() +
  labs(title = "The friendship and trust networks") +
  scale_color_discrete("Gender", labels = c("Male", "Female")) +
  scale_size_continuous("Degree centrality") +
  scale_edge_color_discrete("Tie type", labels = c("Friendship", "Trust", "Both")) +
  guides(color = guide_legend(order = 1), 
         size = guide_legend(order = 2))

# (f) how large is the overlap between both networks
sum(mut_matrix == 3)
