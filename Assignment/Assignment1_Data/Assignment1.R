library(sna)
library(igraph)
library(ggraph)
library(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path)) 
getwd()
affective_network_data = read.csv("2400_affective_w1.csv", encoding="UTF-8", header=FALSE)
class(affective_network_data)
affective_network_data.friendship = data.matrix(affective_network_data)[-1,-1]

affective_network_data.friendship[affective_network_data.friendship == 1] <- 0
affective_network_data.friendship[affective_network_data.friendship == -1] <- 0
affective_network_data.friendship[affective_network_data.friendship == -2] <- 0
affective_network_data.friendship[is.na(affective_network_data.friendship)] <- 0
affective_network_data.friendship[affective_network_data.friendship == 2] <- 1
class(affective_network_data.friendship)
dim(affective_network_data.friendship)


gender_data = read.csv('2400_sex.csv', encoding="UTF-8")
print(gender_data)

affective_network.igraph = graph_from_adjacency_matrix(affective_network_data.friendship,
                                             mode = "directed"
)
affective_network.igraph

autograph(affective_network.igraph)

set.seed(56)
layout.graph <- create_layout(affective_network.igraph, 
                              layout = 'fr')


count.ties <- sum(affective_network_data.friendship)
print(count.ties)

#network size (i.e., number of nodes), 
dim(affective_network_data.friendship)
count.ties <- sum(affective_network_data.friendship)
print(count.ties)
#density, 
density.manual <- count.ties / (nrow(affective_network_data.friendship)*(nrow(affective_network_data.friendship)-1))
gden(affective_network_data.friendship)
#average degree,
sum(affective_network_data.friendship, na.rm = T)/dim(affective_network_data.friendship)[1]

outdegree <- rowSums(affective_network_data.friendship) 
print(outdegree)

indegree <- colSums(affective_network_data.friendship)
print(indegree)
plot(outdegree)
plot(indegree)
hist(indegree)
hist(outdegree)
psych::pairs.panels(data.frame(outdegree, indegree))
?pairs.panels
#reciprocity ratio, 
grecip(affective_network_data.friendship, 
       measure = "dyadic.nonnull")
#centrality
gender_data$centrality <- sna::degree(affective_network_data.friendship, cmode = 'freeman')
gender_data$c.betw <- igraph::betweenness(affective_network.igraph)
#gender composition in class, 
sum(gender_data$sex == 1)
sum(gender_data$sex == 2)
#count of same gender ties (i.e., both nodes have the same gender)
gender_data$gender = (gender_data$sex == 1)
node_num = nrow(affective_network_data.friendship)

temp <- matrix(kronecker(gender_data$sex, gender_data$sex, FUN="=="), nrow=node_num)

same_gender = temp * affective_network_data.friendship

same_gender.igraph = graph_from_adjacency_matrix(same_gender,
                                                       mode = "directed"
)
same_gender.igraph

set.seed(56)
same_gender.graph <- create_layout(same_gender.igraph, 
                              layout = 'fr')

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
  theme_graph() 
sum(same_gender)
###################
edges <- geom_edge_link2(alpha = .7,
                         arrow = arrow(length = unit(1, 'mm'), 
                                       type = "closed"),
                         end_cap = circle(1, 'mm'))
ggraph(layout.graph)+
  edges +
  geom_edge_link(alpha = 0.5, aes(colour=))+
  geom_node_point(
    aes(colour = as.factor(gender_data$sex),
        size = gender_data$centrality))+ 
  theme_graph() 

trust_data = read.csv("2400_trust_w1.csv", encoding="UTF-8", header=FALSE)
trust_data.matrix = data.matrix(trust_data)[-1,-1]
mut_matrix = affective_network_data.friendship
mut_matrix[trust_data.matrix == 1] <- 2
mut_matrix[trust_data.matrix == 1 & affective_network_data.friendship == 1] <- 3

mut.igraph = graph_from_adjacency_matrix(mut_matrix,
                            mode = "directed",
                            weighted = TRUE
)
mut.graph <- create_layout(mut.igraph, 
                                   layout = 'fr')

edges <- geom_edge_link2(alpha = .7,
                         arrow = arrow(length = unit(1, 'mm'), 
                                       type = "closed"),
                         end_cap = circle(1, 'mm'))
ggraph(mut.graph)+
  edges +
  geom_edge_link(aes(colour=as.factor(weight)))+
  geom_node_point(
    aes(colour = as.factor(gender_data$sex),
        size = gender_data$centrality))+ 
  theme_graph() 
?graph_from_adjacency_matrix
?geom_edge_link2
