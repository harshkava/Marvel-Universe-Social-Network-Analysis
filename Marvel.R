library(readr)     # fast reading of csv files
library(tidyverse) # tidy data analysis
library(tidygraph) # tidy graph analysis
library(ggraph)    # for plotting
library(dplyr)
library(ggplot2)
library(igraph)


#Importing all csv Files from a Folder
marvel <- read_csv("D:/Courses/02_Socal network Analytics/Harsh Study Material/Final Project/marvel.csv")
marvel

marvel_nodes <- read_csv("D:/Courses/02_Socal network Analytics/Harsh Study Material/Final Project/marvel_nodes.csv")
marvel_nodes

marvel$Source <- marvel_nodes$Label[match(marvel$Source, marvel_nodes$Id)]

marvel$Target <- marvel_nodes$Label[match(marvel$Target, marvel_nodes$Id)]
marvel

marvel <- marvel[c("Source","Target","Type","Id","Weight")]
marvel

marvel_ch <- marvel %>%   
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(Weight)) %>%
  ungroup()

marvel_ch

plot.igraph(graph.data.frame(marvel))


marvel_ch_l <- marvel_ch %>%arrange(desc(sum_weight)) %>%top_n(50, sum_weight)
marvel_ch_l

plot.igraph(graph.data.frame(marvel_ch_l,directed = FALSE))
# creating conversation table between top 100 characters
marvel_conversation <- marvel %>%filter(Source %in% marvel_ch_l$name & Target %in% marvel_ch_l$name)
marvel_conversation

gm <- graph.data.frame(marvel_conversation,directed = FALSE)

plot.igraph(gm)


#The first step is to convert our edge table into a tbl_graph object structure. 
#Here, we use the as_tbl_graph() function from tidygraph; it can take many different types of input data, like data.frame, matrix, dendrogram, igraph, etc.

#creating table for graph manipulation
as_tbl_graph(marvel_conversation, directed = FALSE)

#Using the activate() function that allows us to remove multiple edges.
as_tbl_graph(marvel_conversation, directed = FALSE) %>% activate(edges) %>% filter(!edge_is_multiple())


#Node ranking
#Often, especially when visualising networks with certain layouts, the order in which the nodes appear will have a huge influence on the insight you can get out (e.g. matrix plots and arc diagrams).

?node_rank
#Minimize hamiltonian path length using a travelling salesperson solver.
#or we can use any other seriation algorithm

as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(n_rank_trv = node_rank_traveller()) %>%
  arrange(n_rank_trv)

########################################################
#Centrality        
########################################################

# ?centrality 
#Centrality describes the number of edges that are in- or outgoing to/from nodes.
#High centrality networks have few nodes with many connections, low centrality networks have many nodes with similar numbers of edges.
#The centrality of a node measures the importance of it in the network.

#using Betweeness Centrality
as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(neighbors = centrality_betweenness()) %>%
  arrange(-neighbors)

#using Degree Centrality
as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(neighbors = centrality_degree()) %>%
  arrange(-neighbors)

########################################################
#Grouping and clustering
########################################################


#Group nodes by minimizing description length using.
#We can use ?group_graph for an overview about all possible ways to cluster and group nodes. 

as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(group = group_infomap()) %>%
  arrange(-group)

########################################################
#Querying node types
########################################################

#I am trying out node_is_center() (does the node have the minimal eccentricity in the graph) 
#and node_is_keyplayer() to identify the top 10 key-players in the network.

#install.packages('influenceR')
as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(center = node_is_center(),
         keyplayer = node_is_keyplayer(k = 10))

########################################################
#Node pairs
########################################################

as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(nodes) %>% 
  mutate(dist_to_center = node_distance_to(node_is_center()))

########################################################
#Edge betweenness
########################################################


#Similarly to node metrics, we can calculate all kinds of edge metrics.
#Betweenness, for example, describes the shortest paths between nodes.
#More about what you can do with edges can be found with ?edge_types and in the tidygraph manual.

as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  activate(edges) %>% 
  mutate(centrality_e = centrality_edge_betweenness())

########################################################
#Combining everything in true tidy verse fashion:
########################################################

marvel_conversation_graph <- as_tbl_graph(marvel_conversation, directed = FALSE) %>%
  mutate(n_rank_trv = node_rank_traveller(),
         neighbors = centrality_degree(),
         group = group_infomap(),
         center = node_is_center(),
         dist_to_center = node_distance_to(node_is_center()),
         keyplayer = node_is_keyplayer(k = 10)) %>%
  activate(edges) %>% 
  filter(!edge_is_multiple()) %>%
  mutate(centrality_e = centrality_edge_betweenness())

########################################################
#We can also convert our active node or edge table back to a tibble:
######################################################## 
marvel_conversation_graph %>%
  activate(nodes) %>% # %N>%
  as.tibble()

marvel_conversation_graph %>%
  activate(edges) %>% # %E>%
  as.tibble()  

########################################################  
#Plotting with ggraph
########################################################


#define a layout, here I am using a Fruchterman-Reingold algorithm.
layout <- create_layout(marvel_conversation_graph, layout = "fr")



#The rest works like any ggplot2 function call, just that we use special geoms for our network,
#like geom_edge_density() to draw a shadow where the edge density is higher, geom_edge_link() to connect edges 
#with a straight line, geom_node_point() to draw node points and geom_node_text() to draw the labels. 
ggraph(layout) + 
  geom_edge_density(aes(fill = Weight)) +
  geom_edge_link(aes(width = Weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(group)), size = 10) +
  geom_node_text(aes(label = name), size = 8, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_graph() +
  labs(title = "Marvel Universe Network",
       subtitle = "Nodes are colored by group")

######################################################## 
#For the next graphs, I want specific colors form the RColorBrewer palette "Set1":
########################################################

cols <- RColorBrewer::brewer.pal(3, "Set1")

ggraph(layout) + 
  geom_edge_density(aes(fill = Weight)) +
  geom_edge_link(aes(width = Weight), alpha = 0.2) + 
  geom_node_point(aes(color = factor(center), size = dist_to_center)) +
  geom_node_text(aes(label = name), size = 8, repel = TRUE) +
  scale_colour_manual(values = c(cols[2], cols[1])) +
  theme_graph() +
  labs(title = "Marvel Universe",
       subtitle = "Nodes are colored by centeredness")




library(tidyverse) # tidy data analysis
library(tidygraph) # tidy graph analysis
library(igraph)

#Importing all csv Files from a Folder
#Importing all csv Files from a Folder
marvel <- read_csv("D:/Courses/02_Socal network Analytics/Harsh Study Material/Final Project/marvel.csv")
marvel

marvel_nodes <- read_csv("D:/Courses/02_Socal network Analytics/Harsh Study Material/Final Project/marvel_nodes.csv")
marvel_nodes

marvel$Source <- marvel_nodes$Label[match(marvel$Source, marvel_nodes$Id)]
marvel$Target <- marvel_nodes$Label[match(marvel$Target, marvel_nodes$Id)]
marvel <- marvel[c("Source","Target","Type","Id","Weight")]

marvel_ch <- marvel %>%   
  select(-Type) %>%
  gather(x, name, Source:Target) %>%
  group_by(name) %>%
  summarise(sum_weight = sum(Weight)) %>%
  ungroup()

marvel_ch_l <- marvel_ch %>%arrange(desc(sum_weight)) %>%top_n(100, sum_weight)

# creating conversation table between top 100 characters
marvel_conversation <- marvel %>%filter(Source %in% marvel_ch_l$name & Target %in% marvel_ch_l$name)
#correct one input mistake
marvel_conversation %>% 
  group_by(Target)%>% 
  summarise(Weight2 = sum(Weight)) %>% 
  arrange(desc(Weight2))
marvel_conversation[which(marvel_conversation$Target == ',"CAPTAIN AMERICA'),]$Target <- "CAPTAIN AMERICA"
#Sort the dataframe by name
m <- marvel_conversation %>% arrange(Source, desc(Source))
#Calculate the total weight for each node
m2 <- m %>% group_by(Source) %>% summarise(Weight1 = sum(Weight))
m3 <- m %>% group_by(Target) %>% summarise(Weight2 = sum(Weight))
m4 <- cbind(m2, m3)
m4 <- m4 %>% group_by(Source) %>% summarise(Weight = Weight1 + Weight2)
#weight factor to attribute of node
mw <- m4$Weight
V(g)$Weight = mw

#We might wanna cut some edges
#max * 0.025 is the magic number we used here

#######********########
g12 <- delete_edges(g, E(g)[Weight<max(E(g)$Weight)*0.025])
#######********########

# degree and degree distribution
dm <- degree(g12)
V(g12)$degree <- dm

deg.dist <- degree_distribution(g12, cumulative=T, mode="all")
plot( x=0:max(degree(g12)), y=deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

hist(dm, breaks=1:vcount(g12)-1, main="Histogram of node degree", 
     col = "orange", border = "black")

#compute betweenness
bm <- betweenness(g12)
hist(bm, breaks = vcount(g12), main = "Histogram of node betweenness",
     col = "orange", border = "black")
V(g12)$be <- bm
V(g12)$be[1]

#build communities for our heroes
g13 <- simplify(g12)

community = fastgreedy.community(g13)
max(community$membership)
#the node of g12 and g13 has no different, so lets give community's attribute to g12 nodes
V(g12)$com <- community$membership
V(g12)$color <- community$membership

#also we can use other method to color our node, like weight, degree or betweenness
node_color = ifelse(V(g12)$Weight >= quantile(V(g12)$Weight)[[4]], 'red', 'gray50')
node_color_d = ifelse(V(g12)$degree >= quantile(V(g12)$degree)[[4]], 'orange', 'royalblue')
node_color_b = ifelse(V(g12)$be >= quantile(V(g12)$be)[[4]], 'magenta', 'ivory4')
#and set nodes' size to weight, degree and betweenness as well
node_size = V(g12)$Weight * 10/ max(V(g12)$Weight) + 1
node_size_d = V(g12)$degree * 10/ max(V(g12)$degree) + 1
node_size_b = V(g12)$be * 10/ max(V(g12)$be) + 1
#don't forget the label size
label_size = V(g12)$Weight * 2/ max(V(g12)$Weight) + 1
label_size_d = V(g12)$degree * 2/ max(V(g12)$degree) + 1
label_size_b = V(g12)$be * 2/ max(V(g12)$be) + 1

#plot community
set.seed(1)
plot(g12, vertex.size = node_size,
     mark.groups = by(seq_along(community$membership), community$membership, invisible),
     layout=layout.fruchterman.reingold,vertex.label=V(g12)$name, vertex.label.color = "black",
     vertex.label.cex = label_size,edge.color = "gray30")
#plot degree
set.seed(1)
plot(g13, vertex.color = node_color_d, layout = layout.fruchterman.reingold,
     vertex.label = V(g13)$name, vertex.label.color = "black", vertex.label.cex = label_size_d,
     vertex.size = node_size_d, edge.color = "gray30")
#plot weight
set.seed(1)
plot(g13, vertex.color = node_color , layout = layout.fruchterman.reingold, vertex.label = V(g13)$name,
     vertex.label.color = "black", vertex.label.cex = label_size,
     edge.color = "gray30", vertex.size = node_size)
#plot betweenness
set.seed(1)
plot(g13, vertex.color = node_color_b , layout = layout.fruchterman.reingold, vertex.label = V(g13)$name,
     vertex.label.color = "black", vertex.label.cex = label_size_b,
     edge.color = "gray30", vertex.size = node_size_b)

# tie strength between two main characters
sort(degree(g12), decreasing = T)[1:10]

edge_connectivity(graph = g, target = 83, source = 40)
neighbors(graph = g12, v = which(V(g)$name == "SPIDER-MAN/PETER PAR"))
neighbors(graph = g12, v = which(V(g)$name == "IRON MAN/TONY STARK"))
length(intersect(neighbors(graph = g12, v = 83), neighbors(graph = g12, v = 40))) / length(union(neighbors(graph = g12, v = 83), neighbors(graph = g12, v = 40)))

#edge prediction
shortest_paths_sim = -distances(g12)
adamic_adar_sim = similarity.invlogweighted(g12)
jaccard_sim = similarity.jaccard(g12)

# name the rows and cols of the similarity matrices using node names
node_names = as.vector(V(g12)$name)
rownames(shortest_paths_sim) = node_names
colnames(shortest_paths_sim) = node_names
rownames(adamic_adar_sim) = node_names
colnames(adamic_adar_sim) = node_names
rownames(jaccard_sim) = node_names
colnames(jaccard_sim) = node_names

recommend_links = function(node_name, sim_matrix, n_recommend = 10){
  similarity_to_node = sim_matrix[toString(node_name), ]
  ranked_nodes_by_sim = 
    names(similarity_to_node[order(similarity_to_node, decreasing = T)])
  neighbours_in_train = c(toString(node_name), as.vector(neighbors(g13, toString(node_name))$name))
  recommend_ids = setdiff(ranked_nodes_by_sim, neighbours_in_train)[1:n_recommend]
  return(recommend_ids)
}

my_answer_s = lapply(V(g13)$name, recommend_links, sim_matrix = shortest_paths_sim, n_recommend = 10)
my_answer_j = lapply(V(g13)$name, recommend_links, sim_matrix = jaccard_sim, n_recommend = 10)
my_answer_a = lapply(V(g13)$name, recommend_links, sim_matrix = adamic_adar_sim, n_recommend = 10)

my_answer_s[[which(V(g13)$name == "SPIDER-MAN/PETER PAR")]]
my_answer_j[[which(V(g13)$name == "SPIDER-MAN/PETER PAR")]]
my_answer_a[[which(V(g13)$name == "SPIDER-MAN/PETER PAR")]]

my_answer_s[[which(V(g13)$name == "THOR/DR. DONALD BLAK")]]
my_answer_j[[which(V(g13)$name == "THOR/DR. DONALD BLAK")]]
my_answer_a[[which(V(g13)$name == "THOR/DR. DONALD BLAK")]]

my_answer_s[[which(V(g13)$name == "BEAST/HENRY &HANK& P")]]
my_answer_j[[which(V(g13)$name == "BEAST/HENRY &HANK& P")]]
my_answer_a[[which(V(g13)$name == "BEAST/HENRY &HANK& P")]]

# 2-degree network of some characters
sort(bm, decreasing = T)[1:10]

n_m = function(hn, hc){
  nm0 <- graph.neighborhood(g12, order = 1,nodes = which(V(g12)$name == hn))[[1]]
  plot(nm0, layout = layout.fruchterman.reingold, vertex.label = V(nm0)$name, 
       edge.color = "gray30", vertex.color = ifelse(V(nm0)$name == hn, hc, V(nm0)$com),
       vertex.label.cex = 5, vertex.label.color = "black")
}

n_m("BEAST/HENRY &HANK& P", "lightblue")
n_m("SPIDER-MAN/PETER PAR", "red")
n_m("THOR/DR. DONALD BLAK", "steelblue")

