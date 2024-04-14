library(tidyverse)
library(rdflib)
library(ggnetwork)
library(igraph)
library(data.table)
library(ggraph)
library(shiny)
library(networkD3)

# 
# df <- "from year to
# <Claudio> A <gianni>.
# <gianni> B <mario>.
# <mario> C <albert>."






















# below is legacy code ----------------------------------------------------




edgelist_creator <- function(ttl) {
  
  read.csv(text=  gsub("\\.","", ttl ), sep= " ")
  
  
  

}

#°edgelist_creator(ttl)


graph_creator <- function(df){
  
  graph_from_data_frame(edgelist_creator(df)[-2]) %>%  set_edge_attr(value =  edgelist_creator(df)[,2], name = "Property")
  
}





graph_creator_attr <- function(df){
  
  edgelist_creator(df) %>% relocate(to, .after = from) %>% graph_from_data_frame()
  
}


graph_creator_basic <- function(df){
  
  graph_from_data_frame((edgelist_creator(df)[-2]))
  
  
  
}


# make visgraph funciton --------------------------------------------------


make_visgraph <- function(df){
  

  
  edgelist <- edgelist_creator(df)
  
  edgelist
  
  
  
  # nodes -------------------------------------------------------------------
  
  
  f <- edgelist[1]
  g <- edgelist[3]
  colnames(g) <- "from"
  
  nodes <- add_row(f,g) %>% unique()
  id_table <-  data.frame(node=nodes, id=1:length(nodes$from))
  
  nodes$id <- id_table$id
  nodes <- data.frame(id=id_table$id, label=id_table$from )
  nodes
  
  
  # edges -------------------------------------------------------------------
  
  
  
  from <- left_join(edgelist[1],id_table )
  to <- left_join(edgelist[3],id_table, by=c("to"="from") )
  
  edges <- data.frame(from=unname(from[2]),
                      to= unname(to[2]),
                      label=unname(edgelist[2]))
  edges
  
  
  # drawing network ---------------------------------------------------------
  
  
  
  
  # directed network
  network <- visNetwork(nodes, edges) %>% 
    visEdges(arrows = 'from', scaling = list(min = 2, max = 2)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = TRUE)
  
  
  
  
  
  # Hierarchical Layout
  network_hierach <- visNetwork(nodes, edges) %>% 
    visEdges(arrows = 'from', scaling = list(min = 2, max = 2)) %>%
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE, manipulation = TRUE) %>% visHierarchicalLayout()
}

edges_function <- function(df){
  
  edgelist <- edgelist_creator(df)
  
  edgelist
  f <- edgelist[1]
  g <- edgelist[3]
  colnames(g) <- "subject"
  
  nodes <- add_row(f,g) %>% unique()
  id_table <-  data.frame(node=nodes, id=1:length(nodes$subject))
  
  from <- left_join(edgelist[1],id_table )
  to <- left_join(edgelist[3],id_table, by=c("object"="subject") )
  
  edges <- data.frame(from=unname(from[2]),
                      to= unname(to[2]),
                      label=unname(edgelist[2]))
  edges
  
  
}


nodes_function <- function(df){
  
  edgelist <- edgelist_creator(df)
  
  edgelist
  f <- edgelist[1]
  g <- edgelist[3]
  colnames(g) <- "subject"
  
  nodes <- add_row(f,g) %>% unique()
  id_table <-  data.frame(node=nodes, id=1:length(nodes$subject))
  
  nodes$id <- id_table$id
  nodes <- data.frame(id=id_table$id, label=id_table$subject )
  nodes
  
  
}


# not necessary -----------------------------------------------------------

# 
# 
# 
# 
# df <- "from year to
# <Claudio> A <gianni>.
# <gianni> B <mario>.
# <mario> C <albert>."
# E(graph)$label <- paste0(data$name,"\n\n\n",n2)
# 
# 
# 
# 
# 
# graph <- graph_creator_basic(df)
# 
# edges
# 
# 
# ego <- c("Stability (high)", "Stability (high)", "Stability (high)")
# alter <- c("Status", "Depressive symptoms", "Anxiety Symptoms")
# association <- c("A", "B", "C")
# 
# 
# nodes <- c("Stability (high)", "Status", "Depressive symptoms", "Anxiety Symptoms")
# x <- c(-5, 5, 5, 5)
# y <- c(4, 8, 4, 0)
# 
# edges <- as.data.frame(cbind(ego, alter, association))
# nodes <- cbind.data.frame(nodes, x, y)
# 
# nodes$x <- as.numeric(nodes$x)
# nodes$y <- as.numeric(nodes$y)
# 
# 
# study1 <- graph_from_data_frame(edges, nodes, directed = TRUE)
# 
# E(study1)$color <- "red"
# 
# plot(study1, layout=as.matrix(nodes[,c("x","y")]),
#      vertex.size = 75,
#      vertex.color = "gray",
#      vertex.label.color = "black",
#      vertex.label.family = "Arial",
#      vertex.label.cex = 0.7,
#      edge.arrow.size = 0.7,
#      edge.width = 3.5,
#      edge.color = E(study1)$color,
#      edge.label = E(study1)$association,
#   #   edge.label.y = c(0.6, 0.2, -0.5), # specify the y-coordinate for each label
#      edge.label.cex = 3,
#      edge.label.color = "black")
# 
# 
# nodes
# 
# 
# 
# 
# graph <- graph_from_data_frame((edgelist_creator(df)[-2]))
# 
# set_edge_attr(graph = graph,name="year", value = edgelist_creator(df)[2])
# 
# get.edge.attribute(graph)
# 
# graph_creator_attr(df)
# 
# 
# 
# 
# ggraph(graph_creator_attr(df))+ 
#      geom_node_point()+
#   geom_edge_link()
#   


# graph <- graph_from_data_frame(edgelist_creator(df) %>% 
#                                   relocate(to, .after = from)
#                                )
# 
# edge_attr(graph)
# 
# graph <- graph_creator(df)
# set_edge_attr(graph,value =  edgelist_creator(df)[2], name = "Property")
# edge_attr(graph)
# 
# 
# ggraph(graph) + 
#   geom_edge_link(aes(colour = factor(edgelist_creator(df)[2])))+ 
#   geom_node_point()
# 
# 
# graph <- graph_from_data_frame(highschool)
# 
# # Not specifying the layout - defaults to "auto"
# ggraph(graph) + 
#   geom_edge_link(aes(colour = factor(year))) + 
#   geom_node_point()
# 
# E(graph)
# 
# set_edge_attr(graph,value =  edgelist_creator(df)[2], name = "Property")
# E(graph)
# highschool
# 
# edge.attributes(graph, E(graph))
# 
# E(graph)
# 
# edge
# 
# ggraph(graph, layout = 'dendrogram', circular = TRUE) + 
#   geom_edge_diagonal() +
#   geom_node_point() +
#   theme_void()
#                    
# 
# graph
# 
# df <- "from year to
# <Claudio> A <gianni>.
# <gianni> B <mario>.
# <mario> C <albert>."
# 
# edgelist_creator(df)

#  graph_from_data_frame(edgelist_creator(df)[-2]) %>%  set_edge_attr(value =  edgelist_creator(df)[2], name = "Property")
# 
# f
# df
# edgelist_creator(df)[2]
# 
# 
# graph_from_edgelist(edgelist_creator(df)[-2])
# 
# 
# graph_from_data_frame(edgelist_creator(df)[-2])
