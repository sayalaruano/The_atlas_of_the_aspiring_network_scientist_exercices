library(readr)
library(dplyr)
library(Matrix)
library(igraph)

# 1. Calculate the adjacency matrix, the stochastic adjacency ma-
#   trix, and the graph Laplacian for the network in 
# http://www.networkatlas.eu/exercises/5/1/data.txt.

data <- read_delim("http://www.networkatlas.eu/exercises/5/1/data.txt", delim = " ", 
                   col_names = c("from", "to"))
net <- igraph::graph_from_data_frame(data, directed = FALSE)
adjn_matrix <- igraph::as_adj(net, type="both")
adjn_matrix <- as(adjn_matrix, "dgTMatrix") 

stoch_matrix <- adjn_matrix/rowSums(adjn_matrix)
stoch_matrix <- as(stoch_matrix, "dgTMatrix") 

lap <- adjn_matrix * -1
diag(lap) <- degree(net)
lap <- as(lap, "dgTMatrix") 

lap2 <-  igraph::laplacian_matrix(net)
lap2 <- as(lap2, "dgTMatrix") 

# 2. Given the bipartite network in http://www.networkatlas.eu/exercises/5/2/data.txt, 
# calculate the stochastic adjacency matrix
# of its projection. Project along the axis of size 248. 
# (Note: don’t ignore the weights)

data <- read_delim("http://www.networkatlas.eu/exercises/5/2/data.txt", 
                 col_names = c("from", "to", "weight"), delim = " ")
g <- igraph::graph_from_data_frame(data, directed = F)
V(g)$type  <- startsWith(V(g)$name, "a")

g1 <- induced_subgraph(g, V(g)[V(g)$type == 1])
length(E(g1)) == 0
g2 <- induced_subgraph(g, V(g)[V(g)$type == 0])
length(E(g2)) == 0

