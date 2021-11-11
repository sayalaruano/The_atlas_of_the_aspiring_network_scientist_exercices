library(readr)
library(dplyr)
library(igraph)

# 1. Write the code to perform a random walk of arbitrary length on
# the network in http://www.networkatlas.eu/exercises/7/1/data.txt

interactions <- read_delim(url("http://www.networkatlas.eu/exercises/7/1/data.txt"), 
                         col_names = c("to", "from"), delim = " ")

net <- igraph::graph_from_data_frame(interactions, directed = FALSE)
is.connected(net)
doRandomWalk <- function(network, steps)
{
  node <- sample(V(network), 1)
  visited <- c()
  for (i in 1:steps) {
    cat("step:", i, "node:", node, "\n")
    visited <- append(as.vector(visited), node)
    node <- sample(neighbors(network,node), 1)
  }
  return(visited)
}

tenSteps <- doRandomWalk(net, 10)
fortySteps <- doRandomWalk(net, 40)

# 2. Find all cycles in the network in http://www.networkatlas.eu/
#   exercises/7/2/data.txt. Note: the network is directed.
interactions <- read_delim(url("http://www.networkatlas.eu/exercises/7/2/data.txt"), 
                           col_names = c("to", "from"), delim = " ")

net <- igraph::graph_from_data_frame(interactions, directed = TRUE)


# 3. What is the average reciprocity in the network used in the previ-
#   ous question? How many nodes have a reciprocity of zero?
net_rec <- reciprocity(net, ignore.loops = TRUE, mode = "default")   
reciprocityByNode <- lapply(V(net), function(node) {
  n_out <- neighbors(net, node, mode = "out")
  n_in <- neighbors(net, node, mode = "in")
  
  if(length(n_out) == 0 & length(n_in) == 0) {
    return(0)
  } else if(length(n_out) == 0) {
    return(Inf)
  }
  return(sum(n_in %in% n_out)/length(n_out))
})
reciprocityByNode <- unlist(reciprocityByNode, use.names = F)
sum(reciprocityByNode == 0, na.rm = T)
sum(is.infinite(reciprocityByNode))

# 4. How many weakly and strongly connected component does the
# network used in the previous question have? Compare their sizes,
# in number of nodes, with the entire network. Which nodes are in
# these two components?
strg_components <- components(net, mode = "strong")
strg_components$csize
max(strg_components$csize)/length(V(net))
in_max_strg <- names(strg_components$membership)[strg_components$membership == which.max(strg_components$csize)]

weak_components <- components(net, mode = "weak")
weak_components$csize
in_max_weak  <- names(weak_components$membership)[weak_components$membership == which.max(weak_components$csize)]
max(weak_components$csize)/length(V(net))

in_both <- intersect(in_max_strg, in_max_weak)
