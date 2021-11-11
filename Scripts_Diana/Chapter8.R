library(readr)
library(dplyr)
library(expm)
library(Matrix)
library(igraph)

# 1. Calculate the stationary distribution of the network at 
# http://www.networkatlas.eu/exercises/8/1/data.txt in three ways: by
# raising the stochastic adjacency to a high power, by looking at the
# leading left eigenvector, and by normalizing the degree. Verify that
# they are all equivalent.

interactions <- read_delim(url("http://www.networkatlas.eu/exercises/8/1/data.txt"), 
                           col_names = c("to", "from"), delim = " ")
net <- igraph::graph_from_data_frame(interactions, directed = FALSE)

s_adj_matrix <- igraph::stochastic_matrix(net)
i <- 1
p_adj_matrix <- s_adj_matrix
while(TRUE) {
  cat(i, "\n")
  p_adj_matrix_old <- p_adj_matrix
  print(p_adj_matrix_old[1, 1:5])
  i <- i+1
  p_adj_matrix <- as.matrix(s_adj_matrix) %^% i
  diff <- p_adj_matrix - p_adj_matrix_old
  cat(sum(diff >= 0.0000000001), "left\n")
  if(all(diff < 0.0000000001)) {
    break;
  }
}
stat_distr1 <- p_adj_matrix[1, ]

eig_d <- eigen(s_adj_matrix)
stat_distr2 <- eig_d$vectors[1,]

stat_distr3 <- igraph::degree(net)/sum(igraph::degree(net))

# 2. Calculate the non-backtracking matrix of the network used for the
# previous question. (The network is undirected)
net_both <- igraph::as.undirected(igraph::as.directed(net, mode = "mutual"), mode="each")
edge_dir <- lapply(1:length(E(net)), function(edge_id) {
  cat(edge_id, "\n")
  vs <- igraph::ends(net, edge_id)
  incident1 <- incident(net, vs[1])
  incident2 <- incident(net, vs[2])
  ### Esto no corre en el lapply
  #incident1 <- incident1[incident1 != E(net)[edge_id]]
  #incident2 <- incident2[incident2 != E(net)[edge_id]]
  directions <- list(as.integer(E(net_both) %in% incident1),
                      as.integer(E(net_both) %in% incident2))
  names(directions) <- c(paste(vs, collapse = "--"), paste(rev(vs), collapse = "--"))
  return(directions)
})
edge_dir <- bind_rows(edge_dir)
