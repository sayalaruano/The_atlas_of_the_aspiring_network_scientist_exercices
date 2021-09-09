library(readr)
library(dplyr)
library(igraph)

# 1. The network in http://www.networkatlas.eu/exercises/4/1/data.txt 
# is bipartite. Identify the nodes in either type and find the
# nodes, in either type, with the most neighbors.

data <- read_delim("http://www.networkatlas.eu/exercises/4/1/data.txt", delim = " ", 
                   col_names = c("from", "to"))
net <- graph_from_data_frame(data, directed = FALSE)

# Se podría implementar con DFS o BFS, pero no encontré una forma sencilla 
# de hacerlo en igraph. Aquí un intento:
#
# bfs_environment <- new.env()
# assign("type1", c(), envir=bfs_environment)
# assign("type2", c(), envir=bfs_environment)
# 
# bfs_results <- bfs(net, root=1, unreachable = TRUE, rho = bfs_environment,
#     order=TRUE, father=TRUE, callback = function(g, data, exta) {
#       cat(data, "\n")
#       type1 <- get('type1', envir=parent.frame())
#       type2 <- get('type2', envir=parent.frame())
#       
#       Parent es el previo visitado (hermano), no el nodo padre.
#       parent <- data[[2]] + 1
#       if(parent %in% type1) {
#         assign("type2", append(type2, data[[1]] +1 ), envir=bfs_environment)
#       } else {
#         assign("type1", append(type1, data[[1]] +1), envir=bfs_environment)
#       }
#       FALSE
# })
# 
# type1 <- sort(get('type1', envir=bfs_environment))
# type2 <- sort(get('type2', envir=bfs_environment))
# 
# ty1 <- induced_subgraph(net,  V(net)[type1])
# ty2 <- induced_subgraph(net, type2)

type1 <- c()
type2 <- c()
elist <- as_edgelist(net)
for (i in 1:nrow(elist)) {
  e <- elist[i, ]
  cat(e[1], " ", e[2], "\n")
  if(e[1] %in% type1) {
    type2 <- union(type2, e[2])
  } else if(e[2] %in% type1) {
    type2 <- union(type2, e[1])
  }else {
    type1 <- union(type1, e[1])
    type2 <- union(type2, e[2])
  }
}

ty1 <- induced_subgraph(net, type1)
length(E(ty1)) == 0 #TRUE
ty2 <- induced_subgraph(net, type2)
length(E(ty2)) == 0 #TRUE

sort(degree(net, v = type1), decreasing = T)[1]
sort(degree(net, v = type2), decreasing = T)[1]

# 2. The network in http://www.networkatlas.eu/exercises/4/2/data.txt 
# is multilayer. The data has three columns: source and
# target node, and edge type. The edge type is either the numerical id 
# of the layer, or “C” for an inter-layer coupling. Given that this is
# a one-to-one multilayer network, determine whether this network
# has a star, clique or chain coupling.
data <- read_delim("http://www.networkatlas.eu/exercises/4/2/data.txt", delim = "\t", 
                   col_names = c("from", "to", "type"))

net <- graph_from_data_frame(data, directed = FALSE)
## Number of layers
max_layer <- as.integer(max(data$type[data$type != "C"]))
inter_layer <- data %>% filter(data$type == "C")
inter_net <- graph_from_data_frame(inter_layer, directed = FALSE)
## Componentes conexos formados por puras interacciones entre capas
ccs <- components(inter_net)
ctype <- lapply(1:ccs$no, function(comp) {
  nodes <- names(ccs$membership[ccs$membership == comp])
  isub <- induced_subgraph(net, nodes)
  
  # Por isomorfía
  # if(is_isomorphic_to(isub, make_star(length(nodes), mode="undirected"))) {
  #   return("star")
  # } else if(is_isomorphic_to(isub, make_full_graph(length(nodes), directed=FALSE))){
  #   return("clique")
  # } else if(is_isomorphic_to(isub, make_tree(length(nodes), children=1, mode="undirected"))){
  #   return("chain")
  #}
  
  # Por grado
  dgrs <- sort(degree(isub), decreasing = T)
  if(all(dgrs == length(nodes)-1)) {
    return("clique")
  } else if (dgrs[1] == length(nodes)-1 & all(dgrs[-1] == 1)) {
    return("star")
  } else if(all(dgrs[1:2] == 2) & all(dgrs[-c(1,2)] == 1)) {
    return("chain")
  }

})
ctype <- unique(ctype)
ctype[[1]]


# 3. The network in http://www.networkatlas.eu/exercises/4/3/data.txt 
# is a hypergraph, with a hyperedge per line. Transform it
# in a unipartite network in which each hyperedge is split in edges
# connecting all nodes in the hyperedge. Then transform it into a
# bipartite network in which each hyperedge is a node of one type
# and its nodes connect to it.
con = file("http://www.networkatlas.eu/exercises/4/3/data.txt", "r")

all_edges <- tibble(node=c(), edge=c())
i <- 1
while (TRUE) {
  line = readLines(con, n = 1)
  if (length(line) == 0) {
    break
  }
  nodes <- as.numeric(strsplit(line, " ")[[1]])
  print(nodes)
  df <- data.frame(node = nodes, 
               edge = i)
  all_edges <- bind_rows(all_edges, df)
  i <- i+1
}
close(con)

uni_edges <- lapply(1:max(all_edges$edge), function(he) {
  nodes <- all_edges %>% filter(edge == he) %>% pull(node)
  expand.grid(from=nodes, to=nodes)
})
uni_edges <- bind_rows(uni_edges)
uni_edges <- uni_edges %>% filter(from != to)
uni_net <- graph_from_data_frame(uni_edges, directed = FALSE)
plot(uni_net)

bi_edges <- all_edges %>% mutate(to = paste0("h", edge)) %>%
  rename("from" = "node") %>% select(from, to)
bi_net <- graph_from_data_frame(bi_edges, directed = FALSE)
plot(bi_net)

# 4. The network in http://www.networkatlas.eu/exercises/4/4/data.txt 
# is dynamic, the third and fourth columns of the edge
# list tell you the first and last snapshot in which the edge was
# continuously present. An edge can reappear if the edge was
# present in two discontinuous time periods. Aggregate it using a
# disjoint window of size 3.
data <- read_tsv("http://www.networkatlas.eu/exercises/4/4/data.txt",
                 col_names = c("to", "from", "first", "last"))

windows <- seq(from=1, to=max(data$last), by=3)
win_edges <- lapply(windows, function(start) {
  win_data <- data %>% filter(start < first & (start+3) > first  |
                     start >= first & (start+3)<=last)
  win_data$win_start <- start
  win_data$win_end <- start+3
  return(win_data)
  })
win_edges <- bind_rows(win_edges)
