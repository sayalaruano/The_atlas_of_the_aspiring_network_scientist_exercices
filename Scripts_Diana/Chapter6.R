library(readr)
library(dplyr)
library(ggplot2)
library(igraph)
library(stats)
# 1. Write the in- and out-degree sequence for the graph in Figure
# 6.3(a). Are there isolated nodes? Why? Why not?

in_degree <- c(2, 1, 1, 1, 0)
out_degree <- c(0, 2, 1, 0, 2)
names(in_degree) <- c("a", "b", "c", "d", "e")
## Sí, no hay forma de salir del vértice a ni del del e

# 2. Calculate the degree of the nodes for both node types in the
# bipartite adjacency matrix from Figure 6.5(a). Find the isolated
# node(s).
v1 <- c(2, 3, 4, 2, 3, 1, 0, 2, 1)
v2 <- c(1, 2, 3, 1, 1, 2, 4, 1, 3)
## Vértice 7 del set 1

# 3. Write the degree sequence of the graph in Figure 6.7. First consid-
#   ering all layers at once, then separately for each layer.

# 4. Plot the degree distribution of the network at http://www.networkatlas.
# eu/exercises/6/4/data.txt. Start from a plain degree distribu-
#   tion, then in log-log scale, finally plot the complement of the
# cumulative distribution.

interactions <- read_tsv(url("https://www.networkatlas.eu/exercises/6/4/data.txt"), 
                col_names = c("to", "from"))
net <-  igraph::graph_from_data_frame(interactions)
dd <- degree_distribution(net)
cd <- degree_distribution(net, cumulative = T)

ggplot() + 
  geom_point(aes(x = 0:(length(dd)-1), y = dd), size = 0.5) + 
  xlab("Degree") +
  ylab("Fraction") +
  theme_base()

ggplot() + 
  geom_point(aes(x = 0:(length(dd)-1), y = dd), size = 0.5) + 
  xlab("Degree") +
  ylab("Fraction") +
  theme_base() + 
  scale_x_log10() + 
  scale_y_log10()

ggplot() +
  geom_line(aes(x = 0:(length(dd)-1), y = cd)) +
  theme_base() + 
  scale_x_log10() + 
  scale_y_log10()

### Take 2
degree_seq <- degree(net)
degree_seq <- table(degree_seq)
dd2 <- as.numeric(degree_seq)
dd2_names <- as.numeric(names(degree_seq))

ggplot() + 
  geom_point(aes(x = dd2_names, y = dd2), size = 0.5) + 
  xlab("Degree") +
  ylab("Fraction") +
  theme_base() 

ggplot() + 
  geom_point(aes(x = dd2_names, y = dd2), size = 0.5) + 
  xlab("Degree") +
  ylab("Fraction") +
  theme_base() +
  scale_x_log10() +
  scale_y_log10()

cd2 <- rev(cumsum(rev(dd2))/sum(dd2))
ggplot() + 
  geom_line(aes(x = dd2_names, y = cd2), size = 0.5) + 
  xlab("Degree") +
  ylab("Fraction") +
  theme_base() +
  scale_x_log10() + 
  scale_y_log10()

# 5. Estimate the power law exponent of the CCDF degree distribution
# from the previous exercise. First by a linear regression on the log-
#   log plane, then by using the powerlaw package. Do they agree? Is
# this a shifted power law? If so, what’s kmin ? (Hint: powerlaw can
# calculate this for you)
ccdf <- tibble(k = log10(dd2_names), cd = log10(cd2))
linearModel <- lm(cd ~ k, ccdf)
summary(linearModel)

slope <- linearModel$coefficients[[2]]
intercept <- 10 ** linearModel$coefficients[[1]]
r2 <- summary(linearModel)$r.squared
plot(cd ~ k, data = ccdf)
abline(linearModel, col = "red")

fit1 <- igraph::fit_power_law(degree_seq+1)


# 6. Find a way to fit the truncated power law of the network at http:
#   //www.networkatlas.eu/exercises/6/6/data.txt. Hint: use the
# scipy.optimize.curve_fit to fit an arbitrary function and use the
# functional form I provide in the text.





  