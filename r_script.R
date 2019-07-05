# source
http://dontpad.com/redescarvalho

library(igraph)
(g <- graph.formula(1-2, 2-3, 3-4, 2-4))

V(g)
E(g)
plot(g)

as_adjacency_matrix(g)
as_edgelist(g, names = FALSE)
ends(g, 1, names = FALSE)
neighborhood(g)
neighbors(g, 1)

# basic definitions
degree(g)

# laplacian
my_laplacian <- function (g, sparse=FALSE)
  diag(degree(g)) - as_adj(g, sparse = sparse)

my_laplacian(g)

# Exercise
incidence <- function(g, w = NULL){
  if (is.null(w)) w <- rep(1, vcount(g))
  w <- sqrt(w)
  M <- matrix(0, nrow = vcount(g),
              ncol = ecount(g))
  el <- as_edgelist(g, names= FALSE)
  for (e in seq_len(ecount(g))){
    M[el[e, 1], e] <- -w[e]
    M[el[e, 2], e] <- -w[e]
  }
  M
}

incidence(g)

# building networks
vfrom <- c(1, 2, 3, 4)
vto <- c(2, 3, 4, 4)
(g <- graph_from_data_frame(
  data.frame(from=vfrom, to=vto),
  directed=FALSE
))

(g <- graph_from_edgelist(
  cbind(vfrom, vto),
  directed = FALSE
))

A <- matrix(c(0, 1, 0, 0,
              1, 0, 1, 1,
              0, 1, 0, 1,
              0, 1, 1, 0), nrow = 4)
plot(graph_from_adjacency_matrix(A, mode = "undirected"))

# attributes
V(g)$name <- c("Alice", "Bob", "Carol", "David")
V(g)$color <- c("red", "green", "blue", "orange")
E(g)$friendship <- c(10, 2, 4, 5)
plot(g, edge.width = E(g)$friendship)

(d <- strength(g,
               weight = E(g)$friendship))

# operations
g <- delete_vertex_attr(g, "size")
(g - vertices(1, 4))

induced.subgraph(g, c(2, 3))
plot(g - edges(4))

# exercise
sand::elist.lazega
g <- graph_from_data_frame(sand::elist.lazega, directed = FALSE)
plot(g)
# verificando cardinalidade de vértices e arestas
c(vcount(g), ecount(g))

# descriptive statistics
head(degree(g), n = 10)
hist(degree(g), col="gray")
is_connected(g)
# verificando quais vértices estão em quais components (lembrar que components são subgrafos)
clusters(g)
# maior caminho mínimo
diameter(g)

# special graphs
plot(make_full_graph(7))
plot(make_ring(7))
# grade
plot(make_lattice(7))
plot(make_tree(7, mode="undirected"))
plot(make_star(7, mode="undirected"))

# exercise
plot(make_lattice(7, circular = TRUE))
plot(make_star(7, mode = "undirected"))
star_by_tree <- function(n){
  plot(make_tree(n, children = n-1, mode = "undirected"))
}
star_by_tree(10)

# example: yeast dataset
library(igraphdata)
data("yeast")
hist(degree(yeast), col="gray")
d <- 0:max(degree(yeast))
dd <- degree_distribution(yeast)
plot(log1p(d), log(dd))

# tomorrow -> regressão multinomial
