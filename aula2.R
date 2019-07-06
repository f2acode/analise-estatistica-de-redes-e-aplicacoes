library(igraph)
library(igraphdata)

# example - see if it follows a power law
data(yeast)
hist(degree(yeast), col = "gray")

d <- 0:max(degree(yeast))
dd <- degree_distribution(yeast)
plot(log1p(d), log(dd))

# digression: how to fit a power law to the degree distribution?
df <- dd * vcount(yeast)
s <- coef(glm(df ~ log1p(d), family = poisson))[2]
lp <- exp(s * log1p(d)); lp <- log(lp/sum(lp))
plot(log1p(d), log(dd))
lines(log1p(d), lp)

q <- seq(7, 20, length = 100)
dev <- sapply(q, function(qi)
  deviance(glm(df ~ log1p(qi + d), family = poisson)))
plot(q, dev, type = "l")
abline(v = q[which.min(dev)], lty = 2)

q <- q[which.min(dev)]
s <- coef(glm(df ~ log1p(q + d), family = poisson))[2]
#lp <- 

# centrality
el <- cbind(c(1, 2, 2, 3), c(2, 3, 4, 4))
g <- graph_from_edgelist(el, dir = FALSE)
edge_density(g)
distances(g)
closeness(g)
betweenness(g)
max_cliques(g)
transitivity(g)
transitivity(g, type = "local")

# we use mean distance, it show us how long we need to go to find someone (or something like that)
mean_distance(g)

# example: zachary's karate club

# example: spectral clustering for zachary's
data("karate")
eL <- eigen(laplacian_matrix(karate), symm=TRUE)
plot(eL$values)

fiedler <- eL$vector[, vcount(karate) - 1]
plot(fiedler)
abline(h = 0, lty = 2)

# more elaborate exercise: perform simulations based on subgraphs (induced) and report graph 
# invariants sech as edge density, transitivity, clustering, and diameter

ind_subg_stats <- function(g, n, stats){
  print(replicate(10, stats(induced_subgraph(g, sample.int(vcount(g), n)))))
  hist(replicate(10, stats(induced_subgraph(g, sample.int(vcount(g), n)))))
}
# replicate(10, norm(1))
ind_subg_stats(yeast, 100, edge_density)

# (1) sample_gnp -> erdos-reiny
# sample_smallworld -> small world
# sample_pa -> pref attachment

# homework
# (1) n = 100, number of vertices
# what happens with p = 0.1, 0.2, 0.3, ...,  0.9
# shows density of edges density (boxplot)

sim <- function(property){
  e_den <- c()
  x <- seq.int(0.1, 1, 0.1)
  for (r in c(0:10)){
    for (i in x){
      e_den <- c(e_den, property(sample_gnp(100, i)))
    }
  }
  
  data = data.frame(x, e_den)
  print(data)
  boxplot(data$e_den ~ data$x, xlab="p", ylab="edge_density", main="Erdős–Rényi")
}
sim(edge_density)