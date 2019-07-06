library(igraph)
library(igraphdata)

# 05 julho

# update r
# https://www.r-bloggers.com/updating-r-on-ubuntu/

# simulation study
n <- 100 # network size
nc <- 20 # cases
ns <- 100 # sample size

do_simulation <- function(sample_net, stat, n, param, ns){
  sapply(param, function(pi){
    replicate(ns, stat(sample_net(n, pi)))
  })
}

p <- seq(0, 1, length = nc + 2)
p <- p[-c(1, nc + 2)]
s <- do_simulation(sample_gnp, mean_distance, n, p, ns)
boxplot(s, names = round(p, 1))

# nei = quantidade de conexões de cada nó
# p aqui é diferente (conectividade de arestas)
# diminui no gráfico porque diminui o caminho médio por maior probabilidade de conexão direta
sample_sw <- function(n, p, nei = 5)
  sample_smallworld(1, n, nei, p)
s <- do_simulation(sample_sw, mean_distance, n, p, ns)
boxplot(s, names = round(p, 1))

a <- seq(1, 3, length = nc)
sample_pa1 <- function(n, power)
  sample_pa(n, power, m = 1, directed = FALSE) # m = quantidade de arestas para adicionar quando add novo nó
s <- do_simulation(sample_pa1, mean_distance, n, a, ns)
boxplot(s, names = round(a, 1))

# lazega law firm
library(sand)
data(lazega) # from 'sand'
lazega <- upgrade_graph(lazega)
V(lazega)$name <- sub("V", "", V(lazega)$name)
A <- as_adj(lazega, sparse = FALSE)
vatts <- as_data_frame(lazega, what = "vertices")
g <- network::as.network(A, directed = FALSE)
network::set.vertex.attribute(g, "Gender", vatts$Gender)
network::set.vertex.attribute(g, "Office", vatts$Office)
network::set.vertex.attribute(g, "Practice", vatts$Practice)
network::set.vertex.attribute(g, "Seniority", vatts$Seniority)
# simple color palette
vcolors <- c("red", "dodgerblue", "orange")

library(ergm)
summary_formula(g ~ edges + kstar(2) + kstar(3) + triangle)

gf <- ergm(g ~ edges + nodemain("Seniority") + nodematch("Gender") + nodematch("Office") +
             nodematch("Practice"))
summary(gf)

plot(lazega, vertex.color = vcolors[vatts$Gender])

gf <- ergm(g ~ edges + nodemain("Seniority") + nodematch("Office") + nodematch("Practice"))
summary(gf)

plot(gof(gf))

# validação cruzada (cross validation) é utilizada
# tiramos um pedaço para verificar o que acontece

library(eigenmodel)
ef1 <- eigenmodel_mcmc(A)
eU <- eigen(ef1$ULU_postmean, symm = TRUE)
plot(eU$values) # R = 2

ll1 <- eU$vectors[, 1:2] # latente positions
plot(lazega, layout = ll1, vertex.color = vcolors[vatts$Office])
plot(lazega, layout = ll1, vertex.color = vcolors[vatts$Practice])

# setup covariates
node_main <- function(x, y) x + y
node_match <- function (x, y) x == y
X <- array(dim = c(vcount(lazega), vcount(lazega), 3))
X[, , 1] <- outer(vatts$Seniority, vatts$Seniority, node_main)
X[, , 2] <- outer(vatts$Office, vatts$Office, node_main)
X[, , 3] <- outer(vatts$Practice, vatts$Practice, node_main)

ef2 <- eigenmodel_mcmc(A, X)
eU <- eigen(ef2$ULU_postmean, symm = TRUE)
plot(eU$values) # R = 1

ll2 <- eU$vectors[, 1:2]
plot(lazega, layout = ll2, vertex.color = vcolors[vatts$Office])

library(blockmodels)
sbm <- BM_bernoulli("SBM", A)
sbm$estimate() # run

plot(sbm$ICL, main = "ICL") # integrated classificattion likehood

icl <- which.max(sbm$ICL)
ez <- sbm$memberships[[icl]]$Z
plot(apply(ez, 1, max)) # pegando a máxima probabilidade de pertencer à algum grupo

Z <- apply(ez, 1, which.max)
plot(lazega, layout = ll1, vertex.color = vcolors[Z])

sbm <- BM_bernoulli_covariates("SBM", A, 
                               list(X[,,1], X[,,2], X[,,3]))
sbm$estimate()
plot(sbm$ICL, main = "ICL")

icl <- which.max(sbm$ICL)
ez <- sbm$memberships[[icl]]$Z
Z <- apply(ez, 1, which.max)
plot(apply(ez, 1, max))

plot(lazega, layout = ll1, vertex.color = vcolors[Z])
plot(lazega, layout = ll2, vertex.color = vcolors[Z])
