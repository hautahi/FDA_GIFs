# This program

# Packages
library(dplyr)
library(igraph)

g <- erdos.renyi.game(5, 0.5, type = "gnp")

vertex_attr(g) <- list(name = c("S","S","I","I","I"),
                       color = c("blue","blue","red","red","red"))

plot(g, edge.arrow.size=0.02,vertex.size = 20)

g1 <- erdos.renyi.game(5, 1, type = "gnp")

vertex_attr(g1) <- list(name = c("S","S","I","I","I"),
                       color = c("blue","blue","red","red","red"))

plot(g1, edge.arrow.size=0.02,vertex.size = 20)