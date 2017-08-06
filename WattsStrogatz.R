# hautahi

# Packages
library(dplyr)
library(igraph)

# -----------------------
# Mess Around
# -----------------------

g <- erdos.renyi.game(5, 0.5, type = "gnp")
png(paste("./output/random1.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10,vertex.color="yellow")
dev.off()

ab <- sample_pa(20, power = 1, m = NULL, out.dist = NULL, out.seq = NULL,
          out.pref = FALSE, zero.appeal = 1, directed = TRUE,
          algorithm = c("psumtree", "psumtree-multiple", "bag"), start.graph = NULL)
png(paste("./output/ab.png",sep=""))
par(bg = "black",col.lab="white")
plot(ab,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10,vertex.color="yellow")
dev.off()

plot(degree_distribution(ab))

er <- erdos.renyi.game(20, 0.2, type = "gnp")
png(paste("./output/er.png",sep=""))
par(bg = "black",col.lab="white")
plot(er,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 10,vertex.color="yellow")
dev.off()

# -----------------------
# Erdos-Renyi Model smaller
# -----------------------
P=c(0.02,0.04,0.06,0.008,0.01)

i=1
g <- erdos.renyi.game(100, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,
     vertex.size = 4,vertex.color="yellow",xlab = paste("p = ",P[i],", n = 100",sep=""))
dev.off()

i=i+1
g <- erdos.renyi.game(100, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,
     vertex.size = 4,vertex.color="yellow",xlab = paste("p = ",P[i],", n = 100",sep=""))
dev.off()

i=i+1
g <- erdos.renyi.game(100, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02, 
     vertex.size = 4,vertex.color="yellow",xlab = paste("p = ",P[i],", n = 100",sep=""))
dev.off()

# -----------------------
# Erdos-Renyi Model
# -----------------------

i=1
g <- erdos.renyi.game(500, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- erdos.renyi.game(500, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- erdos.renyi.game(500, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- erdos.renyi.game(500, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- erdos.renyi.game(500, P[i], type = "gnp")
png(paste("./output/",P[i],"ER.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

# -----------------------
# Watts-Strogatz Models
# -----------------------

P=c(0.1,0.3,0.5,0.7,0.9)

i=1
g <- sample_smallworld(1, 500, 1, P[i], loops = FALSE, multiple = FALSE)
png(paste("./output/",P[i],"WS.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- sample_smallworld(1, 500, 1, P[i], loops = FALSE, multiple = FALSE)
png(paste("./output/",P[i],"WS.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- sample_smallworld(1, 500, 1, P[i], loops = FALSE, multiple = FALSE)
png(paste("./output/",P[i],"WS.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- sample_smallworld(1, 500, 1, P[i], loops = FALSE, multiple = FALSE)
png(paste("./output/",P[i],"WS.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()

i=i+1
g <- sample_smallworld(1, 500, 1, P[i], loops = FALSE, multiple = FALSE)
png(paste("./output/",P[i],"WS.png",sep=""))
par(bg = "black",col.lab="white")
plot(g,vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = paste("p = ",P[i],", mean distance = ",round(mean_distance(g),1),sep=""))
dev.off()
