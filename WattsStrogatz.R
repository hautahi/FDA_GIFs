# This program cleans publicly available FDA TIMS violations data
# hautahi

# Packages
library(dplyr)
library(igraph)

# -----------------------
# Erdos-Renyi Model
# -----------------------

P=c(0.002,0.004,0.006,0.008,0.01)

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