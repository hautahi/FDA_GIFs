# This program cleans publicly available FDA TIMS violations data
# hautahi

# Packages
library(dplyr)
library(igraph)
library(ggplot2)

# -----------------------
# Nodes stay in same place. Fixed number of edges and degree distribution
# -----------------------

n = 100; m = 200

# Create Base Network
l = sample_gnm(n, m, directed = FALSE, loops = FALSE)

# Create Networks
deg <- degree(l)
G = list()
for (i in 1:5) {
  G[[i]] = sample_degseq(deg)
}

# Create Layout Settings
set.seed(2)
layg1 <- layout.fruchterman.reingold(G[[1]])
xlim <- range(c(layg1[,1], layg1[,1]))
ylim <- range(c(layg1[,2], layg1[,2]))

# Plot network graphs
for (i in 1:5) {
  png(paste("./output/StaticFixed",i,".png",sep=""))
  par(bg = "black",col.lab="white")
  plot(G[[i]],vertex.size=50,layout=layg1,xlim=xlim,ylim=ylim,rescale=FALSE,vertex.label=NA,vertex.size = 0.5,edge.arrow.size=0.02,
       vertex.frame.color="orange",main=paste("Network",i))
  title(paste("Network",i),col.main="white")
  dev.off()
}

# Plot degree distributions
for (i in 1:5) {
  
  x=degree_distribution(G[[i]])
  df = data.frame(dist=x,deg=1:length(x)) %>%
    ggplot(aes(x=factor(deg),y=dist)) + geom_bar(stat="identity",fill = "orange")+
    xlab("\n Weighted Node Degrees") + ylab("Relative Frequency") + 
    ggtitle(paste("Network",i))+
    theme(panel.background = element_rect(fill = "black", color  =  NA),
          legend.title=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(color = "black", fill = "black"),
          axis.text.x = element_text(color = "white"),  
          axis.text.y = element_text(color = "white"),
          axis.title.x = element_text(color = "white", margin = margin(0, 10, 0, 0)),  
          axis.title.y = element_text(color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
          plot.title = element_text(color = "white",hjust=0.5))
  df
  ggsave(paste("./output/StaticDist",i,".png",sep=""))
}

# -----------------------
# Nodes move. Fixed number of edges and degree distribution
# -----------------------

# Create Networks
deg <- degree(l)
G = list()
for (i in 1:5) {
  G[[i]] = sample_degseq(deg)
}

# Plot network graphs
for (i in 1:5) {
  png(paste("./output/StaticMove",i,".png",sep=""))
  par(bg = "black",col.lab="white")
  plot(G[[i]],vertex.size=5,vertex.label=NA,vertex.size = 0.5,edge.arrow.size=0.02,
       vertex.frame.color="orange")
  title(paste("Network",i),col.main="white")
  dev.off()
}

# Plot degree distributions
for (i in 1:5) {
  
  x=degree_distribution(G[[i]])
  df = data.frame(dist=x,deg=1:length(x)) %>%
    ggplot(aes(x=factor(deg),y=dist)) + geom_bar(stat="identity",fill = "orange")+
    xlab("\n Node Degrees") + ylab("Relative Frequency") + ggtitle(paste("Network",i)) +
    theme(panel.background = element_rect(fill = "black", color  =  NA),
          legend.title=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(color = "black", fill = "black"),
          axis.text.x = element_text(color = "white"),  
          axis.text.y = element_text(color = "white"),
          axis.title.x = element_text(color = "white", margin = margin(0, 10, 0, 0)),  
          axis.title.y = element_text(color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
          plot.title = element_text(color = "white",hjust=0.5))
  df
  ggsave(paste("./output/StaticMoveDist",i,".png",sep=""))
}

# -----------------------
# Nodes don't move. But weigts of edges change.
# -----------------------

# Create weighted Networks
deg <- degree(l)
gr = sample_degseq(deg)
W = 1:5
G = list()
for (i in 1:5) {
  E(gr)$weight <- runif(length(E(gr)), 1, W[i])
  G[[i]] = gr
}

# Create Layout Settings
set.seed(1)
layg1 <- layout.fruchterman.reingold(G[[1]])
xlim <- range(c(layg1[,1], layg1[,1]))
ylim <- range(c(layg1[,2], layg1[,2]))

# Plot network graphs
for (i in 1:5) {
  png(paste("./output/Dynamic",i,".png",sep=""))
  par(bg = "black",col.lab="white")
  plot(G[[i]],vertex.size=50,layout=layg1,xlim=xlim,ylim=ylim,rescale=FALSE,vertex.label=NA,vertex.size = 0.5,edge.arrow.size=0.02,
       vertex.frame.color="orange",edge.width=E(G[[i]])$weight)
  title(paste("Network",i),col.main="white")
  dev.off()
}

# Plot degree distributions
for (i in 1:5) {
  
  x = graph.strength(G[[i]])
  df = data.frame(dist=x) %>%
    ggplot(aes(dist)) + geom_histogram(aes(y=0.5*..density..),bins=8,fill="orange",color="black") +
    xlab("\n Weighted Node Degrees") + ylab("Relative Frequency") + xlim(c(0,25))+ylim(c(0,0.06))+
    ggtitle(paste("Network",i)) +
    theme(panel.background = element_rect(fill = "black", color  =  NA),
          legend.title=element_blank(),
          axis.ticks.x=element_blank(),
          legend.position="bottom",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.background = element_rect(color = "black", fill = "black"),
          axis.text.x = element_text(color = "white"),  
          axis.text.y = element_text(color = "white"),
          axis.title.x = element_text(color = "white", margin = margin(0, 10, 0, 0)),  
          axis.title.y = element_text(color = "white", angle = 90, margin = margin(0, 10, 0, 0)),
          plot.title = element_text(color = "white",hjust=0.5))
  df
  ggsave(paste("./output/DynamicDist",i,".png",sep=""))
}

# -----------------------
# Nodes stay in same place. Fixed number of edges
# -----------------------

P=c(0.05,0.05,0.005,0.008,0.01)

# 1. Possible different number of connections
g1 <- erdos.renyi.game(n, m, type = "gnm")
g2 <- erdos.renyi.game(n, m, type = "gnm")
g3 <- erdos.renyi.game(n, m, type = "gnm")


V(g1)$name=letters[1:n]
V(g2)$name=letters[1:n]
V(g3)$name=letters[1:n]

# Layouts
set.seed(1)
layg1 <- layout.fruchterman.reingold(g1)
set.seed(1)
layg2 <- layout.fruchterman.reingold(g2)
set.seed(1)
layg3 <- layout.fruchterman.reingold(g3)

layg2[which(V(g2)$name %in% V(g1)$name), ]=layg1[which(V(g1)$name %in% V(g2)$name),]
layg3[which(V(g3)$name %in% V(g1)$name), ]=layg1[which(V(g1)$name %in% V(g3)$name),]

xlim <- range(c(layg1[,1], layg3[,1]))
ylim <- range(c(layg1[,2], layg3[,2]))

par(mfrow=c(1,3))
i=1
png(paste("./output/Static",i,".png",sep=""))
par(bg = "black",col.lab="white")
plot(g1,vertex.size=50,layout=layg1,xlim=xlim,ylim=ylim,rescale=FALSE,vertex.label=NA,vertex.size = 0.5,edge.arrow.size=0.02)
dev.off()

i=i+1
png(paste("./output/Static",i,".png",sep=""))
par(bg = "black",col.lab="white")
plot(g2,vertex.size=50,layout=layg2,xlim=xlim,ylim=ylim,rescale=FALSE,vertex.label=NA,vertex.size = 0.5,edge.arrow.size=0.02)
dev.off()

i=i+1
png(paste("./output/Static",i,".png",sep=""))
par(bg = "black",col.lab="white")
plot(g3,vertex.size=50,layout=layg3,xlim=xlim,ylim=ylim,rescale=FALSE,vertex.label=NA,vertex.size = 0.5,edge.arrow.size=0.02)
dev.off()
