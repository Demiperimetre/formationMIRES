setwd("~/Dropbox/Multiplex/Ecologie/Exposes/2017-06-Inecol-Xalapa/Code_R_plots")

library(igraph)

n=4
X=matrix(rbinom(n^2,1,.2),nrow=n,ncol=n)
diag(X)=0
X[lower.tri(X)]=X[upper.tri(X)]
isSymmetric(X)
X[1,2]=1
X[2,1]=1

library(xtable)
xtable(X,digits = 0)

G=graph_from_adjacency_matrix(X,mode="undirected")
plot(G)

pdf("graphe_adj.pdf")
plot(G)
dev.off()



### LBM 

n=4
m=7
set.seed(4)
X=matrix(rbinom(n*m,1,.2),nrow=n,ncol=m)

library(xtable)
xtable(X)

G=graph_from_incidence_matrix(X)
namesA=paste("A",1:4,sep = "")
namesB=paste("B",1:7,sep="")
plot(G,layout=layout_as_bipartite,vertex.label=c(namesA,namesB),vertex.color=c(rep(1,4),rep(2,7)))

pdf("graphe_bipartite.pdf")
plot(G,layout=layout_as_bipartite,vertex.label=c(namesA,namesB),vertex.color=c(rep(1,4),rep(2,7)))
dev.off()

vertex.attributes(G)
xtable(get.edgelist(G))
