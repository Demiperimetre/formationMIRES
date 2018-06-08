setwd("~/Dropbox/Multiplex/Ecologie/Exposes/2017-06-Inecol-Xalapa/Code_R_plots/")


#illustration autres
G=erdos.renyi.game(100,.03)
plot(G,vertex.size=10,vertex.label=NA,edge.width=2)
#pdf("ER.pdf")
#dev.off()

#pdf("degER.pdf")
hist(rowSums(as.matrix(get.adjacency(G))),breaks=10,main='degree distribution',xlab="deg")
#dev.off()



don=read.table("chilean_TI.txt",header = TRUE,sep="\t",row.names = 2)

don=don[,-1]
head(don)


library(igraph)

A=as.matrix(don)
A[1:5,1:5]

plot(graph_from_adjacency_matrix(A,mode="directed"),vertex.size=8,vertex.label=NA)
#pdf("chilean_food_web.pdf")
#dev.off()

#stat des
reciprocity(graph_from_adjacency_matrix(A,mode="directed"))
rownames(A)
n=nrow(A)
sum(A)/(n*n) 
diag(A)

library(sna)
gplot(A,gmode="digraph")


#classical traits
library(vegan)


#degree
outdeg=rowSums(A)
hist(outdeg,breaks = 20)

# pdf("chilean_outdeg.pdf")
# dev.off()

indeg=colSums(A)
hist(indeg,breaks = 20)

# pdf("chilean_intdeg.pdf")
# dev.off()

#nestedness
#exemple
data(sipoo)
## Matrix temperature
out <- nestedtemp(sipoo)
out
plot(out)
plot(out, kind="incid")
## Use oecosimu to assess the non-randomness of checker board units
nestedchecker(sipoo)
oecosimu(sipoo, nestedchecker, "quasiswap")
## Another Null model a nd standardized checkerboard score
oecosimu(sipoo, nestedchecker, "r00", statistic = "C.score")


out=nestedtemp(A)
out
plot(out,kind="incid")
out
oecosimu(A, nestedchecker, "quasiswap")
oecosimu(A, nestedchecker, "r00", statistic = "C.score")


# pdf("chilean_nested.pdf")
# dev.off()



##modularity
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
wtc <- cluster_walktrap(g)
modularity(wtc)
modularity(g, membership(wtc))


groups=cluster_walktrap(graph_from_adjacency_matrix(A,mode="directed"))
xtable(t(table(groups$membership)))
modularity(groups)

plot(graph_from_adjacency_matrix(A,mode="directed"),vertex.size=10,vertex.label=NA,vertex.color=groups$membership)
# pdf("chilean_modularity.pdf")
# dev.off()



##betweenness
betw=betweenness(graph_from_adjacency_matrix(A,mode="directed"))
max(betw)
colors=rainbow(80)

bols=numeric(nrow(A))

for (i in 1:nrow(A))
  bols[i]=colors[ceiling(betw[i])+1]

par(xpd=TRUE,mar=c(0,0,0,4))
plot(graph_from_adjacency_matrix(A,mode="directed"),vertex.size=7,vertex.label=NA,vertex.color=bols)
image.plot(zlim=c(0,80),legend.only=TRUE,col=colors)

# pdf("chilean_between.pdf")
# dev.off()
library(xtable)
xtable(as.table(summary(betw)))


gplot(A,gmode="digraph",vertex.col =bols)
plot(1:61,col=colors)
plot(betw,indeg)



#sbm on chilean
library(blockmodels)
sbm_chil=BM_bernoulli("SBM",A)
sbm_chil$estimate()
k=which.max(sbm_chil$ICL)

plot(sbm_chil$ICL)
abline(v=7)

cl=apply(sbm_chil$memberships[[k]]$Z,1,which.max)

gplot(A,gmode="digraph",vertex.col = cl)

pi=sbm_chil$model_parameters[[k]]$pi
piS=pi
piS[pi<.05]=0
alpha=table(cl)


#pdf("chilean_sbm.pdf")
plot(graph_from_adjacency_matrix(A,mode="directed"),vertex.size=10,vertex.label=NA,vertex.color=cl)
#dev.off()

G=graph_from_adjacency_matrix(piS,mode="directed",diag = TRUE,weighted = TRUE)
plot(G,vertex.size=alpha,vertex.label=NA,vertex.color=1:k,
     edge.width=E(G)/2)

#pdf("chilean_sbm_sum.pdf")
gplot(piS,gmode = "digraph",vertex.col = 1:k,vertex.cex = alpha/20,edge.lwd = piS)
#dev.off()

xtable(t(table(cl)))

