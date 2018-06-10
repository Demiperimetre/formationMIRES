rm(list=ls())
load("Kabururu.Rdata")

dim(Kabururu$Net)
library(igraph)

Gk=graph_from_adjacency_matrix(Kabururu$Net,mode = "directed")
degree(Gk)
plot(Gk,vertex.label=NA,vertex.size=2)
write.table(get.edgelist(Gk),"Kabururu_edges.csv")



colnames(Kabururu$Net)
class(Kabururu$NodeCovar)

n=nrow(Kabururu$NodeCovar)
Kego = data.frame(dialect = character(n),sex = character(n),age=Kabururu$NodeCovar[,5])
Kego$sex="F"
Kego$sex[Kabururu$NodeCovar[,7]==1]="M"
table(Kego$sex,Kabururu$NodeCovar[,7])
Kabururu$NodeCovar[1:3,]
Kego$dialect="CH"
Kego$dialect[Kabururu$NodeCovar[,2]==1]="MB"
Kego$dialect[Kabururu$NodeCovar[,3]==1]="OTH"
Kego$dialect[Kabururu$NodeCovar[,4]==1]="TH"
table(Kego$dialect)
colSums(Kabururu$NodeCovar)


write.table(Kego,"Kabururu_ego.csv")
