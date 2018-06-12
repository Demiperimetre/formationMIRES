rm(list = ls())

#-----------------------------------------
# Corrige VanesseData Kabururu
#-----------------------------------------

#-----------------------------------------
# importation donnees
#-----------------------------------------

load("Kabururu.Rdata")
Net=as.matrix(Kabururu$Net)
EdgeCovar=Kabururu$EdgeCovar
dim(EdgeCovar)
NodeCovar=Kabururu$NodeCovar
dim(NodeCovar)

# nombre de noeuds
n=dim(Net)[1]
n

# nombre d'arêtes
sum(Net)

# densité du graphe
sum(Net)/(n*(n-1))

library(sna)
gplot(Net)


# Colorer les noeuds en fonction des dialects - et usage Igraph
library(igraph)
N=graph_from_adjacency_matrix(Net, mode = c("directed"), weighted = NULL, diag = TRUE)

# histogramme des degres
hist(igraph::degree(N), col="lightblue")

## definir les couleurs des noeuds en fonction du dialect 
#col=sapply(1:n,function(i) which.max(NodeCovar[i,1:4]))
col<- ifelse(NodeCovar[,"CH"]==1,"purple",ifelse(NodeCovar[,"TH"]==1,"green","orange"))
names(col)<-rownames(Net)
set_vertex_attr(N,"dialect", value = col)

# plot du graph avec des couleurs de noeud différentes selon les dialects
plot(N,edge.width=0.1, edge.color="black",vertex.size=4, vertex.color=col,vertex.label=NA, edge.arrow.size=0.1)
### Structure de type coeur-périphérie: un coeur d'individus densément connectés entourés d'une couronne d'individus qui le sont moins

#-----------------------------------------
# SBM
#-----------------------------------------

library(blockmodels)
sbm.kabururu <- BM_bernoulli("SBM",Net)
m=sbm.kabururu$estimate()

# nombre de groupes estime
Q=which.max(sbm.kabururu$ICL)
Q

# probabilités de connexion entre groupes
sbm.kabururu$model_parameters[Q]

# probabilités a priori d'appartenance aux groupes
alpha=colSums(sbm.kabururu$memberships[[Q]]$Z)/n

# proba a posteriori d'appartenance des noeuds aux groupes
membership=sbm.kabururu$memberships[[Q]]$Z

# graphe des groupes
library(igraph)
piS=sbm.kabururu$model_parameters[[Q]]$pi
G = graph_from_adjacency_matrix(piS, mode = c("directed"), weighted = TRUE, diag = TRUE)
plot.igraph(G,vertex.size=alpha*100,edge.width=abs(E(G)$weight*100),vertex.color=1:Q)

# caractéristiques des individus de chaque groupe
node.membership=sapply(1:n,function(i) which.max(membership[i,]))
n=c()
j=1
#j=2
which(node.membership==j)
n[j]=length(which(node.membership==j))
paste("nombre d'individus dans le groupe",j,":",n[j])
sapply(c(1:4,6:7),function(i) paste("proportion de", colnames(NodeCovar)[i], ":", sum(NodeCovar[which(node.membership==1),i])/n[j]))
paste("moyenne de", colnames(NodeCovar)[5], ":", mean(NodeCovar[which(node.membership==j),5]))

# graphe des noeuds colores selon groupe d'appartenance
#plot.igraph(N, vertex.color=node.membership,vertex.label=rownames(Net))
plot(N, vertex.color=node.membership,vertex.label=rownames(Net))

#-----------------------------------------
# SBM avec covariables
#-----------------------------------------

ListVar=list(EdgeCovar[,,1],EdgeCovar[,,2],EdgeCovar[,,3],EdgeCovar[,,4],EdgeCovar[,,5],EdgeCovar[,,6],EdgeCovar[,,7])
sbm.cov<- BM_bernoulli_covariates_fast("SBM",Net, ListVar)
m=sbm.cov$estimate() 

# nombre de groupes estime
Q=which.max(sbm.cov$ICL)
Q
# on trouve un seul groupe ce qui veut dire :
# toutes les covariables prises en compte il n'y a plus d'autres facteurs engendrant de structure 

# probabilités de connexion entre groupes et coefficients de regression
sbm.cov$model_parameters[Q]

# probabilités a priori d'appartenance aux groupes
alpha=colSums(sbm.cov$memberships[[Q]]$Z)/n
alpha

# influence des covariables
sbm.cov$model_parameters[[Q]]$beta

# #age 
# sbm.age <- BM_bernoulli_covariates_fast("SBM",Net, EdgeCovar[,,5])
# m=sbm.age$estimate(); Q=which.max(sbm.age$ICL)
# Q 
# sbm.age$model_parameters[Q]
# colSums(sbm.age$memberships[[Q]]$Z)/n
# 
# # sexe 
# sbm.sex <- BM_bernoulli_covariates_fast("SBM",Net,list(EdgeCovar[,,6],EdgeCovar[,,7])) 
# m=sbm.sex$estimate()
# Q=which.max(sbm.sex$ICL) 
# Q 
# sbm.asex$model_parameters[Q]
# colSums(sbm.age$memberships[[Q]]$Z)/n

# dialect 
# sbm.dialect <- BM_bernoulli_covariates_fast("SBM",Net,
# list(EdgeCovar[,,1],EdgeCovar[,,2],EdgeCovar[,,3],EdgeCovar[,,4]))
# m=sbm.dialect$estimate() 
# Q=which.max(sbm.dialect$ICL)
# Q
# sbm.dialect$model_parameters[Q] 
# colSums(sbm.dialect$memberships[[Q]]$Z)/n

