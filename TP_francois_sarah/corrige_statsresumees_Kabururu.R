setwd("C:/Massol/Projets/+ Current projects/MIRES/Cours Juin 2018")

library(igraph) #pour les fonctions sur les graphes
library(vegan) #pour les analyses de redondance
library(MuMIn)

load("Kabururu.Rdata")
Net=t(as.matrix(Kabururu$Net))
EdgeCovar=Kabururu$EdgeCovar
dim(EdgeCovar)
NodeCovar=Kabururu$NodeCovar
dim(NodeCovar)

ntora<-NodeCovar[,1:4]
age<-NodeCovar[,5]
sex<-NodeCovar[,6]
sexfactor<-as.factor(sex)

graph<-graph_from_adjacency_matrix(t(Net), mode = "directed")

group.CH<-which(ntora[,1]==1)
group.MB<-which(ntora[,2]==1)
group.OTH<-which(ntora[,3]==1)
group.TH<-which(ntora[,4]==1)
group<-as.factor(ntora[,1]+2*ntora[,2]+3*ntora[,3]+4*ntora[,4])
ntora<-ntora[,1:3]
plot(graph, mark.groups = list(group.CH,group.MB,group.OTH,group.TH),layout = layout_with_mds)

deg<-degree(graph)
deg.in<-degree(graph,mode="in")
deg.out<-degree(graph,mode="out")
plot(deg.in~deg.out)
sunflowerplot(deg.in,deg.out)

boxplot(deg.in~group)
plot(deg.out~age)
plot(deg.in~age)
boxplot(deg.in~sexfactor)

degmodel<-glm(deg.in~group*sexfactor*age,family=poisson(),na.action=na.fail)
dredge(degmodel)

sample.config<-lapply(1:1000,function(x) sample_degseq(deg.out, deg.in, method = "simple.no.multiple"))

centrality<-centr_eigen(graph, directed = TRUE)$vector
centrality2<-centr_eigen(graph_from_adjacency_matrix(t(as.matrix(get.adjacency(graph))),mode = "directed"), directed = TRUE)$vector
centrality-centrality2

plot(sapply(1:500, function(x) min(alpha.centrality(graph,alpha=x/1000))))
alpha<-alpha.centrality(graph,alpha=0.42)
plot(alpha~deg.in)

centrmodel<-glm(centrality~group*sexfactor*age,family=gaussian(),na.action=na.fail)
dredge(centrmodel)

EB.mod<-cluster_edge_betweenness(graph)
LE.mod<-cluster_leading_eigen(graph)
IM.mod<-cluster_infomap(graph)

plot(EB.mod,graph,layout = layout_with_mds)
plot(LE.mod,graph,layout = layout_with_mds)
plot(IM.mod,graph,layout = layout_with_mds)


compare(IM.mod,LE.mod,method="nmi")
compare(group,LE.mod$mem,method="nmi")
compare(group,EB.mod$mem,method="nmi")
compare(group,IM.mod$mem,method="nmi")

EB.nmi.config<-sapply(1:1000, function(x) compare(group,cluster_edge_betweenness(sample.config[[x]])$mem,method="nmi"))
quantile(EB.nmi.config, probs = c(0.025, 0.975))

graph.svd<-svd(Net)
svd.L <- graph.svd$u
svd.R <- t(graph.svd$v)
svd.S <- diag(graph.svd$d)
svd.Ssqrt <- structure(vapply(svd.S, sqrt, numeric(1)),dim=dim(svd.S))

plot(cumsum(graph.svd$d/sum(graph.svd$d)))
cumsum(graph.svd$d/sum(graph.svd$d))[80]

svd.F <- svd.L %*% svd.Ssqrt
svd.V <- svd.Ssqrt %*% svd.R
plot(sqrt(sapply(2:156,function(x) sum((Net-svd.F[,1:x]%*%svd.V[1:x,])^2))/(156^2)))
sqrt(sum((Net-svd.F[,1:80]%*%svd.V[1:80,])^2/(156^2)))
svd.F <-svd.F[,1:80]
svd.V <-t(svd.V[1:80,])

rda.F.all<-rda(svd.F ~ ntora + age + sex)
vap.F<-varpart(svd.F,ntora, age, sex)
plot(vap.F)
anova(rda(svd.F ~ ntora),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ age), permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ ntora + age),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ ntora + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ age + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ ntora + age + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ ntora + Condition(age) + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ Condition(ntora) + age + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ Condition(ntora) + Condition(age) + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ ntora + Condition(age)),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ ntora + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ Condition(ntora) + age),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ age + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ Condition(ntora) + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.F ~ Condition(age) + sex),permutations=how(nperm=9999))$Pr


rda.V.all<-rda(svd.V ~ ntora + age + sex)
vap.V<-varpart(svd.V,ntora, age, sex)
plot(vap.V)
anova(rda(svd.V ~ ntora),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ age),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ ntora + age),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ ntora + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ age + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ ntora + age + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ ntora + Condition(age) + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ Condition(ntora) + age + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ Condition(ntora) + Condition(age) + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ ntora + Condition(age)),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ ntora + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ Condition(ntora) + age),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ age + Condition(sex)),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ Condition(ntora) + sex),permutations=how(nperm=9999))$Pr
anova(rda(svd.V ~ Condition(age) + sex),permutations=how(nperm=9999))$Pr

analysis.function<-function(gr,nb,var1,var2,var3){
adj<-t(as.matrix(gr[,]))
gr.svd<-svd(adj)
svd.L <- gr.svd$u
svd.R <- t(gr.svd$v)
svd.S <- diag(gr.svd$d)
svd.Ssqrt <- structure(vapply(svd.S, sqrt, numeric(1)),dim=dim(svd.S))
svd.F <- svd.L %*% svd.Ssqrt
svd.V <- svd.Ssqrt %*% svd.R
svd.F <-svd.F[,1:nb]
svd.V <-t(svd.V[1:nb,])
c(varpart(svd.F,var1,var2,var3)$part$indfract[["Adj.R.square"]],varpart(svd.V,var1,var2,var3)$part$indfract[["Adj.R.square"]])
}

analysis.function(graph,80,ntora,age,sex)
varpart.config<-sapply(1:1000,function(x) analysis.function(sample.config[[x]],80,ntora,age,sex))
quantiles.fraction<-apply(varpart.config,1,function(x) quantile(x,c(0.025,0.5,0.975)))

