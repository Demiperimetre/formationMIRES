library(igraph)

#construction du reseau
edges = read.table("Kabururu_edges.csv",colClasses = "character")

G =graph_from_edgelist(as.matrix(edges),directed = T)

#representation
plot(G)


#chargement des covariables
ego = read.table("Kabururu_ego.csv")
head(ego)

#covariables sur le graphe
plot(G,vertex.color=ego$dialect)
plot(G,vertex.color=ego$sex)

#degres
degout = degree(G,mode="out")
degin = degree(G,mode="in")

hist(degout)
hist(degin)

#lien degre entrant degre sortant
plot(degout,degin)

#lien degre avec dialecte
boxplot(degout~ego$dialect)
boxplot(degin~ego$dialect)

