
library(sand)
data(karate)

plot(karate)


vertex_attr(karate)
karate=set_vertex_attr(karate,"age",value=round(rnorm(34,30,8)))
karate$age
vertex_attr(karate)
plot(karate)


plot(karate)

Akar = as.matrix(get.adjacency(karate))
Akar

write.table(Akar,"Karate_A.csv")

listkar=get.edgelist(karate)

write.table(listkar,"Karate_edge.csv")

write.table(Reduce(cbind,get.vertex.attribute(karate)),"Karate_ego.csv")

