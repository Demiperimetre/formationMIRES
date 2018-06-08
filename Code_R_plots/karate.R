library(sand)
data("karate")
plot(karate)
str(karate)
attributes(V(karate))
V(karate)$names
E(karate)

A=as.matrix(get.adjacency(karate))

plot(karate,layout=layout.circle)
plot(karate)
plot(karate,layout=layout.fruchterman.reingold)
plot(karate,layout=layout.random)

plot(karate,layout=layout.auto)

plot(karate,layout=layout_as_tree)

plot(karate,layout=layout_as_star)

pdf("karateRandom1.pdf")
plot(karate,layout=layout.random)
dev.off()


pdf("karateRandom2.pdf")
plot(karate,layout=layout.random)
dev.off()

pdf("karateFR1.pdf")
plot(karate,layout=layout.fruchterman.reingold)
dev.off()

pdf("karateFR2.pdf")
plot(karate,layout=layout.fruchterman.reingold)
dev.off()


pdf("arbredirige.pdf")
plot(graph.tree(10,3,mode="in"))
dev.off()


