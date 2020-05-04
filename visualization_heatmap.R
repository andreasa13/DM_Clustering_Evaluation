k.data1<-iris 
set.seed(123)

library(ggplot2)
library(png)

# K- means algorithm (3 centers/clusters) 
km1<-kmeans(k.data1[,3:4], centers = 3, iter.max = 100, nstart = 10,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

#displays plot of clusters and saves it as a png file
ggplot(k.data1, aes(Petal.Length, Petal.Width, color = km1$cluster)) + geom_point()
ggsave("irisClusters.png")

#distances matrix
dis<-dist(k.data1[,3:4]) 
table(km1$cluster)
dis<-data.matrix(dis)

#order by cluster
or_dis<-dis[order(km1$cluster),order(km1$cluster,decreasing = FALSE)]

# display heatmap plot and save it as a png file
png(filename="heatmap.png")
image(1:150,1:150, or_dis, col  = heat.colors(100))
dev.off()

#display two plots in the same screen to compare the results
plot(c(0, 960), c(0, 480), type = "n", xlab = "", ylab = "", axes=FALSE)
rasterImage(readPNG("irisClusters.png", native = TRUE), 0, 0, 480, 480)
rasterImage(readPNG("heatmap.png", native = TRUE), 480, 0, 960, 480)
#heatmap(or_dis)
