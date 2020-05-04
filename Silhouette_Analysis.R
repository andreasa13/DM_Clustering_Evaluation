#Set the seed for reproducibility
set.seed(760)
#dataset declaration
k.data1<-iris
head(k.data1)

# K-means clustering
k.data1.means<-kmeans(k.data1[,3:4], centers = 3, iter.max = 100, nstart = 10,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

# create a plot of the clusters
library(ggplot2)
names(k.data1)
ggplot(k.data1, aes(Petal.Length, Petal.Width, color = k.data1.means$cluster)) + geom_point() 

# sihlouette computation
library(cluster)
ss <- silhouette(k.data1.means$cluster, suppressWarnings(dist(k.data1[,3:4])))
# mean(ss[,3])
sumss <-summary(ss)
#sihlouette mean
as.numeric(sumss$si.summary[4])

# USE SILHOUETTE TO FIND THE NUMBER OF CLUSTERS 

kmeans_ss <- function(k) {
  k.data1.means <- kmeans(k.data1[,3:4], centers = k, iter.max = 100, nstart = 10,
                          algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
  ss <- silhouette(k.data1.means$cluster, suppressWarnings(dist(k.data1[,3:4])))
  sumss <-summary(ss)
  
  return (as.numeric(sumss$si.summary[4]))
}

# Set maximum cluster 
max_k <-20
# Run algorithm over a range of k 
set.seed(760)
wss <- sapply(2:max_k, kmeans_ss)
#put observations in a data frame
elbow <-data.frame(k = 2:max_k, silhouette = wss)

# Plot the graph with ggplot
ggplot(elbow, aes(x = k, y = silhouette)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

