#read data from any file
k.data1<-iris
set.seed(760);
cs<-complete.cases(k.data1)
sum(cs)
#k.data1 <-read.csv2(file.choose(), header =TRUE)

#run k-means
k.data1.means <- kmeans(k.data1[,3:4], centers = 3, iter.max = 100, nstart = 10,
                         algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

library(ggplot2)
ggplot(k.data1, aes(Petal.Length, Petal.Width)) + geom_point()
ggplot(k.data1, aes(Petal.Length, Petal.Width, color = k.data1.means$cluster)) + geom_point()


#print centers
k.data1.means$centers 

#print cluster of each element
k.data1.means$cluster

#sum of the square of the distance from each data point to the cluster center (SSE)
k.data1.means$withinss #lower is better
sum(k.data1.means$withinss)
SSE <- k.data1.means$tot.withinss #easier way
SSE
#sum of the squared distance between cluster centers (SSB)
k.data1.means$betweenss #higher is better

# COMPARE TO UNIFORM DISTRIBUTION 

library(ggplot2)
ggplot(k.data1, aes(Petal.Length, Petal.Width)) + geom_point()

# to find min-max
summary(iris)
# 150 points in the same range as iris dataset
x<-runif(150,1.0,6.9)
y<-runif(150,0.1,2.5)
#plot(x,y)
hist(x)
#create a data frame
mydf<-data.frame(x=x,y=y)
#display the points
ggplot(mydf, aes(x,y)) + geom_point()

sse_f <- function(k) {
  x<-runif(150,1.0,6.9)
  y<-runif(150,0.1,2.5)
  #x<-rnorm(150,0,1)
  #y<-rnorm(150,0,0.5)
  mydata<-data.frame(x=x,y=y)
  km<-kmeans(mydata,3)
  return(km$tot.withinss)
}
# 100 iterations 
sses <- sapply(1:100, sse_f)
par(bg = "white")
hist(sses, xlim = c(0, 150))
# our obs
abline(v=SSE,col="red", lwd=4)

M <- mean(sses)
M
s <- sd(sses)
(SSE-M)/s #compare to -1.65
