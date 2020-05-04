#dataset declaration
k.data1<-iris
#to keep the same results after multi-iterations
set.seed(760) 

# K-means clustering
km1<-kmeans(k.data1[,3:4], centers = 3, iter.max = 100, nstart = 10,
            algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

# km1<-kmeans(k.data1[,1:2], centers = 3, iter.max = 100, nstart = 10,
#             algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)

#incidence matrix
a<-matrix(0,nrow = 150, ncol = 150) #create a zero matrix
for(i in 1:150){
  for(j in 1:150){
    if(km1$cluster[i]==km1$cluster[j])
      
      # 2 obs in the same cluster
      a[i,j]<-1 
  }
}

# distances matrix
dis<-dist(k.data1[,3:4])
#dis<-dist(k.data1[,1:2])

dis<-data.matrix(dis) #convert to matrix

#Convert matrixes to vectors 
av<-as.vector(a)
disv<-as.vector(dis)
disvr<--disv # distance to similarity

#execute the correlation function
cor(av,disvr) 