k.data1<-iris
#sizek<-NROW(k.data1)

#to keep the same results after multi-iterations
set.seed(760)

# 3 classes/species so 3 centers/clusters 
km1<-kmeans(k.data1[,3:4], centers = 3, iter.max = 100, nstart = 10,
            algorithm = c("Hartigan-Wong", "Lloyd", "Forgy","MacQueen"), trace=FALSE)
km1$cluster
table(km1$cluster)

#initialization 
a<-0
b<-0
c<-0

# Same cluster - Same Class
for(i in 1:150){
  
  if( (km1$cluster[i] == 3 && k.data1[i,5] == "setosa") ||
      (km1$cluster[i] == 2 && k.data1[i,5] == "versicolor") || 
      (km1$cluster[i] == 1 && k.data1[i,5] == "virginica") ) {
    
    a<-a+1
    
  }
}
# Same cluster - Different Class
for(i in 1:150){
    
    if( (km1$cluster[i] == 3 && k.data1[i,5] != "setosa") ||
        (km1$cluster[i] == 2 && k.data1[i,5] != "versicolor") || 
        (km1$cluster[i] == 1 && k.data1[i,5] != "virginica") ) {
      
        b<-b+1

      }
}
# Different cluster - Same Class
for(i in 1:150){
  
  if( (km1$cluster[i] != 3 && k.data1[i,5] == "setosa") ||
      (km1$cluster[i] != 2 && k.data1[i,5] == "versicolor") || 
      (km1$cluster[i] != 1 && k.data1[i,5] == "virginica") ) {
    
      c<-c+1
  }
}
#print a,b and c
a
b
c
#jaccard coefficient
jaccard<-a/(a+b+c)
jaccard
