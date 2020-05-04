source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R") 
#set working directory
setwd("/home/andreas/Downloads/R-lab (1)/R-lab")
#dataset declaration 
dataset<-read.csv("SacramentoCrime.csv")  

#to keep the same results after multi-iterations
set.seed(760)

#distances matrix
distances<-suppressWarnings(dist(dataset)) #suppress warning about NA values

#customize the plot
op = par(bg = "grey70") # background color 
cols = hsv(c(0.2, 0.57, 0.95), 1, 1, 0.8) 

# SINGLE METHOD
clusters<-hclust(distances,method = "single")
# display the plot
A2Rplot(clusters, k = 3, show.labels = FALSE, boxes = TRUE, col.up = "black", col.down = cols, main = "Criminal activity")
# compute the CPCC coefficient
coph <- cophenetic(clusters)
cor(distances, coph)

# COMPLETE METHOD
clusters<-hclust(distances,method = "complete")
#display the plot
A2Rplot(clusters, k = 3, show.labels = FALSE, boxes = TRUE, col.up = "black", col.down = cols, main = "Criminal activity")
# compute the CPCC coefficient
coph <- cophenetic(clusters)
cor(distances, coph)

