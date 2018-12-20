#Load relevant datasets
library(datasets)
library(dplyr)

#Create an IRIS Database
db = iris

#Basic Data Exploration
summary(db)

plot(db$Sepal.Length,db$Species)
plot(db$Sepal.Width,db$Species)
plot(db$Petal.Length,db$Species)
plot(db$Petal.Width,db$Species)

#Define a k-means clustering function, which takes database and k value as inputs
kmeans_clust <- function(db, k)
{

#Assign random cluster value to each point  
len = nrow(db)
set.seed(1)

db$cluster_i = sample(1:k,len,replace=TRUE)
db$cluster_f = rep(NA,nrow(db))

#Initialize other relevant variables
nequal=1
cm = data.frame(matrix(data=NA, nrow=k, ncol=4))
cmn = data.frame(matrix(data=NA, nrow=k, ncol=5))
dist = rep(NA,k)

#Loop until the initial and final clusters suggested are equal  
while(nequal)
{ 
  #Calculate the centroid of the clusters
  for(i in 1:k)
  {
    cm[i,] = t(as.data.frame(colMeans(db[db$cluster_i==i,1:4])))
  }
  
  #If a cluster gets dropped fill its value with a very high number, so that no observation gets assigned to it
  cm[is.na(cm)] = 100000
  
  #Calculate the distance of each point from all the different centroids
  for(j in 1:nrow(db))
  {
    for(l in 1:k)
    {
      dist[l] = sqrt(sum((db[j,1:4]-cm[l,])^2))
    }
   db[j,7]=which(dist==min(dist)) 
  }
  
  #Stopping parameter- If the new clusters are same as the earlier clusters stop
  nequal=sum(db$cluster_i!=db$cluster_f)
  
  #Update the clusters to reflect new values
  db$cluster_i=db$cluster_f
}

#Calculate the within cluster variation
cmn[,5] = (data.frame(matrix(data=seq(1,k), nrow=k, ncol=1)))

for(i in 1:k)
{
cmn[i,1:4] = t(as.data.frame(colMeans(db[db$cluster_i==i,1:4])))
}

print(cmn)
cmn[is.na(cmn)] = 100000

#Merge centroid with database
colnames(cmn) = c("p1","p2","p3","p4","cluster_i")
db = merge(db, cmn, by="cluster_i")

print(head(db))

#Calculate the sum of squared values
db$ss = (db[,2]-db[,8])^2+(db[,3]-db[,9])^2+(db[,4]-db[,10])^2+(db[,5]-db[,11])^2

wcv_ss = print(tapply(db$ss, db$cluster_i, sum))

#Analyze the distribution of values across different clusters
print(table(db$cluster_i))

return(sum(wcv_ss))
}

wcv_ss = rep(NA,10)

#Iterate for k values from 1 to 10, and then store the data in a data frame
for(i in 1:10)
{
  wcv_ss[i] = kmeans_clust(db,i)
}

wcv_ss
"
 [1] 681.37060 152.34795  78.85567  57.22847  53.00295  57.25601  49.41806  44.10234  42.70558
[10]  43.73621
"

plot(1:10,wcv_ss,xlab = "Number of clusters",ylab = "Within Cluster SS",pch=16, cex=1, main = "Within cluster SS variation by number of clusters")
