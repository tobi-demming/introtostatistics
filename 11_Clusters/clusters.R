library(cluster)

#tree
library(tree)
library(vegan)
data(varechem)
hvwtree<-tree(varechem)
plot(hvwtree)
text(hvwtree)
summary(hvwtree)

#cluster
data(dune)
library(cluster)
clust <- diana(dune, metric = "manhattan", stand = TRUE) #sum of absolute differences
print(clust)
plot(clust)
#Hit return twice


clust2 <- agnes(varespec, metric = "jaccard",method="complete", stand = TRUE) #
print(clust)
cutclust<-cutree(clust2,k=3)
plot(clust)
library(labdsv)
indi<-indval(dune,cutclust,perm=1000)
summary(indi)
#no significant link

data(bryceveg) # returns a vegetation data.frame
data(brycesite)
clust <- cut(brycesite$elev,5,labels=FALSE)
summary(indval(bryceveg,clust))


#from: https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
data("USArrests")      # Loading the data set
df <- scale(USArrests) # Scaling the data

# View the first 3 rows of the data
head(df, n = 3)
kmeans(x, centers, iter.max = 10, nstart = 1)
install.packages("factoextra")
library(factoextra)
# Compute k-means with k = 4
set.seed(123)
km.res <- kmeans(df, 4, nstart = 25)

# Print the results
print(km.res)
aggregate(USArrests, by=list(cluster=km.res$cluster), mean)
dd <- cbind(USArrests, cluster = km.res$cluster)
head(dd)
km.res$cluster
head(km.res$cluster, 4)


#Another example
# example data
a <- as.factor(c("A","A","B","C","D","A","C","A","C","C"))
b <- rep(1:5,2)
c <- as.factor(c("elephant","elephant","cat","dog","cat","elephant",
                 "cat","elephant","dog","dog"))
df <- data.frame(a,b,c)

# Dissimilarity Matrix Calculation

library(cluster)

x <- daisy(df, metric = c("gower"),
           stand = FALSE, type = list())

# Hierarchical Clustering

z <- agnes(x, diss = inherits(x, "dist"), metric = "euclidean",
           stand = FALSE, method = "single", par.method,
           trace.lev = 0, keep.diss = TRUE)

plot(z,  main="plotit", which.plot = 2)