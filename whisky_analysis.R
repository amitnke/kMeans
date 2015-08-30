D <- read.table("whiskies.txt",header = T, sep = ",")
#Data is from Scotland Whisky
class(D)
str(D)
#Distillary information along with Bunch of testing score in range of 0-4.
#Also location information of location of distillary
#Each record is one distiallry information
#what the whisky that have similar taste. Also we will try to find the correlation between 
#location and whisky taste
tastes = D[,3:14]
str(tastes)
#Devide the datastet into 4 cluster and 5 starting point 
kmfit = kmeans(tastes, centers = 4, nstart = 5)
class(kmfit)
str(kmfit)
# Cluster - Number of cluster to which dataset belongs
# centers - k Center(finding k cluster)
# totss - The sum of total of squre(Distance)
# withinss - Vector of within-cluster sum of squares, one component per cluster.
# tot.withinss(distortion) - Total within-cluster sum of squares, i.e. sum(withinss). The sum of all withinss
# betweenss - The between-cluster sum of squares, i.e. totss-tot.withinss.
# Size - The number of points in each cluster.
# iter - Number of iteration to converge the cluster
# ifault - integer: indicator of a possible algorithm problem – for experts.
kmfit$centers
kmfit$size
#Plots every combination of two dimensions
plot(tastes)
plot(kmfit)
kmfit$cluster==3
D[kmfit$cluster==3,]
kmfit2 = kmeans(tastes, centers = 4, nstart = 10)
kmfit2$size
#Add another column to store cluster information
D$cluster = kmfit$cluster
str(D)
write.csv(D,"whiskies_post_analsysis.csv")
kmeans.wss.k <- function(D, k){
        km <- kmeans(D, centers = k, nstart = 5)
        return (km$tot.withinss)
}
kmeans.wss.k(tastes, 4)
kmeans.wss.k(tastes, 5)
kmeans.wss.k(tastes, 7)
kmeans.wss.k(tastes, 8)
kmeans.wss.k(tastes, 10)
kmeans.wss <- function(D, maxK){
        wss <- (nrow(D) - 1)*sum(apply(D,2,var))
        wss[2:maxK] <- sapply(2:maxK, kmeans.wss.k, D = D, simplify = T)
        return (wss)
}
maxK = 15
wss <- kmeans.wss(tastes, 15)
wss
plot(1:maxK, wss, type = "b", xlab = "Number of Clusters", ylab = "Within Group Sum of Square (Distortion)", col = "blue")

tmp = kmeans(tastes, center =3)