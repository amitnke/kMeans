#Goal - Clustering based on Age and Incom
setwd("~/R/Module3/edureka-insurance-dataset-and-script")
ins<-read.table("clust.csv",head=T,sep=",")
head(ins,10)
ins1<-ins[,c(2,5)]
head(ins1)
km <- kmeans(ins1, 4)
km$cluster
km$centers
km$totss
km$withinss
km$tot.withinss
km$betweenss
km$size


ins2<-cbind(ins1,km$cluster)
head(ins2)
plot(ins2$Income,ins2$Age,col=km$cluster)

write.csv(ins2,"Output.csv")
#Elbow method
Clust <- read.csv("Clust.csv")
work1 <- Clust[,c(2,5)]
#define wss(within sum of squares)
wss <- (nrow(work1) - 1)*sum(apply(work1, 2, var))    
for(i in 2:15) wss[i] <- sum(kmeans(work1, centers = i)$withinss)
plot(1:15, wss, type = "b", xlab="Number of Clusters", ylab = "within groups sum of squares", col = "blue")
