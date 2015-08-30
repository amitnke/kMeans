Iris <- iris
Iris.features = Iris
Iris.features$Species <- NULL
View(Iris.features)
//3 is the number of group or cluster. 
?kmeans
results <- kmeans(Iris.features, 3)
results
results$size
results$cluster
str(Iris)
plot(Iris[c("Petal.Length", "Petal.Width")], col = results$cluster)
plot(Iris[c("Petal.Length", "Petal.Width")], col = Iris$Species)
plot(Iris[c("Sepal.Length", "Sepal.Width")], col = Iris$Species)
plot(Iris[c("Sepal.Length", "Sepal.Width")], col = results$cluster)
