###########Text Clustering using K-Means##########

###Download required libraries
library(tm)
#library(SnowballC)

setwd("C:\\PERSONAL\\edureka\\Module-3\\R")

###Read Input file
comments<-read.csv("Comments_Search.csv",head=T)

head(comments)

###Covert into text corpus
comments1<-Corpus(VectorSource(comments$Feedback))
comments1
inspect(comments1)

## Convert to Lower Case
comments2<- tm_map(comments1, tolower)
comments2
comments2[[1]]
comments2[[83]]

## Remove Stopwords
comments3<- tm_map(comments2, removeWords, stopwords("english"))
comments3[[83]]

## Remove Punctuations
comments4<- tm_map(comments3, removePunctuation)
comments4[[83]]
comments4[[81]]


## Remove Numbers
comments5<- tm_map(comments4, removeNumbers)
comments5[[1]]
comments5[[81]]

## Eliminating Extra White Spaces
comments6<- tm_map(comments5, stripWhitespace)
comments6[[81]]

## create a term document matrix
dtm <- DocumentTermMatrix(comments6)
dtm
inspect(dtm[1:5,1:10])

findFreqTerms(dtm, 10)
findFreqTerms(dtm, 5)

## do tfxidf
dtm_tfxidf <- weightTfIdf(dtm)
inspect(dtm_tfxidf[1:5, 1:100])

## do document clustering

### k-means (this uses euclidean distance)
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)

### don't forget to normalize the vectors so Euclidean makes sense
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)

m_norm1<-m_norm[-28,]

### cluster into 4 clusters
cl <- kmeans(m_norm1, 10)
cl

cl$cluster
cl$size

comments_out<-cbind(as.character(comments[-28,]),cl$cluster)

write.csv(comments_out,"Output_Search.csv")

### show clusters using the first 2 principal components
#plot(prcomp(m_norm)$x, col=cl$cl)

findFreqTerms(dtm[cl$cluster==1], 1)
findFreqTerms(dtm[cl$cluster==2], 6)
findFreqTerms(dtm[cl$cluster==3], 3)
findFreqTerms(dtm[cl$cluster==4], 4)



