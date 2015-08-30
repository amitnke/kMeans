setwd("~/R")
library("twitteR", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
library(tm)

# Setup a Twitter application, and provide required keys here.
# Instructions: http://geoffjentry.hexdump.org/twitteR.pdf

api_key <- "x3JPfnDiFx61mURPDlKPKzPe8"
api_secret <- "xbo1ZDSGcHP4qYp24nK4slgFsxvml5TYLQyMOZy1ciTw5uQn5a"
access_token <- "115878452-h21LayhtJbXd4BS0y9G3316zdM5J09dDkpfvNQxq"
access_token_secret <- "fsvLh86IRFUEKhGUKZgDmyV3fJWtP4vfbSI9O8RlmcHri"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


fdaTweets = userTimeline("FDArecalls", n=500)

tweets = fdaTweets
str(tweets)

n = length(tweets)
tweets[1:3]
df <- do.call("rbind", lapply(tweets, as.data.frame))
dim(df)
str(df)
df$text

# ---------------------------------------
# Cleaning the text corpus
# ---------------------------------------

myCorpus = Corpus(VectorSource(df$text))
#myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords('english'), "available", "via", "obama", "modi", "sonia", "loksabha")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

dictCorpus <- myCorpus
df$text[1]
inspect(myCorpus[1])
#install.packages(c("SnowballC", "RWeka", "rJava", "RWekajars"))
myCorpus <- tm_map(myCorpus, stemDocument)
inspect(myCorpus[1:3])
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

inspect(myCorpus[1:3])
dicCorpus <- myCorpus

# ---------------------------------------
# Convert text corpus into document-term matrix, and weigh it via TFIDF scores

myDtm <- DocumentTermMatrix(myCorpus)
dim(myDtm)
inspect(myDtm[20:40, 20:30])
findFreqTerms(myDtm, lowfreq=10)

dtm_tfxidf <- weightTfIdf(myDtm)
inspect(dtm_tfxidf[11:10,1:15])

# run K-means on normalized data w/ Euclidean distance
m <- as.matrix(dtm_tfxidf)
rownames(m) <- 1:nrow(m)
norm_eucl <- function(m) m/apply(m, MARGIN=1, FUN=function(x) sum(x^2)^.5)
m_norm <- norm_eucl(m)
dim(m_norm)


# ---------------------------------------
# Perform K-means, and draw an elbow plot
# ---------------------------------------
kmeans.wss.k = function(D, k) {
        km = kmeans(D,k, iter.max=20, nstart=5)
        return(km$tot.withinss)
}

kmeans.wss = function(D, maxK) {
        wss = (nrow(D)-1)*sum(apply(D, 2, var))
        wss[2:maxK] = sapply(2:maxK, kmeans.wss.k, D=D, simplify=T)
        return (wss)
}

maxK=15
wss = kmeans.wss(m_norm, maxK)
plot(1:maxK, wss, type="b", xlab = "Number of Clusters", ylab="Within Group Sum of Squares (Distortion)", col="blue")


cl <- kmeans(m_norm, 10)
table(cl$cluster)

findFreqTerms(myDtm[cl$cluster==1,], 5)
findFreqTerms(myDtm[cl$cluster==5,], 2)

# Write out clustered documents
write.csv(data.frame(df$text, cl$cluster), "fda_clusters.csv")

