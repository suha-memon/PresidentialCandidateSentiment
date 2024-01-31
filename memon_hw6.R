# Homework 6

setwd("/Users/suhamemon/Dropbox/DATA401/Week6/Homework")


library(tm)
library(SnowballC)
library(wordcloud)
library(stringr)

pal <- brewer.pal(6,"Dark2")

##########################################################################
# 1. Create a word cloud with the ANES 2016 open ended question about what 
#    respondents LIKE about Donald Trump.

trump_likes <- read.csv("redacted_trump_2016.csv")
names(trump_likes)
colnames(trump_likes) <- c("ID", "likes")

trump_likes <- Corpus(VectorSource(trump_likes$likes)) # data comes from a vector, so use VectorSource

trump_likes <- tm_map(trump_likes, tolower)
trump_likes <- tm_map(trump_likes, removeWords, stopwords('english'))
trump_likes <- tm_map(trump_likes, removePunctuation)
trump_likes <- tm_map(trump_likes, removeWords, c("like", "think", stopwords('english')))

trump_likesTDM <- TermDocumentMatrix(trump_likes) # corpus back to term document matrix
trump_likesMT <- as.matrix(trump_likesTDM) # Back to matrix
trump_likesMT <- sort(rowSums(trump_likesMT), decreasing = TRUE) # sort by row sums

trump_likesDF <- data.frame(word = names(trump_likesMT), freq = trump_likesMT) # transform into dataframe

head(trump_likesDF)

wordcloud(trump_likesDF$word, freq= trump_likesDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)

##########################################################################
# 2. Create a word cloud with the ANES 2016 open ended question about what 
#    respondents DISLIKE about Donald Trump.

trump_dislikes <- read.csv("redacted_trump_2016_dislikes.csv")
colnames(trump_dislikes) <- c("ID", "dislikes")

trump_dislikes <- Corpus(VectorSource(trump_dislikes$dislikes)) # data comes from a vector, so use VectorSource

trump_dislikes <- tm_map(trump_dislikes, tolower)
trump_dislikes <- tm_map(trump_dislikes, removeWords, stopwords('english'))
trump_dislikes <- tm_map(trump_dislikes, removePunctuation)
trump_dislikes <- tm_map(trump_dislikes, removeWords, c("hes", "like", "think", stopwords('english')))

trump_dislikesTDM <- TermDocumentMatrix(trump_dislikes) # corpus back to term document matrix
trump_dislikesMT <- as.matrix(trump_dislikesTDM) # Back to matrix
trump_dislikesMT <- sort(rowSums(trump_dislikesMT), decreasing = TRUE) # sort by row sums

trump_dislikesDF <- data.frame(word = names(trump_dislikesMT), freq = trump_dislikesMT) # transform into dataframe

head(trump_dislikesDF)

wordcloud(trump_dislikesDF$word, freq= trump_dislikesDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)

##########################################################################
# 3. Create a word cloud with the ANES 2016 open ended question about what 
#    respondents LIKE about Hilary Clinton.

clinton_likes <- read.csv("redacted_2016_clinton_likes.csv")
colnames(clinton_likes) <- c("ID", "likes")

clinton_likes <- Corpus(VectorSource(clinton_likes$likes)) # data comes from a vector, so use VectorSource

clinton_likes <- tm_map(clinton_likes, tolower)
clinton_likes <- tm_map(clinton_likes, removeWords, stopwords('english'))
clinton_likes <- tm_map(clinton_likes, removePunctuation)
clinton_likes <- tm_map(clinton_likes, removeWords, c("like", "think", stopwords('english')))

clinton_likesTDM <- TermDocumentMatrix(clinton_likes) # corpus back to term document matrix
clinton_likesMT <- as.matrix(clinton_likesTDM) # Back to matrix
clinton_likesMT <- sort(rowSums(clinton_likesMT), decreasing = TRUE) # sort by row sums

clinton_likesDF <- data.frame(word = names(clinton_likesMT), freq = clinton_likesMT) # transform into dataframe

head(clinton_likesDF)

wordcloud(clinton_likesDF$word, freq= clinton_likesDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)


##########################################################################
# 4. Create a word cloud with the ANES 2016 open ended question about what 
#    respondents DISLIKE about Hilary Clinton.

clinton_dislikes <- read.csv("redacted_clinton_2016_dislikes.csv")
names(clinton_dislikes)
colnames(clinton_dislikes) <- c("ID", "dislikes")

clinton_dislikes <- Corpus(VectorSource(clinton_dislikes$dislikes)) # data comes from a vector, so use VectorSource

clinton_dislikes <- tm_map(clinton_dislikes, tolower)
clinton_dislikes <- tm_map(clinton_dislikes, removeWords, stopwords('english'))
clinton_dislikes <- tm_map(clinton_dislikes, removePunctuation)
clinton_dislikes <- tm_map(clinton_dislikes, removeWords, c("shes", "like", "think", "dont", stopwords('english')))

clinton_dislikesTDM <- TermDocumentMatrix(clinton_dislikes) # corpus back to term document matrix
clinton_dislikesMT <- as.matrix(clinton_dislikesTDM) # Back to matrix
clinton_dislikesMT <- sort(rowSums(clinton_dislikesMT), decreasing = TRUE) # sort by row sums

clinton_dislikesDF <- data.frame(word = names(clinton_dislikesMT), freq = clinton_dislikesMT) # transform into dataframe

head(clinton_dislikesDF)

wordcloud(clinton_dislikesDF$word, freq= clinton_dislikesDF$freq, max.words = 100, 
          random.order = FALSE, colors = pal)
