# setwd("~/Documents/presentations/Workshops/topicModeling/")
library(tm)
library(topicmodels)

eccoTexts.df <- read.csv("eccoTexts.csv", sep = ",", stringsAsFactors = FALSE)

stopwords <- scan("stopwords.txt", what="character", sep="\n")

eccoCorpus <- Corpus(DataframeSource(eccoTexts.df))

eccoCorpus <- tm_map(eccoCorpus, content_transformer(tolower))
eccoCorpus <- tm_map(eccoCorpus, removeWords, stopwords)
eccoCorpus <- tm_map(eccoCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
eccoCorpus <- tm_map(eccoCorpus, removeNumbers)
eccoCorpus <- tm_map(eccoCorpus, stemDocument, language = "en")
eccoCorpus <- tm_map(eccoCorpus, stripWhitespace)

DocTermMatrix <- DocumentTermMatrix(eccoCorpus)

topicNo <- 20

eccoTM <- LDA(DocTermMatrix, topicNo, method = "Gibbs", control = list(iter=500, verbose=50))

terms(eccoTM, 10)

tmResult <- posterior(eccoTM)

theta <- tmResult$topics

rowSums(theta)

topFive <- terms(eccoTM, 5)
topicNames <- apply(topFive, 2, paste, collapse=" ")

# medTopic <- 10 
topicThreshold <- .2
selectedDocumentIndexes <- which(theta[, medTopic] >= topicThreshold)

topicProportions <- colSums(theta) / nDocs(DocTermMatrix)
names(topicProportions) <- topicNames
sortedTopics <- sort(topicProportions, decreasing = TRUE)
plot(sortedTopics[1:5], type = "h", main = "Topics in ECCO", xlab = "Topics")
axis(1,1:5, labels = names(sortedTopics[1:5]))