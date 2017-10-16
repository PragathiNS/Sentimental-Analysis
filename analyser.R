install.packages("stringr", dependencies = TRUE)
install.packages("tm")
install.packages("NLP")
install.packages("Rgraphviz")
install.packages("SnowballC")
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('topicmodels')
install.packages('dplyr')
install.packages("devtools")
library(dplyr)
library(devtools)
library(RColorBrewer)
library(wordcloud)
library(topicmodels)
library(RTextTools)
library(e1071)
library(stringr)
library(sentiment)
library(Rgraphviz)
library(SnowballC)
library(NLP)
library(tm)
  

wordsToRemove = c('get', 'cant', 'can', 'now', 'just', 'will', 'dont', 'ive', 'got', 'much')

GetCorpus <-function(textVector)
{
  doc.corpus <- Corpus(VectorSource(textVector))
  doc.corpus <- tm_map(doc.corpus, tolower)
  doc.corpus <- tm_map(doc.corpus, removeNumbers)
  doc.corpus <- tm_map(doc.corpus, removePunctuation)
  doc.corpus <- tm_map(doc.corpus, removeWords, wordsToRemove)
  doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))
  doc.corpus <- tm_map(doc.corpus, stemDocument, "english")
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  doc.corpus <- tm_map(doc.corpus, removeURL)
  doc.corpus <- tm_map(doc.corpus, PlainTextDocument)
  doc.corpus <-DocumentTermMatrix(doc.corpus,control = list(weighting = function(x) 
    weightTfIdf(x, normalize = FALSE)))
  doc.corpus = as.data.frame(as.matrix(doc.corpus))
  colnames(doc.corpus) = make.names(colnames(doc.corpus))
  return(doc.corpus)
}

tweets <- read.csv("Tweets.csv")
inData <- data.frame(tweets$tweet_id, tweets$airline_sentiment, tweets$airline ,tweets$text)
summary(inData)

# divide tweets in 2 dataframes according to positive or negative sentiment
positive = subset(inData, inData$tweets.airline_sentiment == 'positive')
negative = subset(inData, inData$tweets.airline_sentiment == 'negative')
dim(positive); dim(negative)

words = GetCorpus(negative$tweets.text)
dim(words)

# sum the number of times each word appears in total accross all negative tweets.
freqWords_neg = colSums(words)
freqWords_neg = freqWords_neg[order(freqWords_neg, decreasing = T)]
tail(freqWords_neg)


# analysis of positive tweets
words = GetCorpus(positive$tweets.text)
dim(words)

freqWords_pos = colSums(words)
freqWords_pos = freqWords_pos[order(freqWords_pos, decreasing = T)]
tail(freqWords_pos)

par(mfrow = c(1,2))

wordcloud(freq = as.vector(freqWords_neg), words = names(freqWords_neg),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Reds')[4:9])

wordcloud(freq = as.vector(freqWords_pos), words = names(freqWords_pos),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'BuPu')[4:9])


docs <- GetCorpus(inData$tweets.text)
#docs[[1]]   -------   [1] "virginamerica  dhepburn said"
#strwrap(docs[[10]]) ----- [1] "virginamerica amazing arrived hour early good"

####### to read the contents in the Corpus ##############
### BEFORE TM_MAP this is correct ######
dataframe<-data.frame(text=unlist(sapply(docs, `[`, "content")), stringsAsFactors=F)
#dataframe[1,] ------ prints the first doc

#Texts and label (Just Combining the required Table)
newData <- data.frame(dataframe,tweets$airline_sentiment, tweets$airline)



docs <- tm_map(docs, PlainTextDocument)

nowData <- Corpus(VectorSource(docs))
#Stage the Data
#dtm <- DocumentTermMatrix(docs) 
#inspect(dtm[1:4, 100:2000]) ## docs 1-4, terms 1-2000 frequency

dtm <-DocumentTermMatrix(docs,control = list(weighting = function(x) 
  weightTfIdf(x, normalize = FALSE)))
#inspect(dtm[1:4, 100:2000])
word_frequencies = as.data.frame(as.matrix(dtm))

colnames(word_frequencies) = make.names(colnames(word_frequencies))

#WordCloud
words <- colnames(word_frequencies)
freq <- colSums(word_frequencies)
wordcloud(words, freq,
          min.freq=sort(freq, decreasing=TRUE)[[150]],
          colors=brewer.pal(8, "Dark2"),
          random.color=TRUE) 


words <- names(freq)
wordcloud(words[1:100], freq[1:100])


