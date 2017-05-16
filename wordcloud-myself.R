#load required packages
library(ggplot2)
library(tm)
library(NLP)
library(RColorBrewer)
library(wordcloud)
library(syuzhet)

#get the data from whatsapp chat
texts <- readLines("comments on Friedman book.txt")

#let us create the corpus
docs <- Corpus(VectorSource(texts))

#clean our chat data
trans <- content_transformer(function(x,pattern) gsub(pattern," ",x))
docs <- tm_map(docs,trans,"/")
docs <- tm_map(docs,trans,"@")
docs <- tm_map(docs,trans,"\\|")
docs <- tm_map(docs,content_transformer(tolower))
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removeWords,stopwords("english"))
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,stripWhitespace)


#create the document term matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing = TRUE)
#data frame
d <- data.frame(word = names(v), freq = v)
head(d,10)

#generate the wordcloud
set.seed(1056)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35,
          colors=brewer.pal(8,"Dark2"))

#fetch sentiment words from texts
sentiment <- get_nrc_sentiment (texts)
text <- cbind(texts,sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x=sentiment, y = count)) +
  geom_bar(aes(fill = sentiment),stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") +ggtitle("Total Sentiment Score")
