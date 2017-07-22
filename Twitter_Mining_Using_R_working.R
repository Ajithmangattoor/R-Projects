#Twitter mining using R

install.packages("twitteR")
install.packages("RCurl")

require("twitteR")
require("RCurl")
#providing consumer key related info

consumer_key <- 'OZ82AkgINNQv92rTmyVLEyiOb'
consumer_secret <- 'iydxH6Aa4X8APGwEdvLx8CsMLZtmPIBZLw3wLCWmgJSF9Uk4KH'
access_token <- '138290204-hki2Ba7JZQkINGUWGBmLrR8Mmv5r2RNDVluX1Xtg'
access_secret <- '6CQ4ueHWJxifNqEpZTQjVBiZWqxLZhHWiTkiyPU0HAqrc'


## Extracting Tweets

## Option 1: retrieve tweets from Twitter
library(twitteR)
library(ROAuth)
## Twitter authentication
setup_twitter_oauth(consumer_key, consumer_secret, access_token,access_secret)

## 3200 is the maximum to retrieve
tweets <- userTimeline("Samsung", n = 3200)
tweets

# convert tweets to a data frame
tweets.df <- twListToDF(tweets)
##tweets.df$text

str(tweets)

# tweet #190
tweets.df[1:20, c("id", "created", "screenName", "replyToSN","favoriteCount", "retweetCount", "longitude", "latitude", "text")]

# print tweet #190 and make text fit for slide width
writeLines(strwrap(tweets.df$text[1:5], 200))

=========================================================
##Text Cleaning
library(tm)

# Just have a look on the textual data
tweets.df$text


# build a corpus, and specify the source to be character vectors

myCorpus <- Corpus(VectorSource(tweets.df$text))
#myCorpus <- tm_map(myCorpus, content_transformer(tolower))



# remove URLs

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
myCorpus[1:20]$content

# remove anything other than English letters or space

removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#OR
#removeNumPunct <- function(myCorpus) gsub("[^[:alpha:][:space:]]*", "", myCorpus)

myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

myCorpus[1:20]$content

#Now we can convert to lowercase

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus[1:20]$content

# remove stopwords

myStopwords <- c(setdiff(stopwords('english'), c("ucc")))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus[1:20]$content

#strip whitespaces
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus[1:20]$content


# keep a copy for stem completion later
myCorpusCopy <- myCorpus
===============================================================================
#Stemming and Stem Completion - eg: argued,arguing is changed to argue

##myCorpus <- tm_map(myCorpus, stemDocument) # stem words
##myCorpus[1:20]$content

writeLines(strwrap(myCorpus[1:17]$content, 6000))

#tm_map(myCorpus,removeWords,c("coldplay"))
#coldplay_tweets_clean <- tm_map(coldplay_tweets_corpus,removeWords,stopwords("english"))

#WordCloud part

require(wordcloud)
wordcloud(myCorpus)
wordcloud(myCorpus, random.order='F', max.words=100, scale=c(3,0.5), colors = rainbow(50))