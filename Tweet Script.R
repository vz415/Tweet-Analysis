# Load it up
tweets <- read.csv("tweets.csv", stringsAsFactors = F)
str(tweets)
tweets$negative <- as.factor(tweets$Avg <= 1)
table(tweets$negative)
library("tm") # Stands for "text mighty"!
library("SnowballC") # Helps use the tm package
# Need to convert tweets to corpus in order to perform preprocessing. corpus is a collection of documents.
corpus <- Corpus(VectorSource(tweets$Tweet))
corpus <- tm_map(corpus, tolower) # Make all letters lowercase.
corpus <- tm_map(corpus, removePunctuation) # Does what it says
# Now, we want to remove a list of stop words. tm can do that.
corpus <- tm_map(corpus, removeWords, c("apple", stopwords("english"))) # Making fewer, more significant, words.
corpus <- tm_map(corpus, stemDocument) # Gets the stems from the main words in the document.
# Cool, definitely see how I can use this later.

