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

# Document term matrix shows the number of times a word appears in a document. Cool!
corpus <- Corpus(VectorSource(unlist(corpus)))
frequencies <- DocumentTermMatrix(corpus)
findFreqTerms(frequencies, lowfreq = 20)
# Remove terms that don't occur often
sparse <- removeSparseTerms(frequencies, .995)
tweetsSparse <- as.data.frame(as.matrix(sparse))
# Make sure that all of the variable names don't have a number at a beginning. Important!!!
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$negative <- tweets$negative

library("caTools")
set.seed(123)
split <- sample.split(tweetsSparse$negative, SplitRatio = 0.7)
trainSparse <- subset(tweetsSparse, split == T)
testSparse <- subset(tweetsSparse, split == F)

# Now, we're going to build a predictive model
library("rpart")
library("rpart.plot")
tweetCart <- rpart(negative ~ ., data=trainSparse, method = "class") # Method is "class" since we have a classification problem
# plot using prp function
prp(tweetCart)
predictCart <- predict(tweetCart, newdata = testSparse, type = "class")
table(testSparse$negative, predictCart)
table(testSparse$negative)
# Compare the tables to get specificity and sensitivity
# Now, let's see how a random forest model performs
library("randomForest")
set.seed(123)
tweetRF <- randomForest(negative ~ ., data = trainSparse)
predictRF <- predict(tweetRF, newdata = testSparse)
table(testSparse$negative, predictRF)

# Lexicon = relation btwn ddifferent words... what watson uses
# How watson works
# Question analysis: What is the question looking for? Watson starts by trying to find the Lexical Answer Type (LAT) of the question
# LAT is the word or noun in the qustion that specifies the type of answer
# Can replace the LAT with the answer to get correcto. If we know the LAT, we know what to look for.
# To enhance teh question analysis steps, watson also performs "relation detection" to find relationships among words, 
# and decomposition to split teh question into different clues.
## STep 2
# create hypothesis generations with the different potential targets.
## Step 3
# compute CI for each of the hypotheses. Combines a large number of different methods to 
# First, lightweigh scoring algorithms to prune down large set of hypotheses. What is the likelihood that a candiate answer is an 
# instance of the LAT? If likelihood is not very high, throw away the hypothesis. Watson lets about 100 candidates pass.
## Scoring Analytics
# Watson needs to gather supporting evidence for each candidate answer. Passage search.
# Retrieve passages that contain the hypothesis text. Let's see what happens when we search for our hypotheses on Google.
# Basically, using the google algorithm and results to come up with the most promising answer... Typical.
## Step 4: Final Merging and Ranking
# Selecting the single best supported hypothesis. First need to merge similar answers (honest abe and abe lincoln).
# Rank hypoetheses and estimate an overall confidence for each.
## Ranking and confidence
# Training data is a set of historical questions and answers. Each of the scoring algorithms is an independent variable. 
# Use logistic regression to predict whether or not a candidate answer is correct, using the scores.
# If the confidence for the best answer is high enough, Watson buzzes in to answer the question.


