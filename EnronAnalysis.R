# Predictive Analytics - In the courtroom
# Load it up. We're going to use predictive coding to help manually label some of the documents to train models.
# Apply models to much larger set of documents.
emails <- read.csv("energy_bids.csv", stringsAsFactors = F)
str(emails)
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]
library(tm)
corpus <- Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
# Look at first email again
strwrap(corpus[[1]])

# Bag of words processing
corpus <- Corpus(VectorSource(unlist(corpus))) # Some update makes me do this every time before I do some other processing. Bullshit.
dtm <- DocumentTermMatrix(corpus)
dtm
dtm <- removeSparseTerms(dtm, 0.97)
labeledTerms <- as.data.frame(as.matrix(dtm))
labeledTerms$responsive <- emails$responsive
str(labeledTerms)

# Building Models
library(caTools)
set.seed(144)
spl <- sample.split(labeledTerms$responsive, 0.7)
train <- subset(labeledTerms, spl == T)
test <- subset(labeledTerms, spl == F)
library(rpart)
library(rpart.plot)
emailCART <- rpart(responsive ~ ., data = train, method = "class")
prp(emailCART)
pred <- predict(emailCART, newdata = test)
pred[1:10,]
# We're looking for the predictive probability of the document being responsive. Whatever that means.
pred.prob <- pred[,2]
table(test$responsive, pred.prob >= 0.5)
table(test$responsive)
# Compare the accuracy between each test. The original and the cleaned.
## The ROC Curve
library(ROCR)
predROCR <- prediction(pred.prob, test$responsive)
perfROCR <- performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize = T)
performance(predROCR, "auc")@y.values

# ta-daaaa


