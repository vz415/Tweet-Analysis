# Predictive Analytics - In the courtroom
# Load it up. We're going to use predictive coding to help manually label some of the documents to train models.
# Apply models to much larger set of documents.
emails <- read.csv("energy_bids.csv", stringsAsFactors = F)
str(emails)
