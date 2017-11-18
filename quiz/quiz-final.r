# This file is designed to work through homework 3 https://docs.google.com/document/d/15pwjdBmrQvQiQV320GrPijokb0kDjutqtFDQKBCdyzk/edit.  

# We are building a classifier to predict whether an answer to a quiz show question is correct, based on information about the question and the guess. The data is here: https://www.kaggle.com/c/digging-into-data-hw3-question-answering2/data

# Remove all previous objects
rm(list=ls(all=TRUE))

# Load the tidyverse
library(tidyverse)

# Load e1071 for the SVM (Support Vector Machine) functions
library(e1071)

# Load the library nnet, which will allow us to run multinom, which is a form of logistic regression different than glm. 
library(nnet)

# Load cwhmisc and two dependencies, lattice and grid, though I'm not sure what these are for
library(cwhmisc) # not sure what these next three are for
library(lattice)
library(grid)
library(rpart)

# This is for testing the accuracy of our logistic regression, if we decide not to use the classmatch function provided by prof. sayed
library(pscl) 

# Define a function called "record_performance" that accepts five variable arguments model_type, df, name, model and test.  We'll use this to measure the performance of our models we're building -- svm, decision tree, logisitc regression, naive bayes. Note that the if-else function allows us to change the "type" for prediction so that the prediction works.   

record_performance <- function( model_type, df, name, model, test) {
  # if-else statement to change pred variable depending on model type 
  if (model_type=="svm"){
    pred <- predict(model, test)
  }
  else if (model_type=="tree"){
    pred <- predict(model, test,type="class")
  }
  else if (model_type=="logit"){
    pred <- predict(model, test,type="class") #we may need to store this as type="response"
  }
  # create a table with the prediction from the model and the actual value in the test data
  table <- table(pred = pred, true=test$corr)
  # calculate the "score" the degree to which the values in our prediction agree (classAgreement) with values in the test data.  Then add them to a table we create with multiple model predictions.
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(table)$diag)))
  return(df)
}

# Here are examples of how I'll test SVM, LOGISTIC REGRESSION, DECISION TREE
results <- record_performance("svm", results, "body_score", svm(corr ~ body_score, data=trainset), testset)
results <- record_performance('logit', results , "body_score", glm(corr ~ body_score, data=trainset, family=binomial), testset) # May need to change this to say multinom instead of glm to work
results <- record_performance("tree", results , "body_score", rpart(corr ~ body_score, data=trainset,method="class"), testset)

# Load in the data see record-layout.txt for details https://www.kaggle.com/c/digging-into-data-hw3-question-answering2/data

# Train is the dataset we'll work with to build and perfect our models
train <- read_csv("csv/qb.train.csv", col_names= TRUE)

# Test is the dataset that we'll test our answers against.  It doesn't contain a corr column, which tells us if our answers are correct.  Those are contained in "guess", so we could do some matching.
test <- read_csv("csv/qb.test.csv", col_names= TRUE)
guess <- read_csv("csv/qb.guess.csv", col_names= TRUE)

# Create a copy of the training data called full, which we'll use to do feature engineering -- transforming some of the columns to use to build a better model.   
full <- train

# First, create a column that counts the number of characters in the questions. Longer questions are those that have more information in them - people waited longer before buzzing in.
full$obs_len <- apply(full, 1, function(x) {nchar(x['text'])})

# Creates a column that standardizes the values in obs_len along a normalized scale (I think it's SD from the mean)
full$scale_len <- scale(full$obs_len)

# Creates a column that standardizes the values in body_score along a normalized scale (I think it's SD from the mean)
full$scale_score <- scale(full$body_score)

# Creates a column that checks whether the page name has something in parens that also appears in the body of the question text.
paren_match <- function(page, text) {
  start <- cpos(page, "(")
  end <- cpos(page, ")")
  if (!is.na(start) && !is.na(end)) {
    search <- substring(page, start + 1, end - 1)
    return(grepl(tolower(search), tolower(text), fixed=TRUE))
  } else {
    return(FALSE)
  }
}
full$paren_match <- apply(full, 1, function(x) {paren_match(x['page'], x['text'])})

# Normalizes the inlinks into a page by standardizes the values in inlinks along a normalized scale (I think it's SD from the mean)
full$log_links <- scale(log(as.numeric(full$inlinks) + 1))

# Convert the variable we're trying to predict -- whether question is correct -- to 1 and 0 so we can use them in the analysis. 
full$corr<-factor(full$corr, levels = c('True','False'), labels = c(1,0))

# Now let's split our "full" data set into a training slice and a testing slice (I know, even though we have a separate test set). 1/5 in testset and 4/5 in trainset.

index <- 1:nrow(full)
testindex <- sample(index, trunc(length(index)/5))
testset <- full[testindex,]
trainset <- full[-testindex,]

# For now, remove full, guess, test, train

# QUESTION 2: (30 points) Build the best classifier you can with the given data, documenting the choices that you make.  Try using logistic regression, SVM (multiple kernels), and decision trees.  Create a table with your accuracy with each of these methods. Look at where you’re making mistakes.  Can you see any patterns?

# LOGISTIC REGRESSION 

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer. 
lr.mfc_baseline <- sum(testset$corr == 0) / nrow(testset)
lr.results <- data.frame(model=c("MFC"), score=c(lr.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
lr.results <- record_performance("logit", lr.results, "body_score", multinom(corr ~ body_score, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "tournaments", multinom(corr ~ tournaments, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "answer_type", multinom(corr ~ answer_type, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "links", multinom(corr ~ inlinks, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "obs_len", multinom(corr ~ obs_len, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "scale_len", multinom(corr ~ scale_len, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "scale_score", multinom(corr ~ scale_score, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "paren_match", multinom(corr ~ paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "log_links", multinom(corr ~ log_links, data=trainset),testset)

# Now let's do two way and see what we get
lr.results <- record_performance("logit", lr.results, "score+len", multinom(corr ~ obs_len + body_score, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "paren+len", multinom(corr ~ obs_len + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+paren_match", multinom(corr ~ scale_score + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+len+paren_match", multinom(corr ~ scale_len + scale_score + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+len+links+paren_match", multinom(corr ~ scale_len + scale_score + log_links + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+links+paren_match+tournaments+answertype", multinom(corr ~ scale_len + scale_score + paren_match + tournaments + answer_type, data=trainset),testset)
lr.results
results

# Let's doublecheck the best model using a Confusion Matrix, which seems like the way I really want to be checking the accuracy of the model.  

# SVM

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer. 
svm.mfc_baseline <- sum(testset$corr == 0) / nrow(testset)
svm.results <- data.frame(model=c("MFC"), score=c(svm.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
svm.results <- record_performance("svm", svm.results, "body_score", svm(corr ~ body_score, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "tournaments", svm(corr ~ tournaments, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "answer_type", svm(corr ~ answer_type, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "links", svm(corr ~ inlinks, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "obs_len", svm(corr ~ obs_len, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "scale_len", svm(corr ~ scale_len, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "scale_score", svm(corr ~ scale_score, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "paren_match", svm(corr ~ paren_match, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "log_links", svm(corr ~ log_links, data=trainset),testset)

# Now let's do two way and see what we get
svm.results <- record_performance("svm", svm.results, "score+len", svm(corr ~ obs_len + body_score, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "paren+len", svm(corr ~ obs_len + paren_match, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "score+paren_match", svm(corr ~ scale_score + paren_match, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "score+len+paren_match", svm(corr ~ scale_len + scale_score + paren_match, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "score+len+links+paren_match", svm(corr ~ scale_len + scale_score + log_links + paren_match, data=trainset),testset)
svm.results <- record_performance("svm", svm.results, "score+links+paren_match+tournaments+answertype", svm(corr ~ scale_len + scale_score + paren_match + tournaments + answer_type, data=trainset),testset)
svm.results

# My best SVM result then was score+len at .815
svm.best <- svm(corr ~ obs_len + body_score, data=trainset)

# Confirm it using a confusion matrix
library(caret)
library(heuristica)
pdata<-predict(svm.best, newdata=testset, type="class")
confusionMatrix(pdata,testset$corr)

# DECISION TREE

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer. 
tree.mfc_baseline <- sum(testset$corr == 0) / nrow(testset)
tree.results <- data.frame(model=c("MFC"), score=c(tree.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
tree.results <- record_performance("tree", tree.results, "body_score", rpart(corr ~ body_score, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "tournaments", rpart(corr ~ tournaments, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "answer_type", rpart(corr ~ answer_type, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "links", rpart(corr ~ inlinks, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "obs_len", rpart(corr ~ obs_len, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "scale_len", rpart(corr ~ scale_len, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "scale_score", rpart(corr ~ scale_score, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "paren_match", rpart(corr ~ paren_match, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "log_links", rpart(corr ~ log_links, data=trainset,method="class"),testset)

# Now let's do two way and see what we get
tree.results <- record_performance("tree", tree.results, "score+len", rpart(corr ~ obs_len + body_score, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "paren+len", rpart(corr ~ obs_len + paren_match, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "score+paren_match", rpart(corr ~ scale_score + paren_match, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "score+len+paren_match", rpart(corr ~ scale_len + scale_score + paren_match, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "score+len+links+paren_match", rpart(corr ~ scale_len + scale_score + log_links + paren_match, data=trainset,method="class"),testset)
tree.results <- record_performance("tree", tree.results, "score+links+paren_match+tournaments+answertype", rpart(corr ~ scale_len + scale_score + paren_match + tournaments + answer_type, data=trainset,method="class"),testset)
tree.results

#So in conclusion, svm produced the best model is an SVM with obs_len + body_score guessing right in four out of five cases. 
svm.best <- svm(corr ~ obs_len + body_score, data=trainset)

# Question 3 (40 points) Find additional information you can use to improve predictions.  Be creative.  Look for features you can extract from the data that you have.   NOTE: To get credit for this, you need to have an idea and evaluate it.  You will get full credit for a well-thought out feature that doesn’t improve performance.Create a plot that explains why this information is useful for making predictions (e.g. create a facet graph showing the distribution for correct and incorrect).  Turn this in as “plot.pdf”. How much does this feature improve your classification?

# Let's create some additional features.  

# Counts the number of characters in the length of page that is correct answer.
full$page_len <- apply(full, 1, function(x) {nchar(x['page'])})

# Now that we've made additional features, let's split our "full" data set into a training slice and a testing slice (I know, even though we have a separate test set). 1/5 in testset and 4/5 in trainset.

index <- 1:nrow(full)
testindex <- sample(index, trunc(length(index)/5))
testset <- full[testindex,]
trainset <- full[-testindex,]

# Now let's add our new feature to our best model from previous question and run that.

svm.best <- svm(corr ~ obs_len + body_score, data=trainset)
svm.best.page_len <- svm(corr ~ obs_len + body_score + page_len, data=trainset)

# Confirm the accuracy of best using a confusion matrix - accuracy of .813
library(caret)
library(heuristica)
svm.best.cm<-predict(svm.best, newdata=testset, type="class")
confusionMatrix(svm.best.cm,testset$corr)

# Now let's test the accuracy using a confusion matrix
library(caret)
library(heuristica)
svm.best.page_len.cm<-predict(svm.best.page_len, newdata=testset, type="class")
confusionMatrix(svm.best.page_len.cm,testset$corr)

# And we see that we get an improvement to accuracy of .816

# Now let's plot it. 

TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(881, 266, 31, 437)
df <- data.frame(TClass, PClass, Y)

library(ggplot2)
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  labs(title="original model") +
  ylab("Prediction") +
  xlab("Actual") +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#ffffff", high = "#999999") +
  theme_bw() + theme(legend.position = "none") + 
  scale_x_discrete(breaks=c("0","1"),
                   labels=c("False", "True")) + 
  scale_y_discrete(breaks=c("0","1"),
                   labels=c("False", "True"))


rm(testindex)
rm(testset)
rm(trainset)

# KAGGLE PLOT (30 points) Challenge: Build a classifier that best predicts correct answers in this dataset.  Upload your predictions to Kaggle (note that you must use your UMD e-mail to get access to this competition).  If your UMD username is JSMITH, use JSMITH_DID as your username on Kaggle.  You are welcome to use any additional data you care to use so long as they are not from quiz bowl questions. Provide your final score and username

# Here's our best model
svm.best.page_len <- svm(corr ~ obs_len + body_score + page_len, data=trainset)

# We're going to use the model we built and use it to predict values in the original test data set.

# First we need to take that test set, and do all the feature engineering we did originally, plus the new page_len feature engineering. 

# Store test as full   
full <- test

# First, create a column that counts the number of characters in the questions. Longer questions are those that have more information in them - people waited longer before buzzing in.
full$obs_len <- apply(full, 1, function(x) {nchar(x['text'])})

# Creates a column that standardizes the values in obs_len along a normalized scale (I think it's SD from the mean)
full$scale_len <- scale(full$obs_len)

# Creates a column that standardizes the values in body_score along a normalized scale (I think it's SD from the mean)
full$scale_score <- scale(full$body_score)

# Creates a column that checks whether the page name has something in parens that also appears in the body of the question text.
paren_match <- function(page, text) {
  start <- cpos(page, "(")
  end <- cpos(page, ")")
  if (!is.na(start) && !is.na(end)) {
    search <- substring(page, start + 1, end - 1)
    return(grepl(tolower(search), tolower(text), fixed=TRUE))
  } else {
    return(FALSE)
  }
}
full$paren_match <- apply(full, 1, function(x) {paren_match(x['page'], x['text'])})

# Normalizes the inlinks into a page by standardizes the values in inlinks along a normalized scale (I think it's SD from the mean)
full$log_links <- scale(log(as.numeric(full$inlinks) + 1))

# Counts the number of characters in the length of page that is correct answer.
full$page_len <- apply(full, 1, function(x) {nchar(x['page'])})

# Now let's predict answers
svm.best.page_len <- svm(corr ~ obs_len + body_score + page_len, data=trainset)
full$corr <- predict(svm.best.page_len, newdata=full, type="class")

# Subset it to only get the row and prediction column
kagglesubmit <- full %>%
  select(row,corr)

# Write it to csv
write_csv(kagglesubmit, "sm-kaggle.csv")


