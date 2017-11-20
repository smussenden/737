# HOMEWORK 3
# INST737
# 11/18/17


# This .r file is designed to work through homework 3 https://docs.google.com/document/d/15pwjdBmrQvQiQV320GrPijokb0kDjutqtFDQKBCdyzk/edit.  

# We are building a classifier to predict whether an answer to a quiz show question is correct, based on information about the question and the guess. The data is here: https://www.kaggle.com/c/digging-into-data-hw3-question-answering2/data

# First, let's set up the environment and load needed plackages

# Remove all previous objects
rm(list=ls(all=TRUE))

# Load the tidyverse
library(tidyverse)
library(stringr)

# Load e1071 for the SVM (Support Vector Machine) functions
library(e1071)

# Load the library nnet, which will allow us to run multinom, which is a form of logistic regression different than glm. 
library(nnet)

# Load cwhmisc and two dependencies.
library(cwhmisc) 
library(lattice)
library(grid)
library(rpart)

# Librarys for confustion matrix for confirming svm and lr models
library(caret)
library(heuristica)

# For plotting decision trees
library(rpart.plot)

# This is for testing the accuracy of our logistic regression
library(pscl) 

# Load in the data see record-layout.txt for details https://www.kaggle.com/c/digging-into-data-hw3-question-answering2/data

# Train is the dataset we'll work with to build and perfect our models
train <- read_csv("csv/qb.train.csv", col_names= TRUE)

# Test is the dataset that we'll test our answers against.  It doesn't contain a corr column, which tells us if our answers are correct.  Those are contained in "guess", so we could do some matching to check.
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

# Now we're ready to build models.

# Q2 (30 points) Build the best classifier you can with the given data, documenting the choices that you make.  

# PART A. Try using logistic regression, SVM (multiple kernels), and decision trees.  Create a table with your accuracy with each of these methods.

# Before we build models, define a function called "record_performance" that accepts five variable arguments model_type, df, name, model and test.  We'll use this to measure the performance of our models we're building -- svm, decision tree, logisitc regression, naive bayes. Note that the if-else function allows us to change the "type" for prediction so that the prediction works.   

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

# LOGISTIC REGRESSION MODELS

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer. 
lr.mfc_baseline <- sum(testset$corr == 0) / nrow(testset)
lr.results <- data.frame(model=c("MFC"), score=c(lr.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
lr.results <- record_performance("logit", lr.results, "body_score", multinom(corr ~ body_score, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "answer_type", multinom(corr ~ answer_type, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "links", multinom(corr ~ inlinks, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "obs_len", multinom(corr ~ obs_len, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "scale_len", multinom(corr ~ scale_len, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "scale_score", multinom(corr ~ scale_score, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "paren_match", multinom(corr ~ paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "log_links", multinom(corr ~ log_links, data=trainset),testset)

# Now let's do multi logistic models and see what we get.
lr.results <- record_performance("logit", lr.results, "score+len", multinom(corr ~ obs_len + body_score, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "paren+len", multinom(corr ~ obs_len + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+paren_match", multinom(corr ~ scale_score + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+len+paren_match", multinom(corr ~ scale_len + scale_score + paren_match, data=trainset),testset)
lr.results <- record_performance("logit", lr.results, "score+len+links+paren_match", multinom(corr ~ scale_len + scale_score + log_links + paren_match, data=trainset),testset)
lr.results

# The best logistic regression model, which factors in the body_score -- the degree to which the question text matches the wikipedia page for a given answer -- the length of the question -- how far the host got in reading it before being interrupted) -- and the parentheses match -- whether or not the part of a title of wikipedia page used as a guess contained in parentheses also appeared in the body of the question -- had a .717 accuracy score. 

# SVM (Support Vector Machine) Models

# Get the most frequent baseline, which divides number of false answers by number of total observations to use as a base measurement. This is how well humans did on guessing answers to questions. We need to do better than this with our predictive model. So any model we build needs to do a better job than the humans at getting the right answer. 
svm.mfc_baseline <- sum(testset$corr == 0) / nrow(testset)
svm.results <- data.frame(model=c("MFC"), score=c(svm.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
svm.results <- record_performance("svm", svm.results, "body_score", svm(corr ~ body_score, data=trainset),testset)
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
svm.results

# The best SVM model, which factors in the body_score -- the degree to which the question text matches the wikipedia page for a given answer -- and the length of the question -- how far the host got in reading it before being interrupted) -- had a .814 accuracy score. Better than logistic regression so far. 


# Decision Tree models

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


# The best decision tree model, which factors in the body_score -- the degree to which the question text matches the wikipedia page for a given answer -- and the length of the question -- how far the host got in reading it before being interrupted) -- had a .7795 accuracy score. Not as good as my best SVM.  

# This is a plot of my best decision tree model
prp(rpart(corr ~ obs_len + body_score, data=trainset,method="class"))

#So in conclusion, svm produced the best model is an SVM with obs_len + body_score guessing right in four out of five cases - .814 accuracy score. 

svm(corr ~ obs_len + body_score, data=trainset)

# PART B. Look at where you’re making mistakes.  Can you see any patterns?

# There are several patterns evident from testing multiple models.  In most cases, single variable models are not powerful enough to score well. And several variables had essentially no predictive power. This included the number of inlinks (though it did add something when the log was taken), observation length (length of question) and other factors.  But it was a mistake to leave some of them out in trying multiple-variable models. In combination with other variables, they did add predictive power. 

# Q3 (40 points) Find additional information you can use to improve predictions.  Be creative.  Look for features you can extract from the data that you have.   NOTE: To get credit for this, you need to have an idea and evaluate it.  You will get full credit for a well-thought out feature that doesn’t improve performance.

# Theory: there may be some relationship between the length of an answer and whether it indicates a correct guess. The idea here is that this quiz show is hard, and the organizers are likely to choose things that are difficult to guess, and more complex concepts and book titles may be longer. 

# Counts the number of characters in the length of page title that is the guess.
full$page_len <- apply(full, 1, function(x) {nchar(x['page'])})

# Normalizes the page_len by standardizing the values in inlinks along a normalized scale (I think it's SD from the mean)
full$pagelen_log <- scale(log(as.numeric(full$page_len) + 1))

# Now that we have new features we've engineered, rebuild my trainset and testset
# Now let's split our "full" data set into a training slice and a testing slice (I know, even though we have a separate test set). 1/5 in testset and 4/5 in trainset.

index <- 1:nrow(full)
testindex <- sample(index, trunc(length(index)/5))
testset <- full[testindex,]
trainset <- full[-testindex,]

# Let's test our new models and compare it to our old models 
svm.new.mfc_baseline <- sum(testset$corr == 0) / nrow(testset)
svm.new.results <- data.frame(model=c("MFC"), score=c(svm.mfc_baseline))

# Now let's add single variable logsitic regression models to the list. 
svm.new.results <- record_performance("svm", svm.new.results, "score+len", svm(corr ~ obs_len + body_score, data=trainset),testset)
svm.new.results <- record_performance("svm", svm.new.results, "score+plen", svm(corr ~ body_score + page_len, data=trainset),testset)
svm.new.results <- record_performance("svm", svm.new.results, "qlen+plen", svm(corr ~ obs_len + page_len, data=trainset),testset)
svm.new.results <- record_performance("svm", svm.new.results, "score+qlen+plen", svm(corr ~ obs_len + body_score + page_len, data=trainset),testset)
svm.new.results <- record_performance("svm", svm.new.results, "score+qlen+plenlog", svm(corr ~ obs_len + body_score + pagelen_log, data=trainset),testset)
svm.new.results <- record_performance("svm", svm.new.results, "score+qlen+plenlog+paren", svm(corr ~ obs_len + body_score + pagelen_log + paren_match, data=trainset),testset)
svm.new.results

# With the new slice of test and training data, the score+len baseline is .815.  Adding the page length reduces it slightly to .8111, taking the log of page length is a slight improvement, to .812, but still under the baseline. 

# To get a closer look at the performance of our top two models, let's use our models to predict values in our test set, then build confusion matrixes to evaluate the performance.

# Score and Question Length
svm.score_qlen <- svm(corr ~ obs_len + body_score, data=trainset)
pred.svm.score_qlen <- predict(svm.score_qlen, newdata=testset, type="class")
confusionMatrix(pred.svm.score_qlen,testset$corr)

# Score and Question Length and Page Length
svm.score_qlen_plenlog <- svm(corr ~ obs_len + body_score + pagelen_log, data=trainset)
pred.svm.score_qlen_plenlog <- predict(svm.score_qlen_plenlog, newdata=testset, type="class")
confusionMatrix(pred.svm.score_qlen_plenlog,testset$corr)


# PART A. Create a plot that explains why this information is useful for making predictions (e.g. create a facet graph showing the distribution for correct and incorrect).  Turn this in as “plot.pdf”.

# Add Score and Question Length Confusion Matrix values to dataframe
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(854, 268, 31, 462)
df <- data.frame(TClass, PClass, Y)

# Plot Score and Question Length Confusion Matrix values
ggplot(data =  df, mapping = aes(x = TClass, y = PClass)) +
  labs(title="Score+QLength: Accuracy .8149", subtitle="Few false negatives, many false positives") +
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

# Add Score and Question Length and Page Length Confusion Matrix values to dataframe
TClass <- factor(c(0, 0, 1, 1))
PClass <- factor(c(0, 1, 0, 1))
Y      <- c(862, 280, 23, 450)
df2 <- data.frame(TClass, PClass, Y)

# Plot Score and Question Length and Page Length Confusion Matrix values
ggplot(data =  df2, mapping = aes(x = TClass, y = PClass)) +
  labs(title="Score+QLength+PLength: Accuracy .8124", subtitle="More false positives, fewer false negatives") +
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

# PART B. How much does this feature improve your classification?

# On the whole, the addition of this feature did very little to improve the overall accuracy of the model compared to the model developed in question 2. As measured by the confusion matricies, the accuracy dropped from .8149 to .8124 with the addition to the new feature.  The new model predicted as true slightly fewer questions that were actually true (450 to 462).  And it had more false positives (280 to 268).  But it had fewer false negatives (32 to 30).  And it correctly predicted as false more questions that were actually false (862 to 854)

# QUESTION 4: KAGGLE PLOT (30 points) Challenge: Build a classifier that best predicts correct answers in this dataset.  Upload your predictions to Kaggle (note that you must use your UMD e-mail to get access to this competition).  If your UMD username is JSMITH, use JSMITH_DID as your username on Kaggle.  You are welcome to use any additional data you care to use so long as they are not from quiz bowl questions. Provide your final score and username

# The best classifier I could come up with is a SVM using obs_len and body_score, with an accuracy rate of .8149.
svm.score_qlen <- svm(corr ~ obs_len + body_score, data=trainset)

# We're going to use that model and predict values in the original test data set.First we need to take that test set, and do all the feature engineering we did originally, plus the new page_len feature engineering. 

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
svm.score_qlen <- svm(corr ~ obs_len + body_score, data=trainset)
full$corr <- predict(svm.score_qlen, newdata=full, type="class")

# Subset it to only get the row and prediction column
kagglesubmit <- full %>%
  select(row,corr)

# Write it to csv
write_csv(kagglesubmit, "sm-kaggle.csv")

# PART A. My Kaggle Username is smussend (smussend@umd.edu) and my top score is 0.78947

# PART B. Create an error analysis of your final classifier.  Turn this in as “error.pdf”.  An error analysis must contain real examples of your data, not just an error matrix.  

# In order to evaluate errors in my final classifier, first I need to bind the correct answers in the guess file back to the test set, and then write a function to show false positives and false negatives.

errorcheck <- full %>%
  bind_cols(full,guess) %>%
  select(row:corr,corr2) %>%
  rename(guess = "corr", actual = "corr2")

# Convert guess values from 1s and 0s to True and False
errorcheck$guess<-factor(errorcheck$guess, levels = c(1,0), labels = c('True','False'))

# Create a new column with comparisons of my guess to actual data by concatenating. 
errorcheck <- errorcheck %>%
  mutate(check = str_c(guess,"-",actual))

# Then, create a table showing how my model fared on real data showing correct guesses ("False-False" or "True-True"), False Positives ("True-False") and False Negatives ("False-True") 

errortable <- errorcheck %>%
  group_by(check) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

# There were no false negatives. 18 percent were false positives.  The rest were correct.  Let's examine false positives, first by doing a confusion matrix, which repeats some of the information above.  

guess$corr2<-factor(guess$corr, levels = c('True','False'), labels = c(1,0))
pred.svm.score_qlen <- predict(svm.score_qlen, newdata=full, type="class")
confusionMatrix(pred.svm.score_qlen,guess$corr2)

# Let's examine false negatives a little more closely by looking at a few examples.  The model did really well with extremely high body scores.  In fact, anything with a body score over 100 it predicted with perfect accuracy -- it decided the answer was correct, and it was. Intuitively, this makes sense.  But it also predicted as True several answers with extrmely low body scores which, in hindsight, seems like a poor choice. For example, for row 1605, it guessed Amy Poehler as the correct answer, despite a body score of 2, the second lowest in the data. In fact, in this data, all pages with a body score under 100 were false, and all over 100 were true.  So I could have just built a model that said that, and it would have worked.  

# -30-