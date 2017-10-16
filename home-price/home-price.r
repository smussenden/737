## We have two data sets.  One is our "training data", which we'll use to build a model to predict the price of houses in 2013, which we'll check the accuracy of by updating our "test data" to predict accuracy. 

## To start, let's load the tidyverse, and some of our ggplot themes and data. 

library('tidyverse')
library(ggthemes)
library(scales)

## Read in the data. 

test <- read_csv("csv/house_test.csv", col_names = TRUE)
train <- read_csv("csv/house_train.csv", col_names = TRUE)

##Predict 2013 home prices using state information only.  Answer these questions using all of the training data available.

## Now let's create a linear model to look at how well correlated a home's price is to the state in which it's in.  We're using linear regeression because the dependent variable -- the thing we're trying to predict -- is a continuous variable -- house prices.  If we were going the other way -- if the dependent variable was state -- we'd use logisitic regression, because state is a categorical variable.  
statepricemodel <- lm(train$price2013 ~ train$state)

## To view a summary of the coefficients (including the estimate for mean house price for each state, the standard error and the t value), we use summary.

summary(statepricemodel)

##We get a multiple R-squared of .2868, meaning a week positive correlation between state and home prices, with a tiny p value, indicating a significant result. That means state explains 29 percent of variation in home prices.   This isn't a particularly great model, but whatever.  Let's answer the questions. 

##Question A: What is the intercept?  What does it correspond to?  

##Answer A: The (Intercept) is Alaska, or AK. This appears to have been automatically assigned by the lm function, because it's the first state alphabetically The Estimate value for Alaska corresponds to the mean price of the homes in a given  and it's the mean price of homes in Alaska in the data set. All other estimates use the Alaska mean as a baseline. For example, the estimate for Alabama (AL) is -140,892.  When subtracted from Alaska's mean, that gives us Alabama's mean value 140,837.  

##Question B: How do you get this information from your regression?
##Answer B: I used the summary function to to view the coefficients table, including the estimate for each state, the standard error, the t value and the p value.  And just to be safe, I confirmed that the Estimate given for Alaska was, indeed, the mean, by calculating the mean for each state separately. 

stateaverage <- train %>% 
  group_by(state) %>%
  summarise(mean = mean(price2013)) %>%
  arrange(state)
View(stateaverage)

stateaverage <- stateaverage %>%
  mutate(CalculatedIntercept=mean-281730)
View(stateaverage)

## Question C: Based on your regression coefficients, what states have the most and least expensive average homes?
## Answer C: West Virginia (WV) has the least exepnsive average home. Washington, D.C. has the most.  But if you consider that D.C. isn't actually a "state," then it's California.

## Question D: How do you get this information from your regression?
## Answer D: I obtained this by sorting the estimates, ascending and descending, and then adjusting the values for the highest and lowest values by the intercept. 

## Question E: What is the average price of homes in those states?
## Answer E: In West Virginia, it's $98,423.08.  In Washington, D.C. it's $514,288.89. And in California, the highest "state," it's $512,602.90

## Question F: How do you get this information from your regression?
## Answer F: I obtained this by sorting the estimates, ascending and descending, and then adjusting the values for the highest and lowest values by the intercept. 

## Now that we've trained our model on our training data, let's take the predicted values for each state and apply them to our test data.  We're going to take our "stateaverage" dataframe, and join it with out test data. 

predict.lm(statepricemodel, newdata = test)

## I'm getting an error when I do this.  STILL NEED TO FIGURE OUT WHY I"m GETTING AN ERROR WHEN I RUN THIS.

## (10 points) Predict 2013 home prices from state and county information. What US counties have the highest and lowest regression coefficients? Why?

##First I tried to test using a lm with two coefficients, but it just seemed to create a table of county, on top of a table of states. And it runs into the problem that when you do this, it groups together counties with the same name, even if they're in different states.
statecopricemodel<-lm(train$price2013 ~ train$state+train$county) 
statecopricemodel<-summary(statecopricemodel)$coefficients
View(statecopricemodel)


##Now let's try transforming the data to combine state and county into a single variable (column) so we get unique counties. 

statecountytrain <- train %>%
  unite(statecounty, state, county, remove=FALSE, sep="-")
View(statecountytrain)

##Now let's run the model, with the new statecounty column and the state column.

statecopricemodel<-lm(statecountytrain$price2013 ~ statecountytrain$state+statecountytrain$statecounty)
statecopricemodel<-summary(statecopricemodel)$coefficients
View(statecopricemodel)

##We get a multiple R-squared of .5671, meaning a medium-strong positive correlation between state, county and home prices, with a tiny p value, indicating a significant result. That means state+county explains more than half of the variation in home prices.     Let's answer the questions. 

## Question A: Which counties have the highest and lowest regression coefficients? Why?
## Answer A: Lynchburg, in rural Virginia, has the lowest.  San Mateo county California, located in the San Francisco area, has the highest, which makes sense, since it's one of the most expensive areas to live in the United States. 

##(30 points) Challenge: Build a regressor that best predicts average home values in this dataset.  Upload your predictions to Kaggle (IMPORTANT: you must use your UMD e-mail to get access to this competition).  If your UMD username is JSMITH, use JSMITH_DID as your username on Kaggle.  You are welcome to use any additional data you care to use so long as they are from 2007 or earlier.  You must describe any additional data you use in the writeup.  All students must make a submission to Kaggle; additional points available to those who do best (WARNING: make sure you submit something before the deadline, as you cannot use late days on the competition). Describe what you did to build the best predictor possible Give your best Kaggle score Give your Kaggle username

##The Evaluation metric is RMSE.  This rewards you for getting close to the true answer (and penalizes being very far away).  

##Your answer submission file should have only two columns: id and price2013.  The price2013 should reflect the prediction based on the information from that id in the house_test.csv file.