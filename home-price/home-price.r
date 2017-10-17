# We have two data sets.  One is our "training data", which we'll use to build a model to predict the price of houses in 2013, which we'll check the accuracy of by updating our "test data" to predict accuracy. 

# To start, let's load the tidyverse, and some of our ggplot themes. 

library('tidyverse')
library(ggthemes)
library(scales)

# Read in the data. 

test <- read_csv("csv/house_test.csv", col_names = TRUE)
train <- read_csv("csv/house_train.csv", col_names = TRUE)

# Question 2: Predict 2013 home prices using state information only.  Answer these questions using all of the training data available.

# First, let's create a linear model to determine how much of given home's price can be explained by the state in which it's in.  We're using linear regeression because the dependent variable -- the thing we're trying to predict, house prices -- is a continuous variable.  If we went the other way -- if the dependent variable was state -- we'd use logisitic regression, because state is a categorical variable.  

statepricemodel <- lm(train$price2013 ~ train$state)

# To view a summary of the coefficients for each state (including the estimate for mean house price for each state benchmarked against the intercept, the standard error, the t value and significance), we use summary.

summary(statepricemodel)

# We get a multiple R-squared of .2868, with a tiny p value, indicating a significant result. That means state explains 29 percent of variation in home prices.   This isn't a particularly great model.  Let's answer the specific questions. 

# Question 2A: What is the intercept?  What does it correspond to?  

# Answer 2A: The (Intercept) is Alaska, or AK. This appears to have been automatically assigned by the lm function, because it's the first state alphabetically.  The estimate value for the intercept corresponds to the mean price of the homes in Alaska. All other estimates use the Alaska mean as a baseline. For example, the estimate for Alabama (AL) is -140,892.  When subtracted from Alaska's mean, that gives us Alabama's mean value of $140,837.  

# Question 2B: How do you get this information from your regression?
# Answer 2B: I used the summary function to view the coefficients table, including the estimate for each state, the standard error, the t value and the significance.  And just to be safe, I confirmed that the estimate given for Alaska was, indeed, the mean, by calculating the mean for each state separately, using the following function: 

stateaverage <- train %>% 
  group_by(state) %>%
  summarise(mean = mean(price2013)) %>%
  arrange(state)
View(stateaverage)

stateaverage <- stateaverage %>%
  mutate(CalculatedIntercept=mean-281730)
View(stateaverage)

# Question 2C: Based on your regression coefficients, what states have the most and least expensive average homes?
# Answer 2C: West Virginia (WV) has the least expensive average home. Washington, D.C. has the most.  But if you consider that D.C. isn't actually a "state," then it's California.

# Question 2D: How do you get this information from your regression?
# Answer 2D: I obtained this by sorting the estimates, ascending and descending, and then adjusting the values for the highest and lowest values by the intercept. 

# Question 2E: What is the average price of homes in those states?
# Answer 2E: In West Virginia, it's $98,423.08.  In Washington, D.C. it's $514,288.89. And in California, the highest "state," it's $512,602.90.

# Question 2F: How do you get this information from your regression?
# Answer 2F: I obtained this by sorting the estimates, ascending and descending, and then adjusting the values for the highest and lowest values by the intercept. 

# DO I NEED TO DO THIS?
# Now that we've trained our model on our training data, let's take the predicted values for each state and apply them to our test data.  We're going to take our "stateaverage" dataframe, and join it with out test data. 
predict.lm(statepricemodel, newdata = test)
# I'm getting an error when I do this.  STILL NEED TO FIGURE OUT WHY I"m GETTING AN ERROR WHEN I RUN THIS.

# Question 3a: Predict 2013 home prices from state and county information. What US counties have the highest and lowest regression coefficients? Why?

# To get around the fact that, in several cases, a given county name exists in multiple states, let's first combine state and county into a single variable, so that we get unique counties. 

statecountytrain <- train %>%
  unite(statecounty, state, county, remove=FALSE, sep="-")
View(statecountytrain)

# Let's also calculate the mean home prices in each county, so we have something to check our model's estimates. From this, we see that the county with lowest mean price in the dataset is Edgecombe, North Carolina at $51,700.  The county with the highest mean price is San Mateo, California at $1.1 million.  This makes sense. San Mateo is one of the most expensive areas of the country, in Silicon Valley.  Edgecombe is in a rural, economically depressed part of North Carolina.

stcountymeans <- statecountytrain %>% 
  group_by(statecounty) %>%
  summarise(mean = mean(price2013)) %>%
  arrange(mean)
View(stcountymeans)

# Let's create the multiple linear model, with the concatenated statecounty column and the state column. First, let's run it so that state is the first value.     

statecountypricemodel<-lm(statecountytrain$price2013 ~ statecountytrain$state + statecountytrain$statecounty)
statecountypricemodel<-summary(statecountypricemodel)$coefficients
View(statecountypricemodel)

# The intercept is 316,954.55.  The highest estimate is for San Mateo, at 923,754.55.  Adding this to the intercept give us $1.24 million, which is higher than the mean in our benchmark test ($1.1 million).  The lowest estimate is for Lynchburg, Virginia -188,266.67. Adding this value to the intercept, we get $128,688 -- which is higher than the $77,600 in our mean table. And unlike our means table, Edgecombe isn't the low value here.  Importantly, state values show up in this model.  We get a multiple R squared of .567, with a tiny p value. 

# Now let's run the model again, with the statecounty column first, and the state column second. 

countystatepricemodel<-lm(statecountytrain$price2013 ~ statecountytrain$statecounty + statecountytrain$state)
countystatepricemodel<-summary(countystatepricemodel)$coefficients
View(countystatepricemodel)

# The intercept is 316,954.55. The highest estimate is for San Mateo, at 814,800. The low estimate is for Edgecombe, North Carolina at -265,254.5.  When adjusting against the intercept, both of these match the means in our means table.  Unfortunately, this method returns NA values for all states.  We get a multiple R squared of .567, with a tiny p value.


# Question 4: Build a regressor that best predicts average home values in this dataset.  Upload your predictions to Kaggle (IMPORTANT: you must use your UMD e-mail to get access to this competition).  If your UMD username is JSMITH, use JSMITH_DID as your username on Kaggle.  You are welcome to use any additional data you care to use so long as they are from 2007 or earlier.  You must describe any additional data you use in the writeup.  All students must make a submission to Kaggle; additional points available to those who do best (WARNING: make sure you submit something before the deadline, as you cannot use late days on the competition). Describe what you did to build the best predictor possible Give your best Kaggle score Give your Kaggle username. 

# The available columns have to work with are id, zip code, state, county, poverty, price2007 and price2013.
# We shouldn't use id, because it communicates no valuable information. 
# Should we use zip code?  Let's check to see if there is more than one record for each unique zip code in the data set. 

zipcount <- train %>% 
  group_by(zip) %>%
  summarise(n = n()) %>%
  arrange(n)
View(zipcount)

# And there isn't more than one record per zip code, so let's not use that. 

# Let's use state, county, poverty and price 2007. 
# Use poverty 
lm(statecountytrain$price2013 ~ statecountytrain$poverty)
summary(lm(statecountytrain$price2013 ~ statecountytrain$poverty))
# Use county
# Use price 2007
lm(statecountytrain$price2013 ~ statecountytrain$price2007)
summary(lm(statecountytrain$price2013 ~ statecountytrain$price2007))
# WOW! 

## Now use all four. 
mlm = lm(statecountytrain$price2013 ~ statecountytrain$statecounty + statecountytrain$state + statecountytrain$poverty + statecountytrain$price2007)
summary(mlm)
predict.lm(mlm, newdata=test)


statecopricemodel<-lm(statecountytrain$price2013 ~ statecountytrain$statecounty + statecountytrain$state + statecountytrain$zip + statecountytrain$)
statecopricemodel<-summary(statecopricemodel)$coefficients
View(statecopricemodel)

##The Evaluation metric is RMSE.  This rewards you for getting close to the true answer (and penalizes being very far away).  

##Your answer submission file should have only two columns: id and price2013.  The price2013 should reflect the prediction based on the information from that id in the house_test.csv file.