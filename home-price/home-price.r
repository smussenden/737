# Predicting Housing Prices. 
# INST737

# To start, let's load the tidyverse
library('tidyverse')

# Remove all previous objects, helpful when we're rerunning scripts. 
rm(list=ls(all=TRUE))

# Read in the data. We have two data sets.  One is our "training data", which we'll use to build a model to predict the price of houses in 2013, which we'll check the accuracy of by updating our "test data" to predict accuracy. 

test <- read_csv("csv/house_test.csv", col_names = TRUE)
train <- read_csv("csv/house_train.csv", col_names = TRUE)

# Before we do anything to the test data, let's save three separate copies we can use to answer questions 2, 3 and 4. 

teststate <- test
teststateco <- test
testmlm <- test

# Question 2: Predict 2013 home prices using state information only.  Answer these questions using all of the training data available.

# First, let's create a linear model to determine how much of the variance in home price can be explained by the state it's in.  We're using linear regeression because the dependent variable -- the thing we're trying to predict, house prices -- is a continuous variable.  If we went the other way -- if the dependent variable was state -- we'd use logisitic regression, because state is a categorical variable. Note, we have to write this function as lm(price2013 ~ state, data=train) instead of lm(train$price2013 ~ train$state), otherwise predict function won't work later. 

statepricemodel <- lm(price2013 ~ state, data=train)

# To view a summary of the coefficients for each state (including the estimate for mean house price for each state benchmarked against the intercept, the standard error, the t value and significance), we use summary.

summary(statepricemodel)

# We get a multiple R-squared of .2868, with a tiny p value, indicating a significant result. That means state accounts for 29 percent of variation in home prices. Let's answer the specific questions. 

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

# Now that we've trained our model on our training data, let's take the predicted values for each state and apply them to our test data.  

teststate <- teststate %>%
  mutate(price2013 = predict.lm(statepricemodel, newdata = test))
View(teststate)

# Question 3a: Predict 2013 home prices from state and county information. What US counties have the highest and lowest regression coefficients? Why?

# To get around the fact that, in several cases, a given county name exists in multiple states, let's first combine state and county into a single variable, so that we get unique counties. 

stcotrain <- train %>%
  unite(statecounty, state, county, remove=FALSE, sep="-")
View(stcotrain)

# Let's also calculate the mean home prices in each county, so we can confirm we are interpreting the estimate in our model correctly. From this, we see that the county with lowest mean price in the dataset is Edgecombe, North Carolina at $51,700.  The county with the highest mean price is San Mateo, California at $1.1 million.  This makes sense. San Mateo is one of the most expensive areas of the country, in Silicon Valley.  Edgecombe is in a rural, economically depressed part of North Carolina.

stcomeans <- stcotrain %>% 
  group_by(statecounty) %>%
  summarise(mean = mean(price2013)) %>%
  arrange(mean)
View(stcomeans)

# Let's create the multiple linear model, with the concatenated statecounty column and the state column. 

stcomodel<-lm(price2013 ~ statecounty + state, data=stcotrain)
tblstcomodel<-summary(stcomodel)$coefficients
View(tblstcomodel)

# The intercept is 316,954.55. The highest estimate is for San Mateo, at 814,800. The low estimate is for Edgecombe, North Carolina at -265,254.5.  When adjusting against the intercept, both of these match the means in our means table.  Unfortunately, this method returns NA values for all states.  We get a multiple R squared of .567, with a tiny p value.

# Before we predict the values in the test data file we created for this, we need to transform our test data to match the transformation we did to our training data.  We need to create a statecounty column, concatenating state and county.
teststateco <- test
teststateco <- teststateco %>%
    unite(statecounty, state, county, remove=FALSE, sep="-")

# Create an array of county names that are in our training data, but not in test data. I got this list when from an error when I tried to run the model without creating this and filtering it in the next step.

teststateco <- teststateco %>%
    filter(!statecounty %in% c("CO-gilpin", "FL-washington", "GA-union", "LA-bossier", "NH-grafton", "TX-hill", "TX-hunt", "TX-orange", "VA-carroll", "VA-harrisonburg city", "WA-douglas")) 

# Add our predicted values to a column called price 2013 inside the test set we created.

teststateco <- teststateco %>%   
  mutate(price2013 = predict.lm(stcomodel, newdata=teststateco))

View(teststateco)

# Question 4: Build a regressor that best predicts average home values in this dataset.  Upload your predictions to Kaggle (IMPORTANT: you must use your UMD e-mail to get access to this competition).  If your UMD username is JSMITH, use JSMITH_DID as your username on Kaggle.  You are welcome to use any additional data you care to use so long as they are from 2007 or earlier.  You must describe any additional data you use in the writeup.  All students must make a submission to Kaggle; additional points available to those who do best (WARNING: make sure you submit something before the deadline, as you cannot use late days on the competition). Describe what you did to build the best predictor possible Give your best Kaggle score Give your Kaggle username. 

# Let's build a model.  The available columns we have to work with are id, zip code, state, county, poverty, price2007 and price2013. We shouldn't use id, because it communicates no valuable information. Should we use zip code?  Let's check to see if there is more than one record for each unique zip code in the data set. 

zipcount <- train %>% 
  group_by(zip) %>%
  summarise(n = n()) %>%
  arrange(n)
View(zipcount)

# And there isn't more than one record per zip code, so let's not use that. Let's use state, county (statecounty), poverty and price 2007.  Let's run each of those individually to start, to get a sense of impact.  
# Poverty of a given zip code explains a fairly small amount of the price, R-squared of .06.  Still worth adding to the model, I think. 
povertyonly <- lm(price2013 ~ poverty, data=stcotrain)
summary(povertyonly)
# Let's use statecounty only.  By itself, it explains more of the price, R squared of .56.  Worth adding to the model. 
countyonly <- lm(price2013 ~ statecounty, data=stcotrain)
summary(countyonly)
# Let's use state only. By itself, it explains more of the price, R squared of .29.  Worth adding to the model. 

stateonly <- lm(price2013 ~ state, data=stcotrain)
summary(stateonly)

# Let's use price 2007.  Wow.  By itself, it explains most of the variation in 2013 price, R square of .9. Definitely worth adding to the model. 

price2007only <- lm(price2013 ~ price2007, data=stcotrain)
summary(price2007only)

# Now let's build a mutliple linear model to combine those factors to build a better model.  Let's start with price2007 and county. Pretty strong, we get a R squared of .97. 

pricecounty <- lm(price2013 ~ price2007 + statecounty, data=stcotrain)
summary(pricecounty)

# Now let's add in poverty, see if we get any improvement. None.  

pricecountypoverty <- lm(price2013 ~ poverty + price2007 + statecounty, data=stcotrain)
summary(pricecountypoverty)

# And adding state doesn't really seem add anything either.  

pricecountypovertystate <- lm(price2013 ~ poverty + price2007 + statecounty + state, data=stcotrain)
summary(pricecountypovertystate)

## But I think our final model we'll go with is price2007 and county

mlm <- lm(price2013 ~ price2007 + statecounty, data=stcotrain)
summary(mlm)

# Now let's prep our test data set 

testmlm <- testmlm %>%
  unite(statecounty, state, county, remove=FALSE, sep="-")

# Create an array of county names that are in our training data, but not in test data. I got this list when from an error when I tried to run the model without creating this and filtering it in the next step.

testmlm <- testmlm %>%
  filter(!statecounty %in% c("CO-gilpin", "FL-washington", "GA-union", "LA-bossier", "NH-grafton", "TX-hill", "TX-hunt", "TX-orange", "VA-carroll", "VA-harrisonburg city", "WA-douglas")) 


# Add our predicted values to a column called price 2013 inside the test set we created.
testmlm <- testmlm %>%   
  mutate(price2013 = predict.lm(pricecounty, newdata=testmlm))
View(testmlm)

# Now let's export a csv that just contains id and price2013. 

finalresult <- testmlm %>%
  select(id, price2013)
View(finalresult)

# create an empty file in our csv
file.create("csv/finalresult.csv")

# put our final result predictions into the empty csv file 
write_csv(finalresult, path="csv/finalresult.csv", na = "NA", append = FALSE) 

# Question 5. (10 points) Suppose you have 2 bags. Bag #1 has 1 black ball and 2 white balls. Bag #2 has 1 black ball and 3 white balls. Suppose you pick a bag at random, and select a ball from that bag. What is the probability of selecting a white ball?
# Answer 5: 70.8 percent. Showing work:
# A = Probability of choosing either bag1 or bag 2 = .5
# B = Probability of choosing white in bag1 = .666
# C = Probability of choosing white in bag2 = .75
# D = Probability of randomly drawing bag1 and getting white (B*A) = .333
# E = Probability of randomly drawing bag2 and getting white(C*A) = .375
# F = Cumulitive probability of drawing either bag 1 or bag 2 at random and getting a white ball (D+E) = .708 or 70.8%

#Question 6 (10 points) A soccer team wins 60% of its games when it scores the first goal, and 10% of its games when the opposing team scores first. If the team scores the first goal about 30% of the time, what fraction of the games does it win? 
# Answer: 25 percent of games.
# probability team wins when scores first goal = .6
# probability team wins when it doesn't score first goal = .1
# probability team scores first goal = .3
#  probability team doesn't score first goal = .7
# Percentage of games won = Prob(Win when score first) * Prob(score first) + Prob(Win when not score first) * Prob(not score first)
# print((.6 * .3) + (.1 * .7))
# Result: .25


