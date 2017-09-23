##Titanic passenger data. Intention is to see if we can predict which passengers survive. We'll build our model on the training data, and then test it on the testing data.

##Data Dictionary
##Variable	Definition	Key
##survival	Survival	0 = No, 1 = Yes
##pclass	Ticket class	1 = 1st, 2 = 2nd, 3 = 3rd
##sex	Sex	
##Age	Age in years	
##sibsp	# of siblings / spouses aboard the Titanic	
##parch	# of parents / children aboard the Titanic	
##ticket	Ticket number	
##fare	Passenger fare	
##cabin	Cabin number	
##embarked	Port of Embarkation	C = Cherbourg, Q = Queenstown, S = Southampton

##read in the training data, call it train. read in the test data, call it test.
train <- read.csv(file.choose(),header=T)
test <- read.csv(file.choose(),header=T)

##View both 
View(train)
View(test)

##Run some structural statistics on it to get a sense of the data.
str(train)
str(test)

##How many people lived and how many died? Create a table with count people who survived (1) and who died (0).  Then, calculate that as a percentage. 62 percent died, 38 percent lived. We have to use table because these are numeric variables.
table(train$Survived)
prop.table(table(train$Survived))

##We're going to make our first prediction, which is a totally wrong prediction that everyone in our test set dies.  But we're going to do it anyway, just so we can see what it will ultimately look like when we've tested a model.  We add a column called Survived (which doesn't exist in the test set), and we populate all 418 rows with the value 0 -- for died -- using the repeat function.
test$Survived <- rep(0, 418)
View(test)

##"Submit a csv file with the PassengerId and Survived predictions to Kaggle. Extract those two columns from the test dataframe, store them in a new container.  And we're going to rename the columns so they look nice, using the equals sign
submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "~/Desktop/INST737/titanic/theyallperish.csv", row.names = FALSE)

##Upload to Kaggle. Oof. Not a good result.

##Let's run a summary of gender on the training data
summary(train$Sex)

##Let's use proportion table to figure out what percentage of men and women survived and died. And we see women mostly lived. 75 percent of women lived, 81 percent of men died.  
prop.table(table(train$Sex, train$Survived),1)

##So, NOW, if we update our test data to predict that women lived and men died, then we should improve.  So let's do that. Set the Survived row in our data to 0, unless it's a woman, then set it to 1.
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
View(test)

##Now, let's push out our CSV again as womenlive.csv then upload to Kaggle.
submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "~/Desktop/INST737/titanic/womenlive.csv", row.names = FALSE)

##Getting better. We've tested women. Now let's look at age variable, to test children. 
summary(train$Age)

##And let's create a new category for child, in which we assign 0 to everyone, then override that by assigning 1 to people under 18.
train$Child <- 0
train$Child[train$Age < 18] <- 1

##Now we can see if there's something interesting by child and gender.  We can use the aggregate command and write our own function to calculate the percentage of each group sex/child group that lived or died.  WTF. A higher percentage of adult women lived than girls. A much higher percentage of boys lived than adult men. 
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

##So, dude says there's nothing there from the results of this, but I feel like eventually you could find a way to predict boys more likely to live -- but then again, only 40 percent of boys live, and 50 percent is coin flip, so maybe not. Anyhoo, lets look at fares and passenger classes.  Passenger classes are broken into 1st 2nd and 3rd class.  So we can use those.  But fare is a continuous variable, so will need to bin those to use.
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

##Now lets run the aggregate command again, but this time lets look at death rate of people in different fare/class/sex groups.  THese numbers are percentages. 
aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

##Interesting. Males pretty much the same.  But some clear outliers to the "women live" narrative with the women.  Women who paid 20-30 or 30+ and in 3rd class died a lot.  Let's add them to our model.
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0

##Now, let's push out our CSV again as womenbutnotpoorslive.csv then upload to Kaggle.
submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "~/Desktop/INST737/titanic/womenbutnotpoorslive.csv", row.names = FALSE)