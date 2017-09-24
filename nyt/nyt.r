## NYT Web Traffic Analysis

## First, save this environment as an R project - rproj file -- and put it in the Github repo I set up for this project. When I do that, it should automatically set the working directory to the Github repo.  I can confirm that with the getwd() -- get working directory -- command. 
getwd()

## Now, let's read in the week's worth of New York Times web traffic data, one csv per day.  Store them as variables, nyt1 through nyt7.  

nyt1 <- read.csv("csv/1_week_nyt/nyt1.csv", header=T)
nyt2 <- read.csv("csv/1_week_nyt/nyt2.csv", header=T)
nyt3 <- read.csv("csv/1_week_nyt/nyt3.csv", header=T)
nyt4 <- read.csv("csv/1_week_nyt/nyt4.csv", header=T)
nyt5 <- read.csv("csv/1_week_nyt/nyt5.csv", header=T)
nyt6 <- read.csv("csv/1_week_nyt/nyt6.csv", header=T)
nyt7 <- read.csv("csv/1_week_nyt/nyt1.csv", header=T)

##Later on, it will be helpful to know which day of the week each of these observaations came from.  So let's add a new column to each of these called "Day" and set it equal to the day number. The column Day doesn't currently exist, so adding it will append it after the last column. 

nyt1$Day <- "One"
nyt2$Day <- "Two"
nyt3$Day <- "Three"
nyt4$Day <- "Four"
nyt5$Day <- "Five"
nyt6$Day <- "Six"
nyt7$Day <- "Seven"

nyt1$DayNumber <- 1
nyt2$DayNumber <- 2
nyt3$DayNumber <- 3
nyt4$DayNumber <- 4
nyt5$DayNumber <- 5
nyt6$DayNumber <- 6
nyt7$DayNumber <- 7

## Now let's assemble our seven CSVs in to a single CSV, and store this new sheet.  Note: as long as header rows are the same across sheet, this won't cause problems. Also, the header rows are not repeated with this method, unlike what happens with Excel if you tried to copy and paste sheets together. 
nytweek <- rbind(nyt1, nyt2, nyt3, nyt4, nyt5, nyt6, nyt7)

##To keep our workspace nice and tidy, we can remove the data sets we don't need. nyt1 - 7 are gone, leaving just nytweek. 

rm(nyt1, nyt2, nyt3, nyt4, nyt5, nyt6, nyt7)

##Let's take a look at our data set, and run some summary statistics.  

View(nytweek)
summary(nytweek)

##Now we're ready to start looking at how different age groups use the site.  To get a sense of how ages are distributed, let's create a table with a count of users by age.  

table(nytweek$Age)

##There are a ton of places where age is equal to zero. Since it's almost certainly true that people who are 0 years old are not viewing the NYT site, we should remove those.  Later on, though, it will be useful to analyze the data with those included.  But when we're analyzing by age, they're meaningless.  So let's take them out, but be sure to keep our original data intact.  We'll call this new data set nytweekno0.  

nytweekno0 <- subset(nytweek, Age > 0)

## Now let's group each row/observation into an age group, using the cut function.  Our age groups are <18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+.  We use the cut function and assign breaks. We're also going to group each row/observation into a click group -- whether they clicked through (1+ click) or didn't (0 clicks).  We assign each of these cut functions to a variable name (agegroup, clickgroup) and then use column bind (cbind) to append them to the end of our existing data set. 

nytweekno0 <- cbind(nytweekno0, agegroup = cut(nytweekno0$Age, breaks=c(0,18,24,34,44,54,64,Inf), labels=c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")), clickgroup = cut(nytweekno0$Clicks, breaks=c(-1,0,Inf), labels= c("No Clicks", "Some Clicks")))

##Let's also create a column for Clickthru rate called CTR, which is clicks divided by impressions.  Because CTR doesn't exist, it's automatically added to the end of the existing table. 
nytweekno0$CTR <- nytweekno0$Clicks/nytweekno0$Impressions


## For this analysis, we only need to look at a single day.  So let's create a subset of the data that only includes the first day. Let's call this nytdayno0. Remember, need two equals signs. 

nytdayno0 <- subset(nytweekno0, Day == "One")

##Time to visualize.  Let's load the ggplot library. 
library(ggplot2)

##Let's create small multiple histograms that shows the distribution of Impressions for each age group. It appears to create a normal distribution around users with 5 impressions.It also shows just more users for35-44, 45-54 and 25-34 age group.

ggplot(data=nytdayno0, aes(x=Impressions)) +
geom_histogram(binwidth = 1) +
  facet_grid(agegroup ~ .)
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0))

##Let's also create a small multiple histogram that shows click through rate. This shows that the vast majority of users have a clickthrough rate of zero.  It also destroys the scale for the graph, making it impossible to see the distribution of the non-zero users.  

ggplot(data=nytdayno0, aes(x=CTR)) +
    geom_histogram(binwidth = .01) +
    facet_grid(agegroup ~ .)
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
    theme(axis.text.x = element_text(angle=0))

##To get a better look at the non-zero distribution of CTR, let's rebuild the graph without the 0 users.  Most of the users are disributed under .25 percent CTR.
  
ggplot(data=subset(nytdayno0, CTR > 0), aes(x=CTR)) +
    geom_histogram(binwidth = .01) +
    facet_grid(agegroup ~ .)
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
    theme(axis.text.x = element_text(angle=0))  

## Let's look at users based on click behavior.  We've segmented them into "No Clicks" and "Some Clicks". The vast majority of users do not click through to another page. 
  
  ggplot(data=nytdayno0, aes(x=clickgroup)) +
   geom_bar()
  
## Let's drill in specificially on the behavior of logged-in users vs. non-logged in users.  0 is a not signed in user.  1 is signed in user. We need to go back to our original nytweek for this. 
  
nytday <- subset(nytweek, Day == "One")

## Let's create a natural language version of 0 and 1 for the logged in value "Logged In" "Not Logged In" and add it to the end of the data set.     
  
nytday <- cbind(nytday, usertype = cut(nytday$Signed_In, breaks=c(-1,0,Inf), labels= c("Not Logged In", "Logged In")))

##Let's create small multiple histograms that shows the distribution of Impressions for each usertype. The distribution of impressions is pretty similar for logged in vs not logged in users.  

ggplot(data=nytday, aes(x=Impressions)) +
  geom_histogram(binwidth = 1) +
  facet_grid(usertype ~ .)
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0))

##Let's also create a column for Clickthru rate called CTR, which is clicks divided by impressions.  Because CTR doesn't exist, it's automatically added to the end of the existing table. 
nytday$CTR <- nytday$Clicks/nytday$Impressions

##Let's create small multiple histograms that shows the distribution of Click Through Rate for each usertype. It shows that, again, the vast majority of of users have a CTR of zero. 

ggplot(data=nytday, aes(x=CTR)) +
  geom_histogram(binwidth = .01) +
  facet_grid(usertype ~ .)
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0))


##To get a better look at the non-zero distribution of CTR, let's rebuild the graph without the 0 users.  Most of the users are disributed under .25 percent CTR. The distributions are fairly similar for both. 

ggplot(data=subset(nytday, CTR > 0), aes(x=CTR)) +
  geom_histogram(binwidth = .01) +
  facet_grid(usertype ~ .)
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0)) 

##Now let's drill in on the behavior of men vs. women.  Let's create a natural language version of 0 and 1 for the logged in value "Female" "Male" and add it to the end of the data set in column sex.     

nytday <- cbind(nytday, sex = cut(nytday$Gender, breaks=c(-1,0,Inf), labels= c("Female", "Male")))

##Let's create small multiple histograms that shows the distribution of Impressions for each gender. Besides showing subsantially more women in the data set, the distribution of Impressions appears to be similar. 

ggplot(data=nytday, aes(x=Impressions)) +
  geom_histogram(binwidth = 1) +
  facet_grid(sex ~ .)
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0))

##Let's create small multiple histograms that shows the distribution of Click Through Rate for each gender. It shows that, again, the vast majority of of users have a CTR of zero in both genders. 

ggplot(data=nytday, aes(x=CTR)) +
  geom_histogram(binwidth = .01) +
  facet_grid(sex ~ .)
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0))

##To get a better look at the non-zero distribution of CTR, let's rebuild the graph without the 0 users.  Most of the users are disributed under .25 percent CTR. Similar distribution for both genders. 

ggplot(data=subset(nytday, CTR > 0), aes(x=CTR)) +
  geom_histogram(binwidth = .01) +
  facet_grid(sex ~ .)
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0))  

##Now let's track movements across time.  For each day, let's calcluate some summary statistics. But first, let's load the tidyverse, so we can work with that. 

library("tidyverse")

##And let's add a new column to nytweek, using the grammar of tidyverse, using mutate() to calculate the clickthrough rate.

nytweek <- mutate(nytweek, CTR = Clicks/Impressions)

##Now let's break down what, if any, difference there is by day across a single week.  We'll create a new data frame (or a tibble in parlance of tidyverse) called nytsummary and group by day of the week, and calculate summary stats (means, across each category), plus a count of total observations.  Here's what we see.  There's almost know meaningful variation in average age (range 29.43 to 29.5).  The gender profile -- if 0 is woman and 1 is man, an average under .5 indicates a larger percentage of women -- stays relatively constant across days, raging from .374 on day two, to .355 on day six. The mean impressions stays extremly constant at 5 on all days.  And the mean number of clicks stays constant at .092.  The percentage of signed in users -- if 0 is not signed in and 1 is signed in, then a mean over .5 means more signed in users than not signed in -- stays constant at .7.  In fact, the only real variation across days present is the total number of users.  That figure dips from a low of 370K on day 5 to a high of 765K on day 6.  

nytsummary <- nytweek %>% 
  group_by(DayNumber, Day) %>%
  arrange(Day) %>%
  summarise(MeanAge = mean(Age),
            MeanGender = mean(Gender),
            MeanImpressions = mean(Impressions),
            MeanClicks = mean(Clicks),
            MeanSignedIn = mean(Signed_In),
            MeanCTR = mean(CTR),
            total = n()) 

##This plot shows the total number of users by day, signifying the huge spike on day six, preceeded by the weakest day of the observed period, day five. There was so little variation in the other measures that it wasn't worth showing. At least in the period observed, there was not significant variation amongst users. Note: the "identity" stat was critical to making this work as a bar plot.  The last line is to force the x axis to show all the day labels. 

library("RColorBrewer")  
ggplot(data=nytsummary, aes(DayNumber,total)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = round(seq(min(nytsummary$DayNumber), max(nytsummary$DayNumber), by = 1),1)) +
  theme_economist() + scale_colour_economist() 
