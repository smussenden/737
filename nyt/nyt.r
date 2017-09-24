##Need to go back through and add comments.
nyt1 <- read.csv("/1_week_nyt/nyt1.csv", header=T)
nyt1 <- read.csv("csv/1_week_nyt/nyt1.csv", header=T)
rm(nyt1)

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

## Now let's assemble our seven CSVs in to a single CSV, and store this new sheet.  Note: as long as header rows are the same across sheet, this won't cause problems. Also, the header rows are not repeated with this method, unlike what happens with Excel if you tried to copy and paste sheets together. 
nytweek <- rbind(nyt1, nyt2, nyt3, nyt4, nyt5, nyt6, nyt7)

##To keep our workspace nice and tidy, we can remove the data sets we don't need. nyt1 - 7 are gone, leaving just nytweek. 

rm(nyt1, nyt2, nyt3, nyt4, nyt5, nyt6, nyt7)

##Let's take a look at our data set, and run some summary statistics.  

View(nytweek)
summary(nytweek)

##Now we're ready to start looking at how different age groups use the site.  To get a sense of how ages are distributed, let's create a table with a count of users by age.  

table(nyt1$Age)

##There are a ton of places where age is equal to zero. Since it's almost certainly true that people who are 0 years old are not viewing the NYT site, we should remove those.  Later on, though, it will be useful to analyze the data with those included.  But when we're analyzing by age, they're meaningless.  So let's take them out, but be sure to keep our original data intact.  We'll call this new data set nytweekno0.  

nytweekno0 <- subset(nytweek, Age > 0)

## Now let's group each row/observation into an age group, using the cut function.  Our age groups are <18, 18-24, 25-34, 35-44, 45-54, 55-64, 65+.  We use the cut function and assign breaks. We're also going to group each row/observation into a click group -- whether they clicked through (1+ click) or didn't (0 clicks).  We assign each of these cut functions to a variable name (agegroup, clickgroup) and then use column bind (cbind) to append them to the end of our existing data set. 

nytweeknno0 <- cbind(nytweek, agegroup = cut(nytweekno0$Age, breaks=c(0,18,24,34,44,54,64,Inf), labels=c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")), clickgroup = cut(nytweekno0$Clicks, breaks=c(-1,0,Inf), labels= c("No Clicks", "Some Clicks")))

## For this analysis, we only need to look at a single day.  So let's create a subset of the data that only includes the first day. Let's call this nytdayno0

nytdayno0 <- subset(nytweek, Day = "One")

##Time to visualize.  Let's load the ggplot library. 
library(ggplot2)

##Let's create small multiple histograms that shows the distribution of Impressions for each age group. 

ggplot(data=nytdayno0, aes(x=Impressions)) +
geom_histogram(binwidth = 1) +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))


##Now CTR...



ggplot(data=nyt, aes(x=Impressions/Clicks)) +
geom_histogram(binwidth = 1) +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
nytweek$CTR <- nytweek$Clicks/nytweek$Impressions
View(nytweek)
nytday <- subset(nytweek, Day == "One")
ggplot(data=nyt, aes(x=CTR)) +
geom_histogram(binwidth = 1) +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram(binwidth = 1) +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_grid(~agegroup.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_grid(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_row(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_wrap(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
(.~agegroup) +
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
_wrap
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_grid(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=subset(nytday, CTR > 0), aes(x=CTR)) +
geom_histogram() +
facet_grid(.~agegroup) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=subset(nytday, CTR > 0), aes(x=CTR)) +
geom_histogram() +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=nytday, aes(x=CTR)) +
geom_histogram() +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
ggplot(data=subset(nytday, CTR > 0), aes(x=CTR)) +
geom_histogram() +
facet_grid(agegroup~.) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
theme(axis.text.x = element_text(angle=0))
View(nytday)
summary(nytday$Signed_In)
table(nytday$Signed_In)
table(nytweek$Signed_In)
##Read in the NYT Data (uncomment if running again)
nyt1 <- read.csv("/Desktop/737/HW1/1_week_nyt/nyt1.csv", header=T)

## Create a single column that assigns an age-range category to each observation, under 18, 18-24, 25-34, 35-44, 45-54, 55-64,65+. Start at -1 so we include zero.
nyt <- cbind(nyt, agegroup = cut(nyt$Age, breaks=c(-1,18,24,34,44,54,64,Inf), labels=c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")), clickgroup = cut(nyt$Clicks, breaks=c(-1,0,Inf), labels= c("No Clicks", "Some Clicks")))

##load ggplot
library(ggplot2)

##Create a small multiples histogram
ggplot(data=nyt, aes(x=Impressions)) +
  geom_histogram(binwidth = 1) +
  facet_grid(agegroup~.) + 
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0)) 

##Create a small multiples histogram
THIS IS NOT RIGHT...CTR = click through rate = impressons/
ggplot(data=nyt, aes(x=Clicks)) +
  geom_histogram(binwidth = 1) +
  facet_grid(agegroup~.) + 
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0)) 
