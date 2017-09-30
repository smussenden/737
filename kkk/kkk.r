## This is an analysis of data from "Second Ku Klux Klan, 1919-1940" project. http://scholarscompass.vcu.edu/hist_data/1/. My intention is to discover patterns documenting the rise of the second-wave KKK in the U.S.  Among the key questions: how did the number build over time?  In what states or regions was it concentrated?    
## Handy cheatsheets
## https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
## https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf
## http://r4ds.had.co.nz/introduction.html
## https://yihui.name/knitr/
## See cheatsheets folder for more info
## http://ggplot2.tidyverse.org/reference/index.html
## http://dplyr.tidyverse.org/reference/index.html
## http://readr.tidyverse.org/reference/index.html

## First, save this environment as an R project - rproj file -- and put it in the Github repo I set up for this project. When I do that, it should automatically set the working directory to the Github repo.  I can confirm that with the getwd() -- get working directory -- command. 
getwd()

## Remove all data objects from working memory
rm(list = ls())

## Load the tidyverse, so we can work with dplyr, ggplot2, readr, and also the ggplot themes package ggthemes 
library('tidyverse')
library(ggthemes)
library(scales)

## Two CSVs here. klavern.csv has name, city, year and location details for each "klavern".  state.csv has codes for each states.  The other one - all_sans_sources - has both put together.  I'm going to not work with that one, so I can do the joins. Also not going to work with the sources table. Load in the data. Note that I'm using tidyverse's readr instead of read.csv, which is default R. 

klaverns <- read_csv("csv/klavern.csv", col_names = TRUE)
states <- read_csv("csv/states.csv", col_names = TRUE)

## Let's take a look at the klaverns table.  View loads it up into a GUI data frame.  Glimpse gives me a breakdown of the fields (variables), with data type (int, chr, dbl) and some sample values.  as_tibble is sort of like glimpse, but it looks more like a data table. summary is NOT dplyr, but it does give us a nice sense of means, medians, min and max, as well as quartile, for numerical data. 
View(klaverns)
glimpse(klaverns)
as_tibble(klaverns)
summary(klaverns)

## Let's take a look at the states table
View(states)
glimpse(states)
as_tibble(states)
summary(states)

## Let's join the two tables, so that we have the name of the state, not just the state id, in the klaverns table.  But that will fail, because in the klaverns table, the column is called "state_id" but in states, the same information is just called "id".  So first we have to rename the column id in the states table as state_id.  The rename function is dplyr. 
states <- rename(states, state_id = id)

## Now let's do the left join wit some dplyr
klaverns <- left_join(klaverns, states, by ="state_id")

## And let's move the state name column -- which is just called state -- so it's next to state.  Note that this select is part of dplyr. It lets me select certain rows, ranges of rows, move rows around. 
klaverns <- select(klaverns,id:city, state, klan_number:year)
View(klaverns)

## And let's take out panama, because we want to stick to u.s. 
klaverns <- filter(klaverns, state != "Panama")
View(states)

##Now let's answer some questions using dplyr.  Note we've named the data set at the top, followed by the %>% operator.  That means that we are applying all of the functions to the klaverns data set.  That lets us get away with not having to write the name of the dataset again and again in each function.  So instead of group_by(klaverns, state), we get group_by(klaverns)

##How many groups are there in each state? As we see, Pennsylvania had the most, with 193, followed by Texas with 163. It's unclear if this is a fully accurate picture of the klavernns in the U.S., or just representative of the data we have.  
statecount <- klaverns %>% 
  group_by(state)%>%
  summarise(count = n()) %>%
  arrange(desc(count))
View(statecount)

##Let's create some summary statistics for the data frame/table that counted groups in each state.  The mean number of groups is 40, with a median of 25. 
statesummary <- statecount %>%
  summarise(Name = "StateSummary",
            Mean = mean(count),
            Median = median(count),
            Min = min(count),
            Max = max(count),
            Variance = var(count),
            StandardDeviation = sd(count),
            Total = sum(count),
            Uniques = n()
            )
View(statesummary)

## How many groups were identified in each year? First, take out rows without a year.  Then let's do a count, arranged from highest to lowest count. The most: 1923. 

klavernsnoyear <- filter(klaverns, year != 0)

yearcount <- klavernsnoyear %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  
View(yearcount)

## Let's visualize it. 
ggplot(data=yearcount, aes(x = reorder(year,count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  ggtitle("Klavern growth concentrated in early 1920s") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  xlab("Year") +
  ylab("Count")

## Let's do the same thing, but arrange by year this time, to get a sense of the distribution. There was a huge spike in the early 1920s, followed by a decline, followed by another good sized spike in the early 1930s. 

yearcount2 <- klavernsnoyear %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(year)
View(yearcount2)


## Let's visualize it. 

ggplot(data=yearcount2, aes(x = year, y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  ggtitle("Klavern growth concentrated in early 1920s") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  theme(axis.text.x=element_text(angle=90,hjust=1)) + 
  xlab("Year") +
  ylab("Count")

##Let's create some summary statistics for the count of groups in each year. The mean number for each year was 83, but the median was only 28. 

yearsummary <- yearcount %>%
  summarise(Name = "YearSummary",
            Mean = mean(count),
            Median = median(count),
            Min = min(count),
            Max = max(count),
            Variance = var(count),
            StandardDeviation = sd(count),
            Total = sum(count),
            Uniques = n()
  )
View(yearsummary)

## Now let's bin the years into decades by using dplyr's mutate (not cbind) to add a column. And then let's group them by those decades, and count the number for each decade. We see extreme concentration in the 1920s.   

klavernsnoyear <- mutate(klavernsnoyear, decade = cut(klavernsnoyear$year, breaks=c(1914,1919,1929,1939,1949), labels=c("10s","20s","30s","40s")))

decadecount <- klavernsnoyear %>%
  group_by(decade) %>%
  summarise(count= n()) %>%
  arrange(decade)

View(decadecount)

## Let's visualize it.

ggplot(data=decadecount, aes(x = reorder(decade, count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  ggtitle("Growth of Klaverns in the U.S.") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  xlab("Decade") +
  ylab("Count")

##Let's create some summary statistics for the count of groups in each decade.
decadesummary <- decadecount %>%
  summarise(Name = "DecadeSummary",
            Mean = mean(count),
            Median = median(count),
            Min = min(count),
            Max = max(count),
            Variance = var(count),
            StandardDeviation = sd(count),
            Total = sum(count),
            Uniques = n()
  )
View(decadesummary)

## Now let's break them down by state and decade.  In most states, we see the same concentration of growth in the 1920s evident in the entire data set, but that's not true of every state. Alaska had more than twice as many klaverns in the 30s than the 20s. 
statedecadecount <- klavernsnoyear %>%
  group_by(state,decade) %>%
  summarise(count=n()) %>%
  arrange(state)
View(statedecadecount)

## And let's just filter maryland out of statedecadecount to take a closer look at local data. More than three times as many klaverns emerged in the 1920s as the 1930s. 
maryland <- filter(statedecadecount,state == "Maryland")
View(maryland)

ggplot(data=maryland, aes(x = reorder(decade, count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  ggtitle("Growth of Klaverns in Maryland") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  xlab("Decade") +
  ylab("Count")

## Now let's look at the most common nicknames by grouping by nickname.  Note that this might not catch everything, because of issues like Charles Lindberg and Charles A. Lindberg. Also, let's look only at most common ones -- 3 or more entries. Note that we're going back to original klaverns data set, because we want to look at all of them, even ones without a year. 

nicknamecount <- klaverns %>%
  group_by(nickname) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(count > 2) 
View(nicknamecount)

##Let's visualise it. This shows the clear dominance of the non-named klaverns, but it's hard to see the distribution of others. 

ggplot(data=nicknamecount, aes(x = reorder(nickname, count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  coord_flip() + 
  ggtitle("Most popular Klavern names") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  xlab("Klaverns") +
  ylab("count")

## Let's run it again, this time removing the not named. We find that the data set does not have nicknames for more than half of klaverns. And we see some interesting trends among the most common names.  Robert E. Lee makes sense, as does Nathan Bedford Forrest, since both are Confederate generals. The inclusion of Abraham Lincoln was genuinely surprising -- perhaps irony?

nicknamecountno0 <- klaverns %>%
  group_by(nickname) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(count > 2 & nickname != "Not Named") 
View(nicknamecount)

ggplot(data=nicknamecountno0, aes(x = reorder(nickname, count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  coord_flip() + 
  ggtitle("Most popular Klavern names") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  xlab("Klaverns") +
  ylab("count")


 
## Now let's look at just Maryland name breakdown. Again, more than half of Maryland klaverns are not named. A common naming convention appears to be geographical -- rivers (Severn, Choptank) and counties (Harford, Queen Anne's).
nicknamecountmd <- klaverns %>%
  filter(state == "Maryland") %>%
  group_by(nickname) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
View(nicknamecountmd)  

##And visualize it. 
ggplot(data=nicknamecountmd, aes(x = reorder(nickname, count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  coord_flip() + 
  ggtitle("Most Maryland Klaverns not Named") +
  scale_color_fivethirtyeight(count) +
  theme_gdocs() +
  xlab("Maryland klaverns") +
  ylab("count")
