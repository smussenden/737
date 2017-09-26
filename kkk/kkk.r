## Analysis of data from "Second Ku Klux Klan, 1919-1940" project. http://scholarscompass.vcu.edu/hist_data/1/. My intention is to discover patterns documenting the rise of the second-wave KKK in the U.S.  Among the key questions: how did the number build over time?  In what states or regions was it concentrated?    
##Keys -- take out panama. 
## https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf
## https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

## First, save this environment as an R project - rproj file -- and put it in the Github repo I set up for this project. When I do that, it should automatically set the working directory to the Github repo.  I can confirm that with the getwd() -- get working directory -- command. 
getwd()

## Load the tidyverse, so we can work with dplyr, ggplot2, readr
library('tidyverse')

## Two CSVs here. klavern.csv has name, city, year and location details for each "klavern".  state.csv has codes for each states.  The other one - all_sans_sources - has both put together.  I'm going to not work with that one, so I can do the joins. Also not going to work with the sources table. Load in the data. Note that I'm using tidyverse's readr instead of read.csv, which is default R. 

klaverns <- read_csv("csv/klavern.csv", col_names = TRUE)
states <- read_csv("csv/states.csv", col_names = TRUE)

## Let's take a look at the klaverns table
View(klaverns)
glimpse(klaverns)

## Let's take a look at the states table
View(states)
glimpse(states)

## Let's join the two tables, so that we have the name of the state, not just the state id, in the klaverns table.  But that will fail, because in the klaverns table, the column is called "state_id" but in states, the same information is just called "id".  So first we have to rename the column id in the states table as state_id. 
states <- rename(states, state_id = id)

## Now let's do the left join
klaverns <- left_join(klaverns, states, by ="state_id")

## And let's move the state name column -- which is just called state -- so it's next to state.  Note that this select is part of dplyr.  
klaverns <- select(klaverns,id:city, state, klan_number:year)
View(klaverns)

## And let's take out panama, because we want to stick to u.s. 
klaverns <- filter(klaverns, state != "Panama")
View(states)

##How many groups are there in each state?
statecount <- klaverns %>% 
  group_by(state)%>%
  summarise(count = n()) %>%
  arrange(desc(count))
View(statecount)

##Let's create some summary statistics for the data frame/table that counted groups in each state
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

## How many groups were identified in each year? First, take out rows without a year.  Then give me a count, arranged from highest to lowest count. 

klavernsnoyear <- filter(klaverns, year != 0)

yearcount <- klavernsnoyear %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
  
View(yearcount)

## Let's do the same thing, but arrange by year this time.

yearcount2 <- klavernsnoyear %>%
  group_by(year) %>%
  summarise(count = n()) %>%
  arrange(year)

View(yearcount2)

##Let's create some summary statistics for the count of groups in each year.
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

## Now let's bin the years into decades by using dplyr's mutate (not cbind) to add a column. And then let's group them by those decades.   

klavernsnoyear <- mutate(klavernsnoyear, decade = cut(klavernsnoyear$year, breaks=c(1914,1919,1929,1939,1949), labels=c("10s","20s","30s","40s")))

decadecount <- klavernsnoyear %>%
  group_by(decade) %>%
  summarise(count= n()) %>%
  arrange(decade)

View(decadecount)

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

## Now let's break them down by state and decade
statedecadecount <- klavernsnoyear %>%
  group_by(state,decade) %>%
  summarise(count=n()) %>%
  arrange(state)
View(statedecadecount)

## And let's just filter maryland out of statedecadecount
maryland <- filter(statedecadecount,state == "Maryland")
View(maryland)

## Now let's look at the most common nicknames by grouping by nickname.  Note that this might not catch everything, because of issues like Charles Lindberg and Charles A. Lindberg. Also, let's look only at most common ones -- 3 or more entries. Note that we're going back to original klaverns data set, because we want to look at all of them, even ones without a year.
nicknamecount <- klaverns %>%
  group_by(nickname) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(count > 2) 
View(nicknamecount)

## Now let's look at just Maryland name breakdown.  
nicknamecountmd <- klaverns %>%
  filter(state == "Maryland") %>%
  group_by(nickname) %>%
  summarise(count=n()) %>%
  arrange(desc(count))
View(nicknamecountmd)  

library(ggthemes)
library(scales)

ggplot(data=nicknamecountmd, aes(x = reorder(nickname, count), y=count, fill=count, label=count)) +
  geom_bar(stat="identity") +
  geom_text(size = 3, position = position_stack(vjust = .5), colour="white") +
  coord_flip() + 
  ggtitle("Most Maryland Klaverns not Named") +
  scale_color_fivethirtyeight(count) +
  theme_fivethirtyeight() + 
  theme(legend.position="none")
