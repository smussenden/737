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

## How many groups are there in each state?

statecount <- klaverns %>% 
  group_by(state) %>%
  summarise( count = n())
statecount <- arrange(statecount, desc(count))
View(statecount)

## How many groups were identified in each year? First, take out rows without a year.  Then give me a count. 
klaverns <- filter(klaverns, year != 0)

yearcount <- klaverns %>%
  group_by(year) %>%
  summarise( count = n())
yearcount <- arrange(yearcount, desc(count))
View(yearcount)

## Let's do the same thing, but arrange by year

yearcount2 <- klaverns %>%
  group_by(year) %>%
  summarise( count = n())
yearcount2 <- arrange(yearcount2, year)
View(yearcount2)

library(choroplethr)

gtd <- read.csv("gtd.csv")

US <- gtd[gtd$country_txt == "United States",]

stateattacks<- ddply(US, .(provstate), "nrow")

# choroplethr needs these column names
colnames(stateattacks) <- c("region", "value")

# choroplethr might do this internally (have not checked)
# but it's not a bad thing to do anyway
stateattacks$region <- tolower(stateattacks$region)

# it won't work with the (…) bit and that might have been your 
# problem with "old school' chorpleths not working
stateattacks$region <- gsub(" (u.s. state)", "", stateattacks$region, fixed=TRUE)

# make the US choropleth
choroplethr(stateattacks, lod="state")