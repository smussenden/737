## Scripts to import game by game stats for 2017 nats from fangraphs.com http://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2017&month=0&season1=2017&ind=0&team=24&rost=0&age=0&filter=&players=0&sort=2,d&page=1_30. Example: http://www.fangraphs.com/statsd.aspx?playerid=12861&position=2B/3B - click game log tab and then export button

##Load El Tidyverse
##library(tidyverse)

## Read in season stats CSV
season <- read.csv("csv/season-index.csv", header=T)
View(season)

##Let's transform the data.  Make a copy of the name column, called full name, using mutate.  And then lets move the column to the beginning using select. And then let's split the name column into first and last, using separate, with a delimiter of a space.  And then lets clean up by getting rid of the first name column, selecting all columns but that. 

season <- season %>%
  mutate(fullname = Name) %>%
  separate(Name, c('first', 'last'), sep = " ") %>%
  mutate(last = tolower(last)) %>%
  mutate(filename = last) %>%
  mutate(folder = "csv/") %>%
  mutate(suffix = ".csv") %>%
  select(fullname, G:playerid, folder, filename, suffix) 
unite("filepath", folder, filename, suffix, sep="") %>%
  
  
  View(season)
rm(season)

