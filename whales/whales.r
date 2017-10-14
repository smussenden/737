## Load the tidyverse, so we can work with dplyr, ggplot2, readr, and also the ggplot themes package ggthemes 
library('tidyverse')
library(ggthemes)
library(scales)

## Two CSVs here. klavern.csv has name, city, year and location details for each "klavern".  state.csv has codes for each states.  The other one - all_sans_sources - has both put together.  I'm going to not work with that one, so I can do the joins. Also not going to work with the sources table. Load in the data. Note that I'm using tidyverse's readr instead of read.csv, which is default R. 

crew <- read_csv("crewlist.csv", col_names = TRUE)
