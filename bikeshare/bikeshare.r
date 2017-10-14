## Working with capital bikeshare data https://www.capitalbikeshare.com/system-data

## load the tidyverse
library(tidyverse)

## First, save this environment as an R project - rproj file -- and put it in the Github repo I set up for this project. When I do that, it should automatically set the working directory to the Github repo.  I can confirm that with the getwd() -- get working directory -- command. 
getwd()

## Remove all data objects from working memory
rm(list = ls())

## Load the tidyverse, so we can work with dplyr, ggplot2, readr, and also the ggplot themes package ggthemes 
library('tidyverse')
library(ggthemes)
library(scales)

## Read in the data 
Q12017 <- read_csv("csv/2017-Q1-Trips-History-Data.csv", col_names = TRUE)
Stations <- read_csv("csv/stations.csv", col_names = TRUE)

blankstations <- Stations %>% 
  filter(`Start station` == NULL)
View(blankstations)
