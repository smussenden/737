##Read in the NYT Data (uncomment if running again)
nyt = read.csv(file.choose(), header=T)

## Create a single column that assigns an age-range category to each observation, under 18, 18-24, 25-34, 35-44, 45-54, 55-64,65+. Start at -1 so we include zero.
nyt = cbind(nyt, agegroup = cut(nyt$Age, breaks=c(-1,18,24,34,44,54,64,Inf), labels=c("<18", "18-24", "25-34", "35-44", "45-54", "55-64", "65+")), clickgroup = cut(nyt$Clicks, breaks=c(-1,0,Inf), labels= c("No Clicks", "Some Clicks")))

##load ggplot
library(ggplot2)

##Create a small multiples histogram
ggplot(data=nyt, aes(x=Impressions)) +
  geom_histogram(binwidth = 1) +
  facet_grid(agegroup~.) + 
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0)) 

##Create a small multiples histogram
THIS IS NOT RIGHT...CTR = click through rate
ggplot(data=nyt, aes(x=Clicks)) +
  geom_histogram(binwidth = 1) +
  facet_grid(agegroup~.) + 
  theme(plot.title = element_text(family="Trebuchet MS", face="bold", size=20, hjust=0, color="#555555")) +
  theme(axis.text.x = element_text(angle=0)) 
