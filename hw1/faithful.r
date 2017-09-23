## Load library datasets to load the Old Faithful datasets
library(datasets)

## plot the old faithful database 
plot(faithful$eruptions, faithful$waiting, xlab="eruption", ylab="waiting", pch=16)

##fit a linear model to the data, to tell us given the size of an eruption, how long we should have waited
fit.lm = lm(faithful$waiting ~ faithful$eruptions)
##plot a line on our graph based on our linear model (line of best fit)
abline(fit.lm,col="red")
##run the 
names(fit.lm)
