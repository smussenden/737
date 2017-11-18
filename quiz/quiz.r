

# Create a logistic regression model (because)
model <-glm(corr ~ body_score, data=train, family=binomial(link='logit'))
summary(model)
exp(coef(model))
library(e1071)
cov(train$body_score,as.double(train$corr))
# When comparing models fitted by maximum likelihood to the same data, the smaller the AIC or BIC, the better the fit. look at AIC.  
AIC(model)
# https://www.r-bloggers.com/evaluating-logistic-regression-models/
library(pscl)
pR2(model) 
results <- record_performance(results, "body_score", glm(corr ~ body_score, data=train, family=binomial), testset)
results

the function parent_match correction in the feature_expansion.R code

There is a mistake in the function parens_match.

at the line:
  
  substring(text,
            
            should be:
              
              substring(page,
                        
                        because you want what is in the parens to see if it exists in the text.
                        
                        
seudo R^2

Unlike linear regression with ordinary least squares estimation, there is no R2 statistic which explains the proportion of variance in the dependent variable that is explained by the predictors. However, there are a number of pseudo R2 metrics that could be of value. Most notable is McFadden’s R2, which is defined as 1−[ln(LM)/ln(L0)] where ln(LM) is the log likelihood value for the fitted model and ln(L0) is the log likelihood for the null model with only an intercept as a predictor. The measure ranges from 0 to just under 1, with values closer to zero indicating that the model has no predictive power.

library(pscl)
pR2(mod_fit_one)  # look for 'McFadden'
##           llh       llhNull            G2      McFadden          r2ML 
## -344.42107079 -366.51858123   44.19502089    0.06029029    0.07101099 
##          r2CU 
##    0.10068486

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

logit2prob(coef(model))



paren_match <- function(page, text) {
  start <- cpos(page, "(")
  end <- cpos(page, ")")
  if (!is.na(start) && !is.na(end)) {
    search <- substring(text, start + 1, end - 1)
    return(grepl(tolower(search), tolower(text), fixed=TRUE))
  } else {
    return(FALSE)
  }
}

full <- read.csv("csv/qb.train.csv")

full$obs_len <- apply(full, 1, function(x) {nchar(x['text'])})
full$scale_len <- scale(full$obs_len)

full$scale_score <- scale(full$body_score)

full$paren_match <- apply(full, 1, function(x) {paren_match(x['page'], x['text'])})

full$log_links <- scale(log(as.numeric(full$inlinks) + 1))

index <- 1:nrow(full)
testindex <- sample(index, trunc(length(index)/5))
testset <- full[testindex,]
trainset <- full[-testindex,]

# Get the most frequent baseline
mfc_baseline <- sum(testset$corr == "False") / nrow(testset)
results <- data.frame(model=c("MFC"), score=c(mfc_baseline))

results <- record_performance(results, "body_score", svm(corr ~ body_score, data=trainset), testset)
results <- record_performance(results, "scale_score", svm(corr ~ scale_score, data=trainset), testset)
results <- record_performance(results, "obs_len", svm(corr ~ obs_len, data=trainset), testset)
results <- record_performance(results, "score+len", svm(corr ~ obs_len + body_score, data=trainset), testset)
results <- record_performance(results, "paren+len", svm(corr ~ obs_len + paren_match, data=trainset), testset)
results <- record_performance(results, "paren_match", svm(corr ~ paren_match, data=trainset), testset)
results <- record_performance(results, "score+paren_match", svm(corr ~ scale_score + paren_match, data=trainset), testset)
results <- record_performance(results, "score+len+paren_match", svm(corr ~ scale_len + scale_score + paren_match, data=trainset), testset)
results <- record_performance(results, "links", svm(corr ~ inlinks, data=trainset), testset)
results <- record_performance(results, "loglinks", svm(corr ~ log_links, data=trainset), testset)
results <- record_performance(results, "score+len+links+paren_match", svm(corr ~ scale_len + scale_score + log_links + paren_match, data=trainset), testset)
results <- record_performance(results, "score+links+paren_match", svm(corr ~ scale_len + scale_score + paren_match, data=trainset), testset)
results