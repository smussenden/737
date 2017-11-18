library(e1071)

record_performance <- function(df, name, model, test) {
  svm.pred <- predict(model, test, type='class')
  svm.table <- table(pred = svm.pred, true=test$corr)
  df <- rbind(df, data.frame(model=c(name), score=c(classAgreement(svm.table)$diag)))
  return(df)
}

paren_match <- function(page, text) {
  start <- cpos(page, "(")
  end <- cpos(page, ")")
  if (!is.na(start) && !is.na(end)) {
    search <- substring(page, start + 1, end - 1)
    return(grepl(tolower(search), tolower(text), fixed=TRUE))
  } else {
    return(FALSE)
  }
}

full <- read.csv("qb.train.csv")

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