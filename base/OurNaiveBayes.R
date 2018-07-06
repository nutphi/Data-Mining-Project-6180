
trainNaiveBayes <- function(target, data) {
  # y - target values
  y <- data[,target]
  # classes -  level of target class
  classes <- levels(y)
  # k - number of classes
  k <- length(classes)
  # n - number of rows
  n <- nrow(data)
  #name of all features
  featurenames <- setdiff(colnames(data), target)
  #data of all features separating column by feature names
  features <- data[, featurenames]
  # result
  priors <- setNames(numeric(k), classes)
  probs <- vector("list", k)
  
  for (class in classes) {
    # get features from click "Yes", "No"
    features2 <- features[y == class,]
    # size of features
    priors[class] <- nrow(features2) / n
    # for each feature calculate proportions of levels
    probs[[class]] <- lapply(features2, function(x) {
      p <- prop.table(table(x))
      p <- setNames(as.numeric(p), names(p))
      # if there are 0 entries replace them by threshold (0.001)
      ifelse(p == 0, 0.001, p)
    })
  }
  list(classes=classes, target=target, priors=priors, probs=probs)
}

predictNaiveBayes <- function(model, newdata) {
  n <- nrow(newdata)
  labels <- levels(newdata[, model$target])
  newdata[, model$target] = NULL
  classes <- model$classes
  # result object
  pred = matrix(NA, nrow=n, ncol=length(classes))
  colnames(pred) = classes

  # loop for all rows of new data
  for (i in 1:nrow(newdata)) {
    # feature vector as char vector
    x <- sapply(newdata[i,], as.character)
    # to get prob for each class
    for (class in classes) {
      # index our prob tables
      p <- model$probs[[class]]
      q <- sapply(names(x), function(n) p[[n]][x[n]])
      # multiply probs and prior prob
      pred[i, class] <- prod(q, na.rm=TRUE) * model$priors[[class]]
    }
  }
  
  # normalize
  pred <- t(apply(pred, 1, function(x) x/sum(x)))
  return(pred)
}

# ourNaiveBayes.model = trainNaiveBayes("click", training)
# ourNaiveBayes.pred = predictNaiveBayes(ourNaiveBayes.model, testing)

# Find Max Result
# ourNaiveBayes.predcls <- gl(1, length(ourNaiveBayes.pred)/2, labels = c("No","Yes"))
# for(i in 1:length(ourNaiveBayes.predcls)){
#  ourNaiveBayes.predcls[i]<-ifelse(ourNaiveBayes.pred[i,"No"]>ourNaiveBayes.pred[i,"Yes"],"No","Yes")
# }
# confusionMatrix(ourNaiveBayes.predcls,testing$click)