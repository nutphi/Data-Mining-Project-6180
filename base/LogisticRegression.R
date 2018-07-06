

logisticRegression<-function(training){
model <- glm (click~., data = training, family = binomial)
return(model)
}