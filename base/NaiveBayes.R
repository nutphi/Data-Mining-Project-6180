#The naiveBayes function takes in numeric or factor variables in a data frame or a numeric matrix.
#It's important to note that single vectors will not work for the input data but will work for
#the dependent variable (Y).
library(e1071)
naiveBayesFn<-function(training){
  model<-naiveBayes(click~.,data=training,threshold=0.001)
  return(model)
}