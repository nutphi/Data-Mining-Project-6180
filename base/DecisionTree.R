
library(party)
decisionTree<-function(trainingData){
  model<-ctree(click~., data = trainingData)
  return(model)
}