library(readr) # Reading data file
library(caret)
source("~/INSE6180_Project/base/SVM.R")
source("~/INSE6180_Project/base/DecisionTree.R")
source("~/INSE6180_Project/base/LogisticRegression.R")
source("~/INSE6180_Project/base/NaiveBayes.R")
source("~/INSE6180_Project/preprocessing/Preprocessing.R")


dat <- read_csv("~/INSE6180_Project/data/training10k.csv")

dat <- preprocessing(dat)

# Split data into two sets - Training and Testing
set.seed(100)


inTrain <- sample(2, nrow(dat), replace=TRUE, prob=c(0.7, 0.3))

training <- dat[ inTrain==1,]
testing <- dat[inTrain==2,]
rm(inTrain)

dt.model<-decisionTree(training)
lr.model<-logisticRegression(training)
nb.model<-naiveBayesFn(training)
svmsigmoid.model<-svmkernel("sigmoid",training)
svmradial.model<-svmkernel("radial",training)
svmlinear.model<-svmkernel("linear",training)

dt.pred <- predict(dt.model, newdata = testing)
lr.pred <- predict(lr.model,testing, type = 'response')
for(i in 1:length(lr.pred)){
  lr.pred[i]<-ifelse(lr.pred[i]>0.5,"Yes","No")
}
nb.pred <- predict(nb.model, testing, type="class")
svmsigmoid.pred <- predict(svmsigmoid.model, newdata = testing)
svmradial.pred <- predict(svmradial.model, newdata = testing)
svmlinear.pred <- predict(svmlinear.model, newdata = testing)

confusionMatrix(dt.pred, testing$click)
confusionMatrix(lr.pred, testing$click)
confusionMatrix(nb.pred, testing$click)
confusionMatrix(svmsigmoid.pred, testing$click)
confusionMatrix(svmradial.pred, testing$click)
confusionMatrix(svmlinear.pred, testing$click)

SVMvote<-NULL
SVMvote <- as.factor(SVMvote)
levels(SVMvote) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(svmlinear.pred)){
  SVMvote[i]<-ifelse(which.max(table(c(svmlinear.pred[i],svmradial.pred[i],svmsigmoid.pred[i])))==1,"No","Yes")
}
SVMvote
confusionMatrix(SVMvote,testing$click)


DTLRNBvote<-NULL
DTLRNBvote <- as.factor(DTLRNBvote)
levels(DTLRNBvote) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(dt.pred)){
  DTLRNBvote[i]<-ifelse(which.max(table(c(dt.pred[i],lr.pred[i],nb.pred[i])))==1,"No","Yes")
}
DTLRNBvote
confusionMatrix(DTLRNBvote,testing$click)

vote<-NULL
vote <- as.factor(vote)
levels(vote) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(svmlinear.pred)){
  vote[i]<-ifelse(which.max(table(c(svmlinear.pred[i],svmradial.pred[i],svmsigmoid.pred[i],
                                    dt.pred[i],lr.pred[i],nb.pred[i])))==1,"No","Yes")
}
vote
confusionMatrix(vote,testing$click)


#naive bayes without library
ourNB.model = trainNaiveBayes("click", training)
ourNB.pred = predictNaiveBayes(ourNB.model, testing)

# Find Max Result
ourNB.predcls <- gl(1, length(ourNB.pred)/2, labels = c("No","Yes"))
for(i in 1:length(ourNB.predcls)){
  ourNB.predcls[i]<-ifelse(ourNB.pred[i,"No"]>ourNB.pred[i,"Yes"],"No","Yes")
}
confusionMatrix(ourNB.predcls,testing$click)

DTLROurNBvote<-NULL
DTLROurNBvote <- as.factor(DTLROurNBvote)
levels(DTLROurNBvote) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(dt.pred)){
  DTLROurNBvote[i]<-ifelse(which.max(table(c(dt.pred[i],lr.pred[i],ourNB.pred[i])))==1,"No","Yes")
}
DTLROurNBvote
confusionMatrix(DTLROurNBvote,testing$click)

