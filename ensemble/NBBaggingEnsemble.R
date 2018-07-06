library(readr) # Reading data file
library(caret)
source("~/INSE6180_Project/base/NaiveBayes.R")
source("~/INSE6180_Project/base/OurNaiveBayes.R")
source("~/INSE6180_Project/preprocessing/Preprocessing.R")
dat <- read_csv("~/INSE6180_Project/data/training10k.csv")


dat <-preprocessing(dat)

# Split data into two sets - Training and Testing
set.seed(107)

inTrain <- createDataPartition(y = dat$click, p = .7, list = FALSE)

training <- dat[ inTrain,]
testing <- dat[-inTrain,]

set.seed(200)

inTrain1 <- createDataPartition(y = training$click, p = .9, list = FALSE)
training1 <- dat[ inTrain1,]

set.seed(300)

inTrain2 <- createDataPartition(y = training$click, p = .9, list = FALSE)
training2 <- dat[ inTrain2,]

set.seed(400)
inTrain3 <- createDataPartition(y = training$click, p = .9, list = FALSE)
training3 <- dat[ inTrain3,]

set.seed(500)
inTrain4 <- createDataPartition(y = training$click, p = .9, list = FALSE)
training4 <- dat[ inTrain4,]

set.seed(600)
inTrain5 <- createDataPartition(y = training$click, p = .9, list = FALSE)
training5 <- dat[ inTrain5,]

#training data to get naive bayes model with library
nb1.model<-naiveBayesFn(training1)
nb2.model<-naiveBayesFn(training2)
nb3.model<-naiveBayesFn(training3)
nb4.model<-naiveBayesFn(training4)
nb5.model<-naiveBayesFn(training5)

#training data to get naive bayes model without library
ournb1.model<-trainNaiveBayes("click",training1)
ournb2.model<-trainNaiveBayes("click",training2)
ournb3.model<-trainNaiveBayes("click",training3)
ournb4.model<-trainNaiveBayes("click",training4)
ournb5.model<-trainNaiveBayes("click",training5)

#to get all classes of new data for naive bayes with library
nb1.pred <- predict(nb1.model, testing, type="class")
nb2.pred <- predict(nb2.model, testing, type="class")
nb3.pred <- predict(nb3.model, testing, type="class")
nb4.pred <- predict(nb4.model, testing, type="class")
nb5.pred <- predict(nb5.model, testing, type="class")

#to get all classes of new data for naive bayes without library
ourNb1.pred<-predictNaiveBayes(ournb1.model,testing)
ourNb1.predcls <- gl(1, length(ourNb1.pred)/2, labels = c("No","Yes"))
for(i in 1:length(ourNb1.predcls)){
  ourNb1.predcls[i]<-ifelse(ourNb1.pred[i,"No"]>ourNb1.pred[i,"Yes"],"No","Yes")
}

ourNb2.pred<-predictNaiveBayes(ournb2.model,testing)
ourNb2.predcls <- gl(1, length(ourNb2.pred)/2, labels = c("No","Yes"))
for(i in 1:length(ourNb2.predcls)){
  ourNb2.predcls[i]<-ifelse(ourNb2.pred[i,"No"]>ourNb2.pred[i,"Yes"],"No","Yes")
}

ourNb3.pred<-predictNaiveBayes(ournb3.model,testing)
ourNb3.predcls <- gl(1, length(ourNb3.pred)/2, labels = c("No","Yes"))
for(i in 1:length(ourNb3.predcls)){
  ourNb3.predcls[i]<-ifelse(ourNb3.pred[i,"No"]>ourNb3.pred[i,"Yes"],"No","Yes")
}

ourNb4.pred<-predictNaiveBayes(ournb4.model,testing)
ourNb4.predcls <- gl(1, length(ourNb4.pred)/2, labels = c("No","Yes"))
for(i in 1:length(ourNb4.predcls)){
  ourNb4.predcls[i]<-ifelse(ourNb4.pred[i,"No"]>ourNb4.pred[i,"Yes"],"No","Yes")
}

ourNb5.pred<-predictNaiveBayes(ournb5.model,testing)
ourNb5.predcls <- gl(1, length(ourNb5.pred)/2, labels = c("No","Yes"))
for(i in 1:length(ourNb5.predcls)){
  ourNb5.predcls[i]<-ifelse(ourNb5.pred[i,"No"]>ourNb5.pred[i,"Yes"],"No","Yes")
}

# confusion matrix of naive bayes models with library for naive bayes bagging
confusionMatrix(nb1.pred, testing$click)
confusionMatrix(nb2.pred, testing$click)
confusionMatrix(nb3.pred, testing$click)
confusionMatrix(nb4.pred, testing$click)
confusionMatrix(nb5.pred, testing$click)

# confusion matrix of naive bayes models without library for naive bayes bagging 
confusionMatrix(ourNb1.predcls, testing$click)
confusionMatrix(ourNb2.predcls, testing$click)
confusionMatrix(ourNb3.predcls, testing$click)
confusionMatrix(ourNb4.predcls, testing$click)
confusionMatrix(ourNb5.predcls, testing$click)

# with library
nbBagging<-NULL
nbBagging <- as.factor(nbBagging)
levels(nbBagging) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(nb1.pred)){
  nbBagging[i]<-ifelse(which.max(table(c(nb1.pred[i],nb2.pred[i],nb3.pred[i],nb4.pred[i],nb5.pred[i])))==1,"No","Yes")
}
nbBagging

#without library
ourNBbagging<-NULL
ourNBbagging <- as.factor(ourNBbagging)
levels(ourNBbagging) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(ourNb1.predcls)){
  ourNBbagging[i]<-ifelse(which.max(table(c(ourNb1.predcls[i],ourNb2.predcls[i],ourNb3.predcls[i],ourNb4.predcls[i],ourNb5.predcls[i])))==1,"No","Yes")
}
ourNBbagging

#confusion matrix of naive bayes with library
confusionMatrix(nbBagging,testing$click)

#confusion matrix of naive bayes without library
confusionMatrix(ourNBbagging,testing$click)

