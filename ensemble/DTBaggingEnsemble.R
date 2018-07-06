library(readr) # Reading data file
library(caret) # createDataPartition
source("~/INSE6180_Project/base/DecisionTree.R")
source("~/INSE6180_Project/preprocessing/Preprocessing.R")
dat <- read_csv("~/INSE6180_Project/data/training10k.csv")


dat <-preprocessing(dat)

# Split data into two sets - Training and Testing
set.seed(107)

inTrain <- createDataPartition(y = dat$click, p = .7, list = FALSE)

training <- dat[inTrain,]
testing <- dat[-inTrain,]

set.seed(200)

inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training1 <- dat[inTrain,]

set.seed(300)

inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training2 <- dat[inTrain,]

set.seed(400)
inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training3 <- dat[ inTrain,]

set.seed(500)
inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training4 <- dat[ inTrain,]

set.seed(600)
inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training5 <- dat[ inTrain,]

rm(inTrain)

dt1.model<-decisionTree(training1)
dt2.model<-decisionTree(training2)
dt3.model<-decisionTree(training3)
dt4.model<-decisionTree(training4)
dt5.model<-decisionTree(training5)

dt1.pred <- predict(dt1.model, newdata = testing)
dt2.pred <- predict(dt2.model, newdata = testing)
dt3.pred <- predict(dt3.model, newdata = testing)
dt4.pred <- predict(dt4.model, newdata = testing)
dt5.pred <- predict(dt5.model, newdata = testing)

confusionMatrix(dt1.pred, testing$click)
confusionMatrix(dt2.pred, testing$click)
confusionMatrix(dt3.pred, testing$click)
confusionMatrix(dt4.pred, testing$click)
confusionMatrix(dt5.pred, testing$click)


dtBagging<-NULL
dtBagging <- as.factor(dtBagging)
levels(dtBagging) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(dt1.pred)){
  dtBagging[i]<-ifelse(which.max(table(c(dt1.pred[i],dt2.pred[i],dt3.pred[i],dt4.pred[i],dt5.pred[i])))==1,"No","Yes")
}
dtBagging
confusionMatrix(dtBagging,testing$click)
