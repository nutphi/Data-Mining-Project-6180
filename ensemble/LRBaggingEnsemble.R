library(caret)# used for createDataPartition
library(readr) # Reading data file
source("~/INSE6180_Project/base/LogisticRegression.R")
source("~/INSE6180_Project/preprocessing/Preprocessing.R")
dat <- read_csv("~/INSE6180_Project/data/training10k.csv")


dat <-preprocessing(dat)

# Split data into two sets - Training and Testing
set.seed(107)

inTrain <- createDataPartition(y = dat$click, p = .7, list = FALSE)

training <- dat[ inTrain,]
testing <- dat[-inTrain,]

set.seed(200)
inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training1 <- dat[ inTrain,]

set.seed(300)
inTrain <- createDataPartition(y = training$click, p = .9, list = FALSE)
training2 <- dat[ inTrain,]

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

lr1.model<-logisticRegression(training1)
lr2.model<-logisticRegression(training2)
lr3.model<-logisticRegression(training3)
lr4.model<-logisticRegression(training4)
lr5.model<-logisticRegression(training5)

lr1.pred <- predict(lr1.model,testing, type = 'response')
for(i in 1:length(lr1.pred)){
  lr1.pred[i]<-ifelse(lr1.pred[i]>0.5,"Yes","No")
}
lr2.pred <- predict(lr2.model,testing, type = 'response')
for(i in 1:length(lr2.pred)){
  lr2.pred[i]<-ifelse(lr2.pred[i]>0.5,"Yes","No")
}
lr3.pred <- predict(lr3.model,testing, type = 'response')
for(i in 1:length(lr3.pred)){
  lr3.pred[i]<-ifelse(lr3.pred[i]>0.5,"Yes","No")
}
lr4.pred <- predict(lr4.model,testing, type = 'response')
for(i in 1:length(lr4.pred)){
  lr4.pred[i]<-ifelse(lr4.pred[i]>0.5,"Yes","No")
}

lr5.pred <- predict(lr5.model,testing, type = 'response')
for(i in 1:length(lr5.pred)){
  lr5.pred[i]<-ifelse(lr5.pred[i]>0.5,"Yes","No")
}

confusionMatrix(lr1.pred, testing$click)
confusionMatrix(lr2.pred, testing$click)
confusionMatrix(lr3.pred, testing$click)
confusionMatrix(lr4.pred, testing$click)
confusionMatrix(lr5.pred, testing$click)

lrBagging<-NULL
lrBagging <- as.factor(lrBagging)
levels(lrBagging) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(lr1.pred)){
  lrBagging[i]<-ifelse(which.max(table(c(lr1.pred[i],lr2.pred[i],lr3.pred[i],lr4.pred[i],lr5.pred[i])))==1,"No","Yes")
}
lrBagging
confusionMatrix(lrBagging,testing$click)
