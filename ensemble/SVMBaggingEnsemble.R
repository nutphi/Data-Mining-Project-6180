library(caret) # use for createDataPartition
library(readr) # Reading data file
source("~/INSE6180_Project/base/SVM.R")
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

svmradial1.model<-svmkernel("radial",training1)
svmradial2.model<-svmkernel("radial",training2)
svmradial3.model<-svmkernel("radial",training3)
svmradial4.model<-svmkernel("radial",training4)
svmradial5.model<-svmkernel("radial",training5)

svmradial1.pred <- predict(svmradial1.model, newdata = testing)
svmradial2.pred <- predict(svmradial2.model, newdata = testing)
svmradial3.pred <- predict(svmradial3.model, newdata = testing)
svmradial4.pred <- predict(svmradial4.model, newdata = testing)
svmradial5.pred <- predict(svmradial5.model, newdata = testing)

confusionMatrix(svmradial1.pred, testing$click)
confusionMatrix(svmradial2.pred, testing$click)
confusionMatrix(svmradial3.pred, testing$click)
confusionMatrix(svmradial4.pred, testing$click)
confusionMatrix(svmradial5.pred, testing$click)

SVMbagging<-NULL
SVMbagging <- as.factor(SVMbagging)
levels(SVMbagging) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(svmradial1.pred)){
  SVMbagging[i]<-ifelse(which.max(table(c(svmradial1.pred[i],svmradial2.pred[i],svmradial3.pred[i],svmradial4.pred[i],svmradial5.pred[i])))==1,"No","Yes")
}
SVMbagging
confusionMatrix(SVMbagging,testing$click)
