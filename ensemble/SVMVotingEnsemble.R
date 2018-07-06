library(readr) # Reading data file
source("~/INSE6180_Project/base/SVM.R")
source("~/INSE6180_Project/preprocessing/Preprocessing.R")

dat <- read_csv("~/INSE6180_Project/data/training10k.csv")

dat <- preprocessing(dat)

# Split data into two sets - Training and Testing
set.seed(100)


inTrain <- sample(2, nrow(dat), replace=TRUE, prob=c(0.7, 0.3))

training <- dat[ inTrain==1,]
testing <- dat[inTrain==2,]

svmsigmoid.test<-svmkernel("sigmoid",training,testing)
confusionMatrix(svmsigmoid.test, testing$click)


svmradial.test<-svmkernel("radial",training,testing)
confusionMatrix(svmradial.test, testing$click)

svmlinear.test<-svmkernel("linear",training,testing)
confusionMatrix(svmlinear.test, testing$click)

vote<-NULL
vote <- as.factor(vote)
levels(vote) = list(No=c("1"),Yes=c("2"))
for(i in 1:length(svmlinear.test)){
  vote[i]<-ifelse(which.max(table(c(svmsigmoid.test[i],svmradial.test[i],svmlinear.test[i])))==1,"No","Yes")
}
vote
confusionMatrix(vote,testing$click)
