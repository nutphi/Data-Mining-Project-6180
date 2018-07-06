#stacking ensemble (does not work)
install.packages('pROC')
install.packages('caretEnsemble')
library(caret) # use for createDataPartition
library(caretEnsemble) # use for train
install.packages('package:ggplot2')
library(rpart) #rpart tree

library(pROC)

#library(kernlab)       # support vector machine 
# Reading data file
library(readr)
source("~/INSE6180_Project/preprocessing/Preprocessing.R")
dat <- read_csv("~/INSE6180_Project/data/training10k.csv")

dat <-preprocessing(dat)
#dat[is.na(dat)] <- 0


# Split data into two sets - Training and Testing
set.seed(107)

inTrain <- createDataPartition(y = dat$click, p = .7, list = FALSE)

training <- dat[ inTrain,]
testing <- dat[-inTrain,]

#Training control
#to build the object that specifies how model training is going to happen
#create a 10-fold cross-validation and save the final predictions (the probabilities)
#a classifier that is highly accurate
#but does a horrible job at predicting the outcome of interest, which is to say it doesn't predict any true positives.
#To balance the response, you can upsample the minority class,
#downsample the majority class, or create "synthetic data".

control <- trainControl(method = "cv",
                        number = 5,
                        savePredictions = "final",
                        classProbs = T,
                        index=createResample(training$click, 5),
                        sampling = "up",
                        summaryFunction = twoClassSummary)

algorithmList <- c('rpart', 'glm','nb')

#To balance the response, you can upsample the minority class,
#downsample the majority class, or create "synthetic data"
#next step...
models <- caretList(
  click~., data = testing,
  trControl = control,
  metric = "ROC",
  methodList = algorithmList
)
results <- resamples(models)


#result of 3 models
modelCor(resamples(models))


p <- as.data.frame(predict(models, newdata=head(testing)))
print(p)




#stack model
# To do this, we will capture the predicted probabilities for "Yes" on the test set in a dataframe:
model_preds <- lapply(models, predict, newdata=testing, type="prob")
#model_preds <- lapply(model_preds, function(x) x[,"Yes"])
model_preds <- data.frame(model_preds)

model_preds<-model_preds[ , !(names(model_preds) %in% c("vote"))]

for(i in 1:length(model_preds$rpart)){
  model_preds$vote[i]<-which.max(table(c(model_preds$rpart[i],model_preds$glm[i],model_preds$knn[i])))
}
model_preds$vote <- as.factor(model_preds$vote)

model_preds$vote <- factor(model_preds$vote,labels = c("No", "Yes"))
model_preds$vote

levels(model_preds$vote) = list(No=c("0"),Yes=c("1"))
confusionMatrix(model_preds$rpart,testing$click)
confusionMatrix(model_preds$glm,testing$click)
confusionMatrix(model_preds$knn,testing$click)

confusionMatrix(model_preds$vote,testing$click)
stack <- caretStack(models, method = "glm",
                    metric = "Accuracy",
                    trControl = trainControl(
                      method = "boot",
                      number = 5,
                      savePredictions = "final",
                      classProbs = TRUE,
                      summaryFunction = twoClassSummary
                    ))

prob <- 1-predict(stack, newdata = testing, type = "prob")
model_preds$stack <- ifelse(prob>0.5,"Yes","No")
model_preds$stack<-prob
library(caTools)
model_preds
colAUC(model_preds, testing$click)
