#e1071 package has a nice function for SVM called tune.svm(), which assists in
#the selection of the tuning parameters/kernel functions. The tune.svm() function
#from the package uses cross-validation to optimize the tuning parameters

library(e1071) #svm
svmkernel<-function(kernelName, trainingData){
svm.tune <- tune.svm(click ~ ., data = trainingData,
                            kernel = kernelName,
                            cost = 1, scale=FALSE,gamma=1)
summary(svm.tune)
best.tune <- svm.tune$best.model
return(best.tune)
}