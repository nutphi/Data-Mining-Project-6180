- we use library
library(caret) # for createPartition and confusionMatrix function
library(readr) # to read csv file from our dataset
library(e1071) # for svm and naive bayes
library(party) # for decision tree

- after that, we need to load workspace from INSE6180_project_workspace.RData

- to run our algorithm

1. for base algorithm with the whole dataset
- naive bayes, decision tree, logistic regression and svm
# we can run on VotingEnsemble.R

2. for ensemble algorithm
- Bagging Decision Tree
# we run it on DTBaggingEnsemble.R

- Bagging Logistic Regression
# we run it on LRBaggingEnsemble.R

- Bagging Naive Bayes with and without library
# we run them on NBBaggingEnsemble.R

- Bagging SVM
# we run it on SVMBaggingEnsemble.R

- voting with 3 different SVM kernels
- voting with decision tree, naive bayes, and logistic regression
- voting with 3 different SVM kernels, decision tree, naive bayes, and logistic regression
# we run them on VotingEnsemble.R