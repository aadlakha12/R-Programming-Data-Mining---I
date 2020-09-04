##############################################################
# Homework 5 Problem 1
##############################################################

# loading libararies nnet,caret,ElemStatLearn and gam
library(nnet)
library(caret)
library(ElemStatLearn)
library(gam)

#importing spam dataset
data("spam")
colnames(spam)[58] = "spam"

#splitting data into training and testing
trainIndex = sample(1:nrow(spam), nrow(spam)*0.80)
testIndex = -trainIndex
trainData = spam[trainIndex, ]
testData = spam[testIndex, ]

n<-names(trainData)

# controlling method
controlMethod = trainControl(method = "cv",number = 5)
controlMethod1 = trainControl(method = "loocv",number = 5)

# model 
nn_model = train(spam ~ ., 
                   data= trainData, 
                   method="nnet",
                   trControl=controlMethod)

nn_model1 = train(spam ~ ., 
                 data= trainData, 
                 method="nnet",
                 trControl=controlMethod1)

# predictions
nn_pred = predict(nn_model, testData[,-58])
nn_pred1 = predict(nn_model1, testData[,-58])

# confusion matix
confusionMatrix(nn_pred, testData $spam)
confusionMatrix(nn_pred1, testData $spam)