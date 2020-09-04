#########################################################
# Homework 4 Problem 4
#########################################################

rm(list = ls())

# loading library kernlab
library(kernlab)

data("spam")

#generating training and testing dataset. 
set.seed(1234)
Index = sample(1:nrow(spam), 0.8*nrow(spam))
train = spam[Index,]
test = spam[-Index,]

# loading packages randomForest and MLmetrics
library(randomForest)
library(MLmetrics)
mlist = c()
alist = c()

for(i in seq(5,55,5)){
model = randomForest(type~., spam, ntree = 100, mtry = i)
modelPred = predict(model, test)
mlist = append(mlist, i)
alist = append(alist, MLmetrics::Accuracy(modelPred, test$type))
print(i)
}

alist

# Choosing m value to explore and plotting. 
mlist = c()
OOBlist = rep(0,100)
testError = c()
#select the best m among the choosed values of m. 
for(i in c(2,4,6,8,10)){
  model = randomForest(type~., spam, ntree = 100, mtry = i)
  modelPred = predict(model, test) 
  OOBlist = cbind(OOBlist, model$err.rate[,c(1)])
  mlist = append(mlist, i)
  testError = append(testError, MLmetrics::Accuracy(modelPred, test$type))
  print(i)
}

#creating data frame for plotting errors.
dataPlot = data.frame(mlist, testError)
#giving names to columns
names(dataPlot) = c("mValue", "accuracy")

# loading library ggplot2 for plotting errors.
library(ggplot2)
#plotting the value of m and accuracy of model.
ggplot(dataPlot, aes(x = mValue, y = accuracy)) + geom_line() + geom_point() +
  ggtitle("MValue vs Accuracy") + theme(plot.title = element_text(hjust = 0.5))

# Creating data frame of OOBlist
plotDataOOB = data.frame(OOBlist)[,-c(1)]
names(plotDataOOB) = c("mValue2", "mValue4","mValue6","mValue8","mValue10")
plotDataOOB$NTrees = seq(1, 100)
#plotting plotDataOOB using ggplot function
ggplot(plotDataOOB, aes(NTrees)) + 
  geom_line(aes(y = mValue2, colour = "M Value 2")) + geom_point(aes(y = mValue2), size = 0.3) + 
  geom_line(aes(y = mValue4, colour = "M Value 4")) + geom_point(aes(y = mValue4), size = 0.3) + 
  geom_line(aes(y = mValue6, colour = "M Value 6")) + geom_point(aes(y = mValue6), size = 0.3) + 
  geom_line(aes(y = mValue8, colour = "M Value 8")) + geom_point(aes(y = mValue8), size = 0.3) + 
  geom_line(aes(y = mValue10, colour = "M Value 10")) + geom_point(aes(y = mValue10), size = 0.3) + 
  ggtitle("Plot of Out of Bag Errors.") + xlab("Number of Features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))
