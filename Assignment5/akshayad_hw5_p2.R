########################################################
# Homework 5 Problem 2
########################################################

rm(list = ls())

# loading dataset 
data = read.csv("voice.csv")
set.seed(1)

# checking if data has NA values
sum(is.na(data))

# data splitted into training and testing
index = sample(1:nrow(data), 0.8*nrow(data))
train = data[index,]
test = data[-index,]
trainingOutlier = train

set.seed(123)
#getting the rows and columns of dataset.
row = sample(1:nrow(data),1)
column = sample(1:ncol(data)-1,1) 

# plotting boxplot 
boxplot(train[,c(16)], main = "Boxplot")
trainingOutlier[row, column] = 5
boxplot(trainingOutlier[,c(16)], main = "Outlier's Boxplot")


# impoorting libraries h2o and Mlmetrices
library(h2o)
library(MLmetrics)
localh2o = h2o.init(ip='localhost', port = 54321, max_mem_size = '6g',nthreads = 1)

# training data
trainHex = as.h2o(train)
testHex = as.h2o(test)

#checking the count number of neurons are best 
acc = c()
for(i in seq(1,20,1)){
neuralModel = h2o.deeplearning(x = setdiff(colnames(trainHex), "label"), 
                       y = "label", training_frame = trainHex,
                       activation = "RectifierWithDropout",
                       hidden = c(i), epochs = 100)
prediction = as.data.frame(h2o.predict(neuralModel, newdata = testHex[,-c(21)]))$predict
accuracy = MLmetrics::Accuracy(prediction, test$label)
acc = append(acc, accuracy)
print(i)
}
acc

#The outlier's train data. 
trainingOutlier.hex = as.h2o(trainingOutlier)

#checking the count how many neurons are best 
acc = c()
for(i in seq(1,20,1)){
  neuralModel = h2o.deeplearning(x = setdiff(colnames(trainingOutlier.hex), "label"), 
                              y = "label", training_frame = trainingOutlier.hex,
                              activation = "RectifierWithDropout",
                              hidden = c(i), epochs = 100)
  prediction = as.data.frame(h2o.predict(neuralModel, newdata = testHex[,-c(21)]))$predict
  accuracy = MLmetrics::Accuracy(prediction, test$label)
  acc = append(acc, accuracy)
  print(i)
}
acc



