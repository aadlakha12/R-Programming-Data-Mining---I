############################################################
# Homework 5 Problem 3
############################################################

# loading library
library(ISLR)

# importing data OJ
data("OJ")
OJ = data.frame(OJ)

# sanity check over dataset
sum(is.na(OJ))
summary(OJ)
str(OJ)

# Changing the type of the features. 
OJ$STORE = as.factor(as.character(OJ$STORE))
OJ$StoreID = as.factor(as.character(OJ$StoreID))
OJ$SpecialMM = as.factor(as.character(OJ$SpecialMM))
OJ$SpecialCH = as.factor(as.character(OJ$SpecialCH))

set.seed(1234)

#splitting data into training and testing 
index = sample(1:nrow(OJ), 0.7*nrow(OJ))
train = OJ[index,]
test = OJ[-index,]

#importing libraries e1071 and Metrics
library(e1071)
library(Metrics)

#fitting models. 

#SVM
errorTrain = c()
errorTest = c()
costList = seq(0.01, 10, 0.1)
costList = append(costList, 10)
iter = 0
for(i in costList){
  svmModel = svm(Purchase~., train, cost = i)
  predictions = predict(svmModel, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  errorTest = append(errorTest, test_error) 
  predictions = predict(svmModel, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  errorTrain = append(errorTrain, train_error)
  iter = iter + 1
  print(paste(iter, "of", length(costList)))
}

# Creating dataframe of cost, train errors and test errors
dataPlot = data.frame(costList, errorTrain, errorTest)
names(dataPlot) = c("cost", "train_error", "test_error")

library(plotly)

# plotting SVM performance
p = plot_ly(dataPlot, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "SVM: Training Vs Testing Error Performance",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p

# Radial kernel
errorTrain = c()
errorTest = c()
costList = seq(0.01, 10, 0.1)
costList = append(costList, 10)
iter = 0
for(i in costList){
  svmModel = svm(Purchase~., train, cost = i, kernel = "radial")
  predictions = predict(svmModel, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  errorTest = append(errorTest, test_error)
  predictions = predict(svmModel, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  errorTrain = append(errorTrain, train_error)
  iter = iter + 1
  print(paste(iter, "of", length(costList)))
}

# Creating dataframe of cost, train errors and test errors
dataPlot = data.frame(costList, errorTrain, errorTest)
names(dataPlot) = c("cost", "train_error", "test_error")

#plotting Radial Kernel Performance
p = plot_ly(dataPlot, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Radial Kernel: Training Vs Testing Error Performance",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p

# Degree = 2 polynomial kernel 
errorTrain = c()
errorTest = c()
costList = seq(0.01, 10, 0.1)
costList = append(costList, 10)
iter = 0
for(i in costList){
  svmModel = svm(Purchase~., train, cost = i, kernel = "polynomial", degree = 2)
  predictions = predict(svmModel, test)
  table = table(test$Purchase, predictions)
  test_error = nrow(test) - (table[1,1] + table[2,2])
  errorTest = append(errorTest, test_error)
  predictions = predict(svmModel, train)
  table = table(train$Purchase, predictions)
  train_error = nrow(train) - (table[1,1] + table[2,2])
  errorTrain = append(errorTrain, train_error)
  iter = iter + 1
  print(paste(iter, "of", length(costList)))
}

# creating dataframe of cost, train errors and test errors
dataPlot = data.frame(costList, errorTrain, errorTest)
names(dataPlot) = c("cost", "train_error", "test_error")

# plotting Polynomial Kernel Performance
p = plot_ly(dataPlot, x = ~cost, y = ~train_error, name = 'Train_Error', type = 'scatter', 
            mode = 'lines+markers') %>%
  add_trace(y = ~test_error, name = 'Test_Error', mode = 'lines+markers') %>%
  layout(title = "Polynomial Kernel: Train Vs Test Error Performance",
         xaxis = list(title = "Cost"),
         yaxis = list (title = "Error"))
p

