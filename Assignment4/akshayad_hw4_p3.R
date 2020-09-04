###############################################################
# Homework 4 Problem 3
###############################################################

rm(list = ls())

# importing package MASS and Metrics
library(MASS)
library(Metrics)

# loading Boston dataset.
boston = data("Boston")
boston = data.frame(boston)

# Checking NA in dataset.
sum(is.na(Boston))

set.seed(1234)
# splitting data into training and testing set.
Index = sample(1:nrow(Boston), 0.8*nrow(Boston))
train = Boston[Index,]
test = Boston[-Index,]
rm(boston, Index)

model = c()
RMSE = c()
# fitting regression model
linearModel = glm(crim~., train, family = "gaussian")
linearPredictions = predict(linearModel, test)
model = append(model, "Regression")
RMSE = append(RMSE, Metrics::rmse(test$crim, linearPredictions))


# kNN Classification
# importing FNN library
library(FNN)

# kNN model
knn_predictions = knn.reg(train = train[,-c(1)], test = test[,-c(1)], y = train$crim, k = 3)
model = append(model, "KNN")
RMSE = append(RMSE, Metrics::rmse(test$crim, knn_predictions$pred))

# Random Forest
library(randomForest)

# Performing Random Forest on data
modelRandom = randomForest(crim~., train, ntree = 100, mtry = 9, sampsize = 300)
predRandom = predict(modelRandom, test)
model = append(model, "RF")
RMSE = append(RMSE,Metrics::rmse(test$crim, predRandom))


# Boosting
library(xgboost)

#Performing Boosting on data.
xgboostTrain = as.matrix(train[,-c(1)])
xgboostTest = as.matrix(test[,-c(1)])
xgboostModel = xgboost(xgboostTrain, train$crim, nrounds = 50, eta = 0.1, max_depth = 4, objective = "reg:linear")
xgboostPred = predict(xgboostModel, xgboostTest)
model = append(model, "XGBoost")
RMSE = append(RMSE, Metrics::rmse(test$crim, xgboostPred))


# Bagging 
library(rpart)

# Number of data points.
dataPoints = 300    

# Number of bags. 
bags = 200   
rmseTree = c()

# Performing Bagging on data.
for(i in seq(1,bags)){
  train1 = train[sample(1:nrow(train), dataPoints),]
  modelTree = rpart(crim~., train1)
  predTree = predict(modelTree, test)
  rmseTree = append(rmseTree, Metrics::rmse(test$crim, predTree))
}
mean_rmseTree = mean(rmseTree)
model = append(model, "Bagging_Tree")
RMSE = append(RMSE, mean_rmseTree)



# Creating data frame for plotting model and RMSE.
dataPlot = data.frame(model, RMSE)

# for plotting model and RMSE.
library(plotly)
p = plot_ly(dataPlot, x = ~model, y = ~RMSE, name = 'Model', type = 'scatter', mode = 'markers') %>%
  layout(title = "Models Vs RMSE",
         xaxis = list(title = "Models", type = "category"),
         yaxis = list (title = "RMSE"))
p

# for pair plotting 
library(GGally)
ggpairs(Boston)

# for decision tree
library(rattle)
library(partykit)
fancyRpartPlot(modelTree)
