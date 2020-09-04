################################################################
# Homework 2 Problem 2
################################################################

rm(list = ls())

#loading the training set

train_data = read.table("training.txt")
head(train_data)
sum(is.na(train_data))


#loading the testing set and target set

test_data = read.table("testing.txt")
test_Y = read.table("target.txt")
test_data$V86 = test_Y$V1


#storing the error methods and the values. 

method = c()
error = c()

#Creating model using Linear Regression

lm_model = lm(V86~., train_data)
lm_predictions = predict(lm_model, test_data)

#Decide 1 or 0 from predictions

decider_Class = function(x){
  if(x > 0){
    distance_to0 = x
    distance_to1 = abs(x - 1)
  }else{
    return(0)
  }
  class = ifelse(distance_to0 < distance_to1, 0, 1)
  return(class)
}
lm_predictions = sapply(lm_predictions, decider_Class)

table(lm_predictions)
get_LeastSquares = function(Truevalues, Predictedvalues){
  sum = 0
  for(i in 1:length(Truevalues)){
    sum = sum + ((Truevalues[i] - Predictedvalues[i])^2)
  }
  return(sum)
}
result = get_LeastSquares(Truevalues = test_Y$V1, Predictedvalues = as.numeric(lm_predictions))
table(test_Y)
method = append(method, "regression")
error = append(error, result)

# A Ridge model

# loading library for ridge and lasso model
library(glmnet)

# creating a ridge model
ridge_model = cv.glmnet(x = as.matrix(train_data[,-c(86)]), y = train_data$V86, alpha = 0)
summary(ridge_model)
plot(ridge_model)
best_lambda = ridge_model$lambda.min
ridge_predictions = predict(ridge_model, s = best_lambda, newx = as.matrix(test_data[,c(1:85)]),
                            type =  "response")
ridge_predictions = sapply(ridge_predictions, decider_Class)
table(ridge_predictions)
result = get_LeastSquares(Truevalues = test_Y$V1, as.numeric(ridge_predictions))
method = append(method, "Ridge")
error = append(error, result)


#Lasso Model

#creating lasso model
lasso_model = cv.glmnet(x = as.matrix(train_data[,-c(86)]), y = train_data$V86, alpha = 1)
summary(lasso_model)
plot(lasso_model)
best_lambda = lasso_model$lambda.min
lasso_predictions = predict(lasso_model, s = best_lambda, newx = as.matrix(test_data[,c(1:85)]), 
                            type = "response")
lasso_predictions = sapply(lasso_predictions, decider_Class)
table(lasso_predictions)
result = get_LeastSquares(Truevalues = test_Y$V1, as.numeric(lasso_predictions))
method = append(method, "Lasso")
error = append(error, result)

#Forward Model

#importing library leaps for forward model
library(leaps)
get_feature_index = function(small_list, complete_list){
  index = c()
  for(feature_index_i in small_list){
    found_at = grep(pattern = paste("\\b", feature_index_i, "\\b", sep=""),
                    x = complete_list)
    index = append(index, found_at)
  }
  return(unique(index))
}
forward_model = regsubsets(V86~., nvmax = 85, method = "forward", data = train_data)

# Predicting in forward model. 

library(ggplot2)

new_train = cbind(rep(1, nrow(train_data)), train_data) 
colnames(new_train) = c("Intercept", names(train_data))
new_test = cbind(rep(1, nrow(test_data)), test_data)
colnames(new_test) = c("Intercept", names(test_data))
train_forward_error = c()
test_forward_error = c()
for(i in 1:85){
  print(i)
  coef_i = coef(forward_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  train_preds = sapply(train_preds, function(x) decider_Class(x))
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  test_preds = sapply(test_preds, function(x) decider_Class(x))
  
  train_forward_error = append(train_forward_error,
                               get_LeastSquares(train_data$V86, train_preds))
  test_forward_error = append(test_forward_error, 
                              get_LeastSquares(test_data$V86, test_preds))
}
plot_data = data.frame(train_forward_error, test_forward_error)
plot_data$n = rownames(plot_data)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_forward_error, color = "train_forward_error")) + 
  geom_line(aes(y = test_forward_error, color = "test_forward_error")) + ggtitle("Train and Test error comparison: Forward")

method = append(method, "Forward")
error = append(error, min(test_forward_error))


# Backward Model

backward_model = regsubsets(V86~., nvmax = 85, method = "backward", data = train_data)

# Predicting in backward model. 

new_train = cbind(rep(1, nrow(train_data)), train_data) 
colnames(new_train) = c("Intercept", names(train_data))
new_test = cbind(rep(1, nrow(test_data)), test_data)
colnames(new_test) = c("Intercept", names(test_data))
train_backward_error = c()
test_backward_error = c()
for(i in 1:85){
  print(i)
  coef_i = coef(backward_model, id = i)
  feature_index = get_feature_index(names(coef_i), names(new_train))
  train_preds = as.matrix(new_train[, feature_index]) %*% coef_i
  train_preds = sapply(train_preds, function(x) decider_Class(x))  
  test_preds = as.matrix(new_test[,feature_index]) %*% coef_i
  test_preds = sapply(test_preds, function(x) decider_Class(x))
  train_backward_error = append(train_backward_error,
                                get_LeastSquares(train_data$V86, train_preds))
  test_backward_error = append(test_backward_error, 
                               get_LeastSquares(test_data$V86, test_preds))
}
plot_data = data.frame(train_backward_error, test_backward_error)
plot_data$n = rownames(plot_data)
ggplot(plot_data, aes(n, group = 1)) + geom_line(aes(y = train_backward_error, color = "train_backward_error")) + 
  geom_line(aes(y = test_backward_error, color = "test_backward_error")) + ggtitle("Train Test comparison: Backward")

method = append(method, "Backward")
error = append(error, min(test_backward_error))


# Comparison of methods using barcharts
plot_data = data.frame(method, error)
g = ggplot(plot_data, aes(method, error))
g + geom_bar(stat = "identity") + ggtitle("Method comparison")