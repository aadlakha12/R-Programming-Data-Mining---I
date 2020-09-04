###########################################
#Homework 2 Problem 1
###########################################

rm(list = ls())
# importing library ISLR for the college dataset
library(ISLR);
set.seed(10);
collegeData <- College;

sum(is.na(collegeData));
dim(collegeData);
head(collegeData);
colnames(collegeData);

# spliting data in 75% training set and 25% testing set.

sampleData <- floor(0.75 * nrow(collegeData))
set.seed(123)
index <- sample(seq_len(nrow(collegeData)), size = sampleData)
collegeData.train <- collegeData[index, ]
collegeData.test <- collegeData[-index, ]

# creating a linear model using least squares and predicted against testing dataset.

model <- lm(Apps ~ ., data = collegeData.train)
summary(model)
pred <- predict(model,collegeData.test)
mean((pred - collegeData.test$Apps)^2)

# importing glmnet library for ridge regression and lasso regression.
library(glmnet);

# Ridge regression
train.matrix <- model.matrix(Apps ~ ., data = collegeData.train)
test.matrix <- model.matrix(Apps ~ ., data = collegeData.test)
grid = 10^seq(4, -2, length = 100)

model.ridge <- glmnet(train.matrix, collegeData.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.matrix, collegeData.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
lambda.ridge <-  cv.ridge$lambda.min
lambda.ridge

# Fiiting a ridge model on testing set. 
predict.ridge <- predict(model.ridge, s = lambda.ridge, newx = test.matrix)
mean((predict.ridge - collegeData.test$Apps)^2)

# Lasso regression

model.lasso <- glmnet(train.matrix, collegeData.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.matrix, collegeData.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
lambda.lasso <- cv.lasso$lambda.min
lambda.lasso

# Fitting a testing model on testing set.
predict.lasso <- predict(model.lasso, s = lambda.lasso, newx = test.matrix)
mean((predict.lasso - collegeData.test$Apps)^2)


# importing pls library for pls model.
library(pls)

# creating a PCR model.
model.pcr <- pcr(Apps ~ ., data = collegeData.train, scale = TRUE, validation = "CV")
validationplot(model.pcr, val.type = "MSEP")

#fitting a PCR model on testing set.
predict.pcr <- predict(model.pcr, collegeData.test, ncomp = 10)
mean((predict.pcr - collegeData.test$Apps)^2)

# creating a PLS model.
model.pls <- plsr(Apps ~ ., data = collegeData.train, scale = TRUE, validation = "CV")
validationplot(model.pls, val.type = "MSEP")

# fitting a PLS model on testing set
predict.pls <- predict(model.pls, collegeData.test, ncomp = 10)
mean((predict.pls - collegeData.test$Apps)^2)

# calculating average value of testing set.
average <-  mean(collegeData.test$Apps)

# For comparing results, calculating R-squared value for all models.

lm.RSquare <-  1 - mean((collegeData.test$Apps - pred)^2) /
  mean((collegeData.test$Apps - average)^2)
ridge.RSquare <-  1 - mean((collegeData.test$Apps - predict.ridge)^2)/
  mean((collegeData.test$Apps - average)^2)
lasso.RSquare <-  1 - mean((collegeData.test$Apps - predict.lasso)^2) /
  mean((collegeData.test$Apps - average)^2)
pcr.RSquare <-  1 - mean((collegeData.test$Apps - predict.pcr)^2) /
  mean((collegeData.test$Apps - average)^2)
pls.RSquare <-  1 - mean((collegeData.test$Apps - predict.pls)^2) /
  mean((collegeData.test$Apps - average)^2)

# clubbing R-Squared data in a table.

erroData <- matrix(c(lm.RSquare,ridge.RSquare,lasso.RSquare,pcr.RSquare,pls.RSquare),ncol=5,byrow=TRUE)
colnames(erroData) <- c("Linear","Ridge","Lasso","PCR","PLS")
rownames(erroData) <- c("R Squared")
erroData <- as.table(erroData)
erroData

# plotting a bar chart to get the visualization.
barplot(c(lm.RSquare, ridge.RSquare, lasso.RSquare, pcr.RSquare, pls.RSquare), names.arg = c("Least Squared", "Ridge Regression", "Lasso Regression","PCR", "PLS"),
        main = "Testing R-squared Value")