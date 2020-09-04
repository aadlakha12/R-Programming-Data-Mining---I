#####################################################
# Homework 2 Problem 3
#####################################################

rm(list = ls())
set.seed(1)
# assigning features and observations
p = 20
n = 1000
x = matrix(rnorm(n*p),n,p)

# assigning zero values to some random elements of beta 
beta <- rnorm(p)
beta[2] <- 0
beta[4] <- 0
beta[7] <- 0
beta[15] <- 0
beta[19] <- 0

# epsilon value
epsilon <- rnorm(p)

# expression
y <- x%*%beta + epsilon;

# Data spliting in training data and testing data
 
trainData <- sample(seq(1000), 100, replace = FALSE)
testData <- -trainData
x.trainData <- x[trainData, ]
x.testData <- x[testData, ]
y.trainData <- y[trainData]
y.testData <- y[testData]

# Best selection on trainig dataset

library(leaps)
data.train <- data.frame(y = y.trainData, x= x.trainData)
regfit.full <- regsubsets(y ~ ., data = data.train, nvmax = 20)
train.mat <- model.matrix(y ~ ., data = data.train, nvmax = 20)
val.errors <- rep(NA, 20)
for (i in 1:20) {
    coefi <- coef(regfit.full, id = i)
    pred <- train.mat[, names(coefi)] %*% coefi
    val.errors[i] <- mean((pred - y.trainData)^2)
}
plot(val.errors, xlab = "Number of predictors", ylab = "Training MSE", pch =19, type = "b")

# Best selection on testing data

data.test <- data.frame(y = y.testData, x = x.testData)
test.mat <- model.matrix(y ~ ., data = data.test, nvmax = 20)
val.errors <- rep(NA, 20)
for (i in 1:20) {
    coefi <- coef(regfit.full, id = i)
    pred <- test.mat[, names(coefi)] %*% coefi
    val.errors[i] <- mean((pred - y.testData)^2)
}
plot(val.errors, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")


which.min(val.errors)
coef(regfit.full,which.min(val.errors))

