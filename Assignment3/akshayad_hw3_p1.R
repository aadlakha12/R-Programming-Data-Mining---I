###########################################################
# Problem 1 Homework 3
###########################################################
rm(list = ls())
# importing MASS Library and loading data
library(MASS)
data("Boston")

# exploring dataset
sum(is.na(Boston))
names(Boston)
summary(Boston)

# converting crime feature to categorial value.
crimeData <- rep(0, length(Boston$crim))
crimeData[Boston$crim > median(Boston$crim)] <- 1
BostonData <- data.frame(Boston, crimeData)
head(BostonData)


# splitting data into training and testing set.
set.seed(1234)
train <- sample(1:dim(BostonData)[1], dim(BostonData)[1]*.7, rep=FALSE)
test <- -train
BostonData.train <- BostonData[train, ]
BostonData.test <- BostonData[test, ]
crimeData.test <- crimeData[test]

model <- glm(crimeData ~ . - crimeData - crim, data = BostonData, family = binomial)
model

# importing corrplot library for correlation matrix
library(corrplot)
#plotting correlation matrix
corrplot::corrplot.mixed(cor(BostonData[, -1]), upper="circle")

#fitting linear regression model
model1 <- glm(crimeData ~ nox + indus + age + rad, data = BostonData, family = binomial)

probs <- predict(model1, Boston.test, type = "response")
predModel1 <- rep(0, length(probs))
predModel1[probs > 0.5] <- 1
table(predModel1, crimeData.test)

# linear regression model error
mean(predModel1 != crimeData.test)

# LDA Model
ldaModel <- lda(crimeData ~ nox + indus + age + rad , data = BostonData)
predldaModel <- predict(ldaModel, BostonData.test)
table(predldaModel$class, crimeData.test)

#LDA model error
mean(predldaModel$class != crimeData.test)

# creating dataset for KNN 
data = scale(BostonData[,-c(1,15)])
set.seed(1234)
train <- sample(1:dim(BostonData)[1], dim(BostonData)[1]*.7, rep=FALSE)
test <- -train
training_data = data[train, c("nox" , "indus" , "age" , "rad")]
testing_data = data[test, c("nox" , "indus" , "age" , "rad")]
train.crime01 = BostonData$crimeData[train]
test.crime01= BostonData$crimeData[test]


library(class)

# KNN
KNN_pred = NULL
error_KNN = NULL
for(i in 1:dim(testing_data)[1]){
set.seed(1234)
KNN_pred = knn(training_data,testing_data,train.crime01,k=i)
error_KNN[i] = mean(test.crime01 != KNN_pred)
}

# checking the error rate for each k 
error_KNN
plot(1:dim(testing_data)[1], error_rate,type='b', xlab='K' , ylab= 'error_KNN')
title('KNN Error Rate Demonstration')

# finding the minimum error rate
min_error_KNN = min(error_KNN)
min_error_KNN

# finding the k at which the error rate is the minimum
min_K = which(error_KNN == min_error_KNN)
min_K










