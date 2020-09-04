############################################### 
# PROBLEM 2 HOMEWORK 3
##############################################

rm(list = ls())
library(MASS)
# reading data from a file
data = read.table("problem2data.txt")
dim(data)

# considering the columns 5 to 10 in data and ignoring first 4 columns.
data = data[5:10]

names(data) = c("glucose.area", "insulin.area", "SSPG", "relative.weight", "fasting.plasma.glucose", "target")
data$target =as.factor(as.character(data$target))

# checking if data has NA values
sum(is.na(data))

# importing library ggplot2 and GGally
library(ggplot2)
library(GGally)

#a) plotting scatterplots 

ggpairs(data, columns = 1:5, ggplot2::aes(colour=target))

set.seed(1234)

# creating train and test data
train = sample(1:nrow(data), 0.7*nrow(data))

trainData = data[train,]

testData = data[-train,]

# least squares
get_LeastSquares = function(values, Pvalues){
  sum = 0
  for(i in 1:length(values)){
    sum = sum + ((values[i] -Pvalues[i])^2)
  }
  return(sum)
}

#b)
# LDA Model
ldaModel = lda(target~., trainData)
ldaPred = predict(ldaModel, testData[,1:5])$class
ldaError = get_LeastSquares(as.numeric(as.character(testData$target)), as.numeric(as.character(ldaPred)))


# QDA Model
qdaModel = qda(target~., trainData)
qdaPred = predict(qdaModel, testData[,1:5])$class
qdaError = get_LeastSquares(as.numeric(as.character(testData$target)), as.numeric(as.character(qdaPred)))

# checking error to know the performance of LDA and QDA model
ldaError

qdaError

# c)
pData = data.frame(0.98, 122, 544, 186, 184)

names(pData) = names(trainData)[1:5]
#Prediction with LDA Model
predict(ldaModel, pData)$class

#Prediction with QDA Model.
predict(qdaModel, pData)$class

































 












 
































































