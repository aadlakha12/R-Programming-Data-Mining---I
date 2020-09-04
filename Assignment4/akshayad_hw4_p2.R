###############################################################
# Homework 4 Problem 2
###############################################################

rm(list = ls(all = T))

# Reading data from file
data = read.table("wine.data.txt",sep = ",")

names(data) = c("Wine", "Alcohol", "Malic_acid", "Ash", "Alcalinity_ash", "Magnesium", "Total_phenols", "Flavanoids", 
                "Nonflavanoid_phenols", "Proanthocyanins", "Color_intensity","Hue","OD280/OD315","Proline")
str(data)
# Checking NA values in data and summary of dataset.
sum(is.na(data))
summary(data)


data$Wine = as.factor(as.character(data$Wine))

set.seed(1)

# creating training and testing dataset.
trainIndex = sample(1:nrow(data), 0.8*nrow(data))
train = data[trainIndex,]
test = data[-trainIndex,]

# loading libaries rpart,partykit,metrics and rattle.
library(rpart)				    
library(partykit)
library(Metrics)
library(rattle)

# generating model using rpart
modelTree = rpart(Wine~., train, method = "class")
fancyRpartPlot(modelTree)	

# predicting with training test.
trainPred = predict(modelTree, train, type = "class")

# table of trainingPred and wine train data.
table = table(trainPred, train$Wine)
trainAccuracy = (table[1,1]+table[2,2]+table[3,3]) / (sum(table[1,])+sum(table[2,])+sum(table[3,])) * 100
namesTrain = rownames(modelTree$frame)[modelTree$where]
trainNode4 = sum(namesTrain == 4)
trainNode5 = sum(namesTrain == 5)
trainNode6 = sum(namesTrain == 6)
trainNode7 = sum(namesTrain == 7)

# test data predictions
testPred = predict(modelTree, test, type = "class")

# table testPred and wine test data.
table = table(testPred, test$Wine)
testAccuracy = (table[1,1]+table[2,2]+table[3,3]) / (sum(table[1,])+sum(table[2,])+sum(table[3,])) * 100

# Checking accuracy of training and testing.
trainAccuracy
testAccuracy

testNode5 = 0
testNode4 = sum(testPred == 1)
testNode6 = sum(testPred == 2)

index = which(testPred == 3)
for(i in index){
  if(test[i,]$Proline >= 755){
    testNode5 = testNode5 + 1
  }
}
testNode7 = length(index) - testNode5

