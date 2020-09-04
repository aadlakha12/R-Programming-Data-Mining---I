############################################################
# Homewrok 4 Problem 1
############################################################


rm(list = ls(all = T))

# Importing laaso package
library(lasso2)
# Loading prostate data
prostate = data(Prostate)

# Checking the NA values in data.
sum(is.na(Prostate))

set.seed(1234)
# Generating training and testing datset. 
trainPoint = sample(1:nrow(Prostate), 0.8*nrow(Prostate))
train = Prostate[trainPoint,]
test = Prostate[-trainPoint,]

# loading leaps package
library(leaps)
# model fit
model.fit <- regsubsets(lcavol~., data = train, nbest = 1, nvmax = 8, method = "exhaustive")
outputS <- summary(model.fit)
outputS

#2 Check which feature regression is best. 
which(outputS$cp == min(outputS$cp)) 

# The best 2 feature subset are lcp and lpsa 
which(outputS$outmat[2,] == "*")

# loading stats Library.
library(stats)

AICValue = c()
BICValue = c()
for (i in 1:8){
  index = which(outputS$outmat[i,] == "*")
  index = index + 1 
  trainSet = train[, c(1,index)]
  model.glm = glm(lcavol~., data = trainSet, family = "gaussian") 
  AICValue = append(AICValue, AIC(model.glm))
  BICValue = append(BICValue, BIC(model.glm))
}

features = seq(1,8)
# creating data frame for plotting dataset.
dataPlot = data.frame(features, AICValue, BICValue)

# loading ggplot2.
library(ggplot2)

#plotting the dataset using ggplot.
ggplot(dataPlot, aes(features)) + 
  geom_line(aes(y = AICValue, colour = "AICValue")) + 
  geom_line(aes(y = BICValue, colour = "BICValue")) +
  ggtitle("AIC vs BIC") + xlab("Number of features") + ylab("Values") +
  theme(plot.title = element_text(hjust = 0.5))



# Creating function for cross validation.
CV = function(dataframe, fold = 5){
dataframe<-dataframe[sample(nrow(dataframe)),]

#Creating 10 folds of equal size

foldsValue <- cut(seq(1,nrow(dataframe)),breaks=fold,labels=FALSE)

#Perform 10 fold cross validation
CV.result = c()
for(i in 2:fold){
  testIndexes = which(foldsValue==i,arr.ind=TRUE)
  dataTest = dataframe[testIndexes, ]
  dataTrain = dataframe[-testIndexes, ]
  #training and testing with glm model. 
  model.glm = glm(lcavol~., dataTrain, family = "gaussian")
  predictions = predict(model.glm, dataTest)
  CV.result = append(CV.result, (1/length(predictions))*sum((predictions - dataTest$lcavol)^2))
}
return(mean(CV.result))
}

# 5 CV
trainError5CV = c()
testError5CV = c()
for (i in 1:8){
  index = which(outputS$outmat[i,] == "*")
  index = index + 1
  resultTraining = train[, c(1,index)]
  resultTesting = test[,c(1,index)] 
  resultFit = glm(lcavol~., data = resultTraining, family = "gaussian")
  pred.test = predict(resultFit, resultTesting)
  errorTest = (1/length(resultTesting$lcavol))*sum((pred.test - resultTesting$lcavol)^2)
  errorTrain = CV(resultTraining, 5) 
  trainError5CV = append(trainError5CV, errorTrain)
  testError5CV = append(testError5CV, errorTest)
}

# 10CV
trainError10CV = c()
testError10CV = c()
for (i in 1:8){
  index = which(outputS$outmat[i,] == "*")
  index = index + 1
  resultTraining = train[, c(1,index)]
  resultTesting = test[,c(1,index)]
  resultFit = glm(lcavol~., data = resultTraining, family = "gaussian")
  pred.test = predict(resultFit, resultTesting)
  errorTest = (1/length(resultTesting$lcavol))*sum((pred.test - resultTesting$lcavol)^2)
  errorTrain = CV(resultTraining, 10) 
  trainError10CV = append(trainError10CV, errorTrain)
  testError10CV = append(testError10CV, errorTest)
}

# Creating functions that feed into "bootpred" method
betaFit <- function(X,Y){
  lsfit(X,Y)	
}

betaPredict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

errorFn <- function(Y,Yhat){
  (Y-Yhat)^2
}

# Create data X and Y
X = train[,2:9]
Y = train[,1]

# loading bootstrap package
library(bootstrap)

bootError = c()
for (i in 1:8){
  index = which(outputS$outmat[i,] == "*")  
  res = bootpred(X[,index], Y, nboot = 50, theta.fit = betaFit, theta.predict = betaPredict, err.meas = errorFn) 
  bootError = c(bootError, res[[3]])
}

features = seq(1,8)
# creating data frame of all errors
dataPlot = data.frame(features, trainError5CV, testError5CV, trainError10CV, testError10CV, 
                            bootError)

#plotting the data frame error using ggplot2 
ggplot(dataPlot, aes(features)) + 
  geom_line(aes(y = trainError5CV, colour = "5CV Train Error")) + geom_point(aes(y = trainError5CV),
                                                                               size = 1.5) + 
  geom_line(aes(y = testError5CV, colour = "5CV Test Error")) + geom_point(aes(y = testError5CV),
                                                                             size = 1.5) + 
  
  geom_line(aes(y = trainError10CV, colour = "10CV Train Error")) + geom_point(aes(y = trainError10CV),
                                                                                 size = 1.5) + 
  geom_line(aes(y = testError10CV, colour = "10CV Test Error")) + geom_point(aes(y = testError10CV),
                                                                               size = 1.5) + 
  
  geom_line(aes(y = bootError, colour = "Boot Error")) + geom_point(aes(y = bootError),size = 1.5) + 
  
  ggtitle("Error plot") + xlab("No of features") + ylab("MSE") +
  theme(plot.title = element_text(hjust = 0.5))

