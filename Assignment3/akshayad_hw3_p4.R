###########################################
# Problem 4 Homework 3
###########################################

rm(list = ls())
# Generating simulating data set.
set.seed(1);
x=rnorm(100);
y = x - 2*x^2 + rnorm(100);

#importing boot package 
library(boot);

# creating data frame for computing LOOCV error.
data = data.frame(x, y);

#looping to fit the four models 
for(i in 1:4){
model = glm(y~poly(x,i))
print(paste(i, ":", cv.glm(data, model)$delta, sep = ""))
}

# checking the summary of model
summary(model)