library(e1071)   
library(kernlab)   
library(dplyr)
library(stringr)
library(caret)

dataWithFiltersNONULL<-onostocemokoristiti
dataWithFiltersNONULL = select(dataWithFiltersNONULL, time, cik, ip)
dataWithFiltersNONULL$ip = as.factor(dataWithFiltersNONULL$ip)

n <- nrow(dataWithFiltersNONULL)  # Number of observations
ntrain <- round(n*0.20)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_iris <- dataWithFiltersNONULL[tindex,]   # Create training set
test_iris <- dataWithFiltersNONULL[-tindex,]   # Create test set

svm1 <- svm(formula = as.factor(ip) ~ ., 
            data = train_iris, 
            gamma= 2,
            cost = 1000,
            kernel = "radial",
            scale=TRUE,
            na.action=na.omit)
svm_pred = predict(svm1, newdata = test_iris[-3]) 
confusionMatrix(test_iris$ip, svm_pred)

plot(svm1, train_iris)
points(train_iris, pch = "+", col = 3)

summary(svm1)
