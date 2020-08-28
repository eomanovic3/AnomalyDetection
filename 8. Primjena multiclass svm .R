library(e1071)   
library(kernlab)   
library(dplyr)
library(stringr)
library(caret)

dataWithFiltersNONULL<-filtriraniPodaci
dataWithFiltersNONULL = select(dataWithFiltersNONULL, time, cik, frequencyCounter)
dataWithFiltersNONULL$frequencyCounter = as.factor(dataWithFiltersNONULL$frequencyCounter)

n <- nrow(dataWithFiltersNONULL)  # Number of observations
ntrain <- round(n*0.25)  # 75% for training set
set.seed(314)    # Set seed for reproducible results
tindex <- sample(n, ntrain)   # Create a random index
train_iris <- dataWithFiltersNONULL[tindex,]   # Create training set
test_iris <- dataWithFiltersNONULL[-tindex,]   # Create test set

svm1 <- svm(formula = as.factor(frequencyCounter) ~ ., 
            data = train_iris, 
            gamma= 2,
            cost = 1000,
            kernel = "radial",
            scale=TRUE,
            na.action=na.omit)
svm_pred = predict(svm1, newdata = test_iris[-3])

confusionMatrix(test_iris$frequencyCounter, svm_pred)

plot(svm1, train_iris)
points(train_iris, pch = "+", col = 3)

summary(svm1)
plot(cmdscale(dist(dataWithFiltersNONULL[,-3])),col = as.integer(dataWithFiltersNONULL[,3]),pch = c("o","+")[1:150 %in% svm1$index + 1])

