install.packages('dplyr')
install.packages('stringr')
install.packages('caTools')
install.packages('anytime')
install.packages('e1071') 
install.packages('pROC') 
library(e1071) 
library(dplyr)  
library(stringr)
library(caTools)
library(anytime)
library(caret)
library(pROC)

# Normalizacija podataka
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dataForSvm$cik<-normalize(dataForSvm$cik)
dataForSvm$time<-normalize(dataForSvm$time)
dataForSvm$LM<- ifelse(dataForSvm$LM == 1, 0, 1)
dataForSvm$LM<-as.factor(dataForSvm$LM)

# Razdvojiti podatke na testne i trenirajuće primjerke
split = sample.split(dataForSvm$LM, SplitRatio = 0.30) 
training_set = subset(dataForSvm, split == TRUE) 
test_set = subset(dataForSvm, split == FALSE) 

# Normalizacija testnih i trenirajućih primjeraka
training_set$cik<-normalize(training_set$cik)
training_set$time<-normalize(training_set$time)
test_set$cik<-normalize(test_set$cik)
test_set$time<-normalize(test_set$time)

#An important variant is when we consider one type of error more costly than the other (say, in a medical test for cancer, we prefer a false positive than a false negative, since in the later a sick patient will go untreated). To solve it, we assign different costs to each class:
#  12‖w‖2+C+∑i+ξi+C−∑i−ξi
#where i− are the indexes where yi=−1, and i+ for yi=+1.

#This is done at R’s svm() with option class.weights

# using the previous dataset...
costs <- table(dataForSvm$LM)  # the weight vector must be named with the classes names
costs[1] <- 1e10 # a class -1 mismatch has a terrible cost
costs[2] <- 1    # a class +1 mismatch not so much...
costs
# Korištenje svm funkcije iz e1071 paketa kako bi se kreirao model učenja
svm.model <- svm(LM ~ ., data=dataForSvm, type='C-classification', kernel='linear', class.weights=costs, scale=FALSE)

# Kreiranje predikcije
svm_pred = predict(classifier, newdata = test_set[-3]) 

# Konfuzijska matrica
confusionMatrix(test_set$LM, svm_pred)
confusion_table <- table(test_set$LM, svm_pred)

# AUC ROC
rocCurve_svm = roc(response = test_set$LM,
                   predictor = as.numeric(svm_pred))
auc_curve = auc(rocCurve_svm)
plot(rocCurve_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc_curve

plot(classifier, training_set)
# TRAINING SET SVM
set.seed(500)
set = training_set 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 
grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('cik', 'time') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], 
     main = 'SVM (Training set)', 
     xlab = 'cik', ylab = 'time', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'red3','green4')) 

# TEST DATA SVM
set = test_set 
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01) 
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01) 

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('cik', 'time') 
y_grid = predict(classifier, newdata = grid_set) 

plot(set[, -3], main = 'SVM (Test set)', 
     xlab = 'cik', ylab = 'time', 
     xlim = range(X1), ylim = range(X2)) 

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE) 
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine')) 
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'red3', 'green4' )) 

