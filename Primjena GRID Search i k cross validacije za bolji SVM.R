

#svm----
split = sample.split(dataForSvm$LM, SplitRatio = 0.30) 
training_set = subset(dataForSvm, split == TRUE) 
test_set = subset(dataForSvm, split == FALSE) 
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

# GRID Search
classifier = train(form = LM ~ ., data = training_set, method = 'svmRadial')
classifier
classifier$bestTune
y_pred = predict(classifier, newdata = test_set[-3])
cm = table(test_set[, 3], y_pred)
confusionMatrix(test_set$LM, y_pred)

folds = createFolds(training_set$LM, k = 10)

# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { # start of function
  # in the next two lines we will separate the Training set into it's 10 pieces
  training_fold = training_set[-x, ] # training fold =  training set minus (-) it's sub test fold
  test_fold = training_set[x, ] # here we describe the test fold individually
  # now apply (train) the classifer on the training_fold
  classifier = svm(formula = LM ~ .,
                   data = training_fold,
                   scale = FALSE,
                   gamma=2,
                   cost = 1000,
                   kernel = 'radial')
  # next step in the loop, we calculate the predictions and cm and we equate the accuracy
  # note we are training on training_fold and testing its accuracy on the test_fold
  y_pred = predict(classifier, newdata = test_fold[-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(accuracy)
})
accuracy = mean(as.numeric(cv))
accuracy

# TRAINING SET SVM
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
points(grid_set, pch = '.', col = ifelse(y_grid == 0, 'coral1', 'aquamarine')) 
points(set, pch = 21, bg = ifelse(set[, 3] == 0, 'red3','green4')) 



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
points(grid_set, pch = '.', col = ifelse(y_grid == 0, 'coral1', 'aquamarine')) 
points(set, pch = 21, bg = ifelse(set[, 3] == 0, 'red3', 'green4' )) 