
library(e1071)
library(caret)
library(NLP)
library(tm)
library(pROC)
library(dplyr)
#dataForSvm = dataWithFilters
dataForSvm = filtriraniPodaci
dataForSvm$LM = ifelse(dataForSvm$LM == 1, "robot", "human")
dataForSvm$LM = as.factor(dataForSvm$LM)
dataForSvm = select(dataForSvm, cik, time, LM)
mi=data.frame(dataForSvm)
typeof(dataForSvm$LM)
#mi=subset(mi,select=-LM)
lapply(mi,class)

#Data splitting
positive=subset(mi,mi$LM=="human")
negative=subset(mi,mi$LM=="robot")
intrain=createDataPartition(1:nrow(positive),p=0.30,list=FALSE)
train=positive[intrain,1:2]
trainlabels=positive[intrain,3]
trainpositive =positive[intrain,]

testpositive=positive[-intrain,]
testwithclass=rbind(negative,testpositive)
test=testwithclass[,1:2]
testlables=testwithclass[,3]

#Building SVM Model
svm.model<-svm(train,
               y=NULL,
               type='one-classification',
               nu=0.01152346,
               epsilon=25.11129,
               kernel = "radial",
               scale=TRUE)
#svm.predtrain<-predict(svm.model,train)
svm.predtest<-predict(svm.model,testpositive[,1:2])

summary(svm.predtest)
svm.predtest
summary(testlables)
table(svm.predtest, testpositive[,3])

#ROC and Confusion Matrix
svm.predtest=as.numeric(svm.predtest)#since svm.predtest is in logical form
svm.predtrain=as.numeric(svm.predtrain)#since svm.predtest is in logical form
svm.roc=roc(testpositive[,3], svm.predtest)

#AUC PLOT
plot(svm.roc,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc(svm.roc)

testlables=as.character(testlables)
trainlabels=as.character(trainlabels)
svm.predtest=as.character(svm.predtest)
svm.predtrain=as.character(svm.predtrain)

#ACCURACY
confusionMatrix(factor(testlables, levels=c("0", "1")), factor(svm.predtest, levels=c("0", "1")))

# k = 29 gamma=25,cost = 10,kernel = 'radial' 0.8607547   0.6 test set
# # in creating the folds we specify the target feature (dependent variable) and # of folds
# folds = createFolds(positive$LM, k = 3)
# k = 5 gamma=2,cost = 100000,kernel = 'radial' 0.8417729   0.6 test set
# k = 5 gamma=1,cost = 100000,kernel = 'radial' 0.8077355  0.6 test set
# k = 5 gamma=1,cost = 1000000,kernel = 'radial' 0.8185646  0.6 test set
# k = 5 gamma=1,cost = 1000000,kernel = 'radial' 0.8324573  0.6 test set

# # in cv we are going to applying a created function to our 'folds'
# cv = lapply(folds, function(x) {
#   print(folds)
#   print(x)
#   # start of function
#   # in the next two lines we will separate the Training set into it's 10 pieces
#   training_fold = trainpositive[-x, ] # training fold =  training set minus (-) it's sub test fold
#   test_fold = testpositive[x, ] # here we describe the test fold individually
#   # now apply (train) the classifer on the training_fold
#   classifier = svm(train,
#                    y=NULL,
#                    type='one-classification',
#                    nu=0.01152346,
#                    epsilon=25.11129,
#                    kernel = "radial",
#                    scale=TRUE)
#   # next step in the loop, we calculate the predictions and cm and we equate the accuracy
#   # note we are training on training_fold and testing its accuracy on the test_fold
#   y_pred = predict(classifier, newdata = test_fold)
#   cm = table(test_fold[, 3], y_pred)
#   accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
#   return(accuracy)
# })
# accuracy = mean(as.numeric(cv))