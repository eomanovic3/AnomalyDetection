

split = sample.split(dataForSvm$LM, SplitRatio = 0.90) 

train = subset(dataForSvm, split == TRUE) 
test = subset(dataForSvm, split == FALSE) 

train$LM
train_normal = train[train$LM == "human",] 
train_outliers = train[train$LM == "robot",] 
outlier_prop = length(t(train_normal)) / length(t(train_outliers))

model <- svm(subset(train_normal, select = -LM), 
             train_normal$LM,
             type='one-classification', 
             nu=outlier_prop,
             gamma=0.01,
             kernel = "radial",
             scale=TRUE) #train an one-classification model 


pred <- predict(model, subset(test, select = -LM)) #create predictions


#ROC and Confusion Matrix
svm.roc=roc(test$LM, as.numeric(pred))

#AUC PLOT
plot(svm.roc,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc(svm.roc)


testlables = ifelse(test$LM== "human", TRUE, FALSE)
#ACCURACY
confusionMatrix(factor(testlables, levels=c(TRUE, FALSE)), factor(pred, levels=c(TRUE,FALSE)))

out_index=which(pred==FALSE)
plot(testlables, col="blue", type="l")
summary(svm.predtest)
points(x=out_index, y=testlables[out_index], pch=18, col="red")


