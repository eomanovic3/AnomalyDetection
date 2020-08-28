library(e1071)
df <- dataForSvm

df <- subset(df ,  LM=='human')  #choose only one of the classes

x <- subset(df, select = -LM) #make x variables
y <- df$LM #make y variable(dependent)
model <- svm(x, 
             y,
             type='one-classification', 
             nu=0.01152346,
             epsilon=25.11129,
             kernel = "radial",
             scale=TRUE) #train an one-classification model 


print(model)
summary(model) #print summary

# test on the whole set
pred <- predict(model, subset(dataForSvm, select=-LM)) #create predictions
pred

summary(pred)
summary(dataForSvm)
pred
summary(model)

#ROC and Confusion Matrix
svm.roc=roc(dataForSvm[,3], as.numeric(pred))

#AUC PLOT
plot(svm.roc,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc(svm.roc)


testlables = ifelse(dataForSvm[,3] == "human", TRUE, FALSE)
#ACCURACY
confusionMatrix(factor(testlables, levels=c(TRUE, FALSE)), factor(pred, levels=c(TRUE,FALSE)))

out_index=which(pred==FALSE)
plot(testlables, col="blue", type="l")
summary(svm.predtest)
points(x=out_index, y=testlables[out_index], pch=18, col="red")





