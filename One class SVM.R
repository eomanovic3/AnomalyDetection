

df <- subset(dataForSvm ,  LM==1)  #choose only one of the classes

x <- subset(df, select = -LM) #make x variables
y <- df$LM #make y variable(dependent)
model <- svm(x, y,type='one-classification',kernel = "radial",gamma=0.10,nu=0.0065) #train an one-classification model 


print(model)
summary(model)

# test on the whole set
pred <- predict(model, subset(dataForSvm, select=-LM)) #create predictions

pred
summary(pred)
#One SVM anomaly detection
dataForSvm$LM =as.numeric(dataForSvm$LM)
model_oneclasssvm <- svm(dataForSvm$LM,type='one-classification',kernel = "radial",gamma=0.05,nu=0.05)
model_oneclasssvm

pred_oneclasssvm <- predict(model_oneclasssvm, dataForSvm$LM)
pred_oneclasssvm
summary(pred_oneclasssvm)

confTrain<-table(Predicted=svm.predtrain,Reference=trainLabels)
confTest<-table(Predicted=svm.predtest,Reference=testLabels)


plot(model_oneclasssvm, dataForSvm$LM, type='one-classification',kernel="radial")
#END One SVM anomaly detection

# Classification ovo ispod:
# Truth
# Prediction human robot
# human   106     0
# robot     0    60
index <- c(1:nrow(dataForSvm))
test.index <- sample(index, size = (length(index)/3))
train <- dataForSvm[-test.index ,]
test <- dataForSvm[test.index ,]
train$LM = as.factor(train$LM)

svm.model.linear <- svm(LM ~ ., data = train, kernel = 'linear')
svm.model.linear
prediction = predict(svm.model.linear, test)
table(Prediction = prediction,Truth = test$LM)
# Classification gotovo
