library(e1071)
library(caret)
library(NLP)
library(tm)
dataForSvm = select(noviFinalniSetZaFiltiranje, time, cik, LM)

dataForSvm$SpeciesClass[dataForSvm$LM=="human"] <- "TRUE"
dataForSvm$SpeciesClass[dataForSvm$LM!="human"] <- "FALSE"
trainPositive<-subset(dataForSvm,SpeciesClass=="TRUE")
testnegative<-subset(dataForSvm,SpeciesClass=="FALSE")
inTrain<-createDataPartition(1:nrow(trainPositive),p=0.7,list=FALSE)

trainpredictors<-trainPositive[inTrain, 1:2]
trainLabels<-trainPositive[inTrain,4]


testPositive<-trainPositive[-inTrain,]
testPosNeg<-rbind(testPositive,testnegative)

testpredictors<-testPosNeg[,1:2]
testLabels<-testPosNeg[,4]
trainpredictors$cik<-normalize(trainpredictors$cik)
trainpredictors$time<-normalize(trainpredictors$time)

testpredictors$cik<-normalize(testpredictors$cik)
testpredictors$time<-normalize(testpredictors$time)

svm.model<-svm(trainpredictors,
               y=NULL,
               type='one-classification',
               nu=0.11,
               gamma=90,
               kernel="radial")

svm.predtrain<-predict(svm.model,trainpredictors)
svm.predtest<-predict(svm.model,testpredictors)

# confusionMatrixTable<-table(Predicted=svm.pred,Reference=testLabels)
# confusionMatrix(confusionMatrixTable,positive='TRUE')

confTrain<-table(Predicted=svm.predtrain,Reference=trainLabels)
confTest<-table(Predicted=svm.predtest,Reference=testLabels)

confusionMatrix(confTest, positive='TRUE')

print(confTrain)
print(confTest)

plot(svm.model, testpredictors)
#ROC and Confusion Matrix
svm.roc=roc(testLabels, as.numeric(svm.predtest))

#AUC PLOT
plot(svm.roc,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc(svm.roc)

labelsForTestData = ifelse(testLabels == "TRUE", TRUE, FALSE)
out_index=which(svm.predtest==TRUE)
plot(labelsForTestData, col="blue", type="l")
df = as.data.frame(out_index)
points(x=out_index, y=labelsForTestData[out_index], pch='|',  col="red", cex=1)
something = noviFinalniSetZaFiltiranje[noviFinalniSetZaFiltiranje$X == 492 | noviFinalniSetZaFiltiranje$X == 517 | noviFinalniSetZaFiltiranje$X == 517| noviFinalniSetZaFiltiranje$X == 524| noviFinalniSetZaFiltiranje$X == 580| noviFinalniSetZaFiltiranje$X == 587| noviFinalniSetZaFiltiranje$X == 588| noviFinalniSetZaFiltiranje$X == 465| noviFinalniSetZaFiltiranje$X == 526| noviFinalniSetZaFiltiranje$X == 527 ,]
proba = cbind(something$ip, something$LM)
