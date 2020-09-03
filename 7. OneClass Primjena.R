library(e1071)
library(caret)
library(NLP)
library(tm)
library(pROC)
library(LOF)
dataForSvm = noviFinalniSetZaFiltiranje
pROCdataForSvm = select(noviFinalniSetZaFiltiranje, time, cik, LM)

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
               nu=0.87,
               gamma=0.0001,
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
something = noviFinalniSetZaFiltiranje[noviFinalniSetZaFiltiranje$X %in% out_index,]
proba = cbind(something$ip, something$LM)

dataForSvm$ip = as.factor(dataForSvm$ip)
X <- dataForSvm[,3:4]

# Find outliers by setting an optional k
outlier_score <- lofactor(X, k=10)

# Sort and find index for most outlying observations
names(outlier_score) <- 1:nrow(X)
sort(outlier_score, decreasing = TRUE)

# Inspect the distribution of outlier scores
hist(outlier_score)

outliers <- order(outlier_score, decreasing=T)[1:5]
print(outliers)

n <- nrow(dataForSvm)

labels <- 1:n

labels[-outliers] <- "."

biplot(prcomp(dataForSvm[,3:4]), cex=.8, xlabs=labels)

sets = noviFinalniSetZaFiltiranje
sets$label= sets$LM
write.csv(sets, '/Users/emina/Desktop/setPodataka.csv')
