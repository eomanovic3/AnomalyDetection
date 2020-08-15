set.seed(200)
library(pROC)

dataForSvm = select(dataForSvmBefore, time, cik, LM)
dataForSvm$LM = ifelse(dataForSvm$LM == 0, "0", "1")
dataForSvm$LM = as.factor(dataForSvm$LM)

#svm----
split = sample.split(dataForSvm$LM, SplitRatio = 0.10) 
training_set = subset(dataForSvm, split == TRUE) 
test_set = subset(dataForSvm, split == FALSE) 
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

levels(dataForSvm$LM)
make.names(levels(dataForSvm$LM))


getIndexsOfColumns <- function(t,column_names){
  return(match(column_names,colnames(t)))
}
samp = downSample(training_set[-getIndexsOfColumns(training_set, c("LM") )],training_set$LM,yname="LM")
#choose small data for tuning 
train_index_tuning = createDataPartition(samp$LM,p = 0.05,list=FALSE,times=1)
#choose small data for re-train
train_index_training = createDataPartition(samp$LM,p = 0.1,list=FALSE,times=1)
library(kernlab)
svmGrid = expand.grid(
  .sigma = as.numeric(sigest(LM ~.,data = samp[train_index_tuning,],scaled=FALSE)),
  .C = c(0.1,1,10)
)

ctrl <- trainControl(method = "cv",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     number = 3
)

table(dataForSvm$LM)

svmTuned = train(
  samp[train_index_tuning,-getIndexsOfColumns(samp,"LM")],
  y = samp[train_index_tuning,"LM"],
  method = "svmRadial",
  tuneGrid = svmGrid)


plot(svmTuned)

svmTuned
svm_model = ksvm(LM ~ .,
                 data = samp[train_index_training,],
                 kernel = "rbfdot",
#                 kpar = list(sigma=1.508191 ),
                 C = 10,
                 prob.model = TRUE,
                 scaled = c(),
                 metric = "ROC",
                 type = "C-svc",
                 trControl = ctrl,
                 preProcess = NULL,
                 fit = FALSE)
svm_model_e_1 <- svm(LM~., data = training_set, kernel = "radial", gamma = 1, cost = 10)

predict_loan_status_svm = predict(svm_model, test_set, type="probabilities")
sss = as.data.frame(predict_loan_status_svm)
sss$"1"
predict_loan_status_svm = sss$"1"

rocCurve_svm = roc(response = test_set$LM,
                   predictor = predict_loan_status_svm)
auc_curve = auc(rocCurve_svm)
plot(rocCurve_svm,legacy.axes = TRUE,print.auc = TRUE,col="red",main="ROC(SVM)")
auc_curve
predict_loan_status_label = ifelse(predict_loan_status_svm<0.5,"0","1")
predict_loan_status_label=as.factor(predict_loan_status_label)
c = confusionMatrix(predict_loan_status_label,test_set$LM,positive="1")
c
table_perf = c("SVM",
                   round(auc_curve,3),
                   as.numeric(round(c$overall["Accuracy"],3)),
                   as.numeric(round(c$byClass["Sensitivity"],3)),
                   as.numeric(round(c$byClass["Specificity"],3)),
                   as.numeric(round(c$overall["Kappa"],3))
)

tail(table_perf, 1)
table_perf
plot(svm_model, data = training_set)
plot(svm_model_e_1, data = training_set)

