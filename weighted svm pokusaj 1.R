dataForSvm = dataForSvmBefore
dataForSvm = select(dataForSvm, time, cik, LM)
dataForSvm$LM = ifelse(dataForSvm$LM == 0, 'human', 'robot')
dataForSvm$LM = as.factor(dataForSvm$LM)
prop.table(table(dataForSvm$LM))


newX <- as.data.frame(dataForSvm$LM)
# $human
newY <- dataForSvm$new.Y

model1 <- wsvm(dataForSvm$LM, y, weight = rep(1,99))
