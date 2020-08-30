## Shiny server file
# install.packages('ggplot2')
# install.packages('muStat')
# install.packages('muStat')
# install.packages('grid')

library(ggplot2)
library(muStat)
library(scales)
library(grid)
library(shiny)
library(shinydashboard)
library(shinyjs)

## server function
shinyServer(function(input, output) {
  output$plot<-renderPlot({
    print('input')
    print(input)
    inputElements = NULL
    if(input$select == "80:20") {
      inputElements = 0.80
    }
    if(input$select == "70:30") {
      inputElements = 0.70
    }
    if(input$select == "60:40") {
      inputElements = 0.60
    }
    if(input$select == "50:50") {
      inputElements = 0.50
    }
    dataForSvm$LM=as.factor(dataForSvm$LM)
    split = sample.split(dataForSvm$LM, SplitRatio = inputElements) 
    training_set = subset(dataForSvm, split == TRUE) 
    test_set = subset(dataForSvm, split == FALSE) 
    
    # Normalizacija testnih i trenirajućih primjeraka
    training_set$cik<-normalize(training_set$cik)
    training_set$time<-normalize(training_set$time)
    test_set$cik<-normalize(test_set$cik)
    test_set$time<-normalize(test_set$time)
    
    # Korištenje svm funkcije iz e1071 paketa kako bi se kreirao model učenja
    # Korišteni su gamma i cost izvedeni iz genetičkog algoritma
    classifier = svm(formula = as.factor(LM) ~ ., 
                     data = training_set, 
                     kernel = "linear")
    
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
    points(grid_set, pch = '.', col = ifelse(y_grid == 0, 'coral1', 'aquamarine')) 
    points(set, pch = 21, bg = ifelse(set[, 3] == 0, 'red3','green4')) 
    
  })
})