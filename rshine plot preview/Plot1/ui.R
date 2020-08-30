library(shiny)

shinyUI(fluidPage(
  titlePanel(strong(("Types of SVM"))),
  
  sidebarLayout(
    sidebarPanel(
      helpText("SVM with RBF kernel"),
      fluidRow(
        column(8,
               selectInput("select", label = strong("Select the split between training and test"), 
                           choices = list("80:20" = "80:20",
                                          "70:30" = "70:30", 
                                          "60:40" = "60:40",
                                          "50:50" = "50:50",
                                          "Total"="Total"),selected = "50:50")),
               br()),
      fluidRow(
        column(8,
               numericInput(
                 "selectGamma",
                 label = strong("Select the gamma value"),
                 1,
                 min = NA,
                 max = NA,
                 step = NA,
                 width = NULL
               )),
        br()),
      fluidRow(
        column(8,
               numericInput(
                 "selectCost",
                 label = strong("Select the cost value"),
                 100,
                 min = NA,
                 max = NA,
                 step = NA,
                 width = NULL
               )),
        br()),
    ),
    mainPanel(
      plotOutput("plot",height=600,width=600)
    )
)))