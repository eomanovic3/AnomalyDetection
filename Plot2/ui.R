library(shiny)

shinyUI(fluidPage(
  includeCSS("styles.css"),
  headerPanel("Linear SVM"),
  div(class="labelSelect", "Select the split between training and test"),
  fluidRow(
        column(12,
               tags$style(type='text/css', ".selectize-input { margin-left: 40px;}"),
               selectInput("select", label=NULL,
                           choices = list("80:20" = "80:20",
                                          "70:30" = "70:30", 
                                          "60:40" = "60:40",
                                          "50:50" = "50:50"),selected = "50:50")),
        br(),
        plotOutput("plot",height=400,width=400)
      )
   )
)