#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


library(shiny)
# Define server logic for slider examples

shinyServer(function(input, output) {
  #test
  # Reactive expression to compose a data frame containing all of the values
  sliderValues <- reactive({
    
    # Compose data frame
    data.frame(
      Name = c("Prevalence",
               "Expected sensitivity of index test",
               "Confidence interval for index test sensitivity",
               "alpha",
               "beta"
               ),
      Value = as.character(c(input$prev,
                             input$SnI,
                             input$CI,
                             input$alpha,
                             input$beta
                             )), 
      stringsAsFactors=FALSE)
  }) 
  
  # Show the values using an HTML table
  output$values2 <- renderTable({
    sliderValues()
  })
  
  formula <-reactive({
    samplesize(input$prev, input$SnI, input$CI, input$alpha, input$beta)
  })
  
  output$view <- renderTable({
    formula()
  })
})

