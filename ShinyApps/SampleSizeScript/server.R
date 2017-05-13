#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

samplesize <- function(prev, SnI, CI, alpha, beta){
  
  z_a = qnorm(1-alpha/2, mean = 0, sd = 1, log = FALSE) # currently set up for two tail
  z_b = qnorm(1-beta, mean = 0, sd = 1, log = FALSE)
  
  
  n_obs = (z_a*sqrt(SnI*(1-SnI)) + z_b*sqrt((SnI-CI)*(1-(SnI-CI))))^2/CI^2
  n = n_obs/prev  
  
  s_size = 
    data.frame(
      Name = c("Number of events required", 
               "Sample size required"),
      Value = c(n_obs, 
                n))
  return(s_size)
}
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

