#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for slider demo application
shinyUI(pageWithSidebar(

  #  Application title
  headerPanel("Sample Size calculator"),
  
  # Sidebar with sliders that demonstrate various available options
  sidebarPanel(
   
    # Prevalence
    sliderInput("prev", "Prevalence",
                min = 0, max = 1, value = 0.3, step= 0.01),
    
    # True sens of index test
     sliderInput("SnI", "Expected sensitivity of index test", 
                 min = 0, max = 1, value = 0.95, step = 0.05),
     
    # Confidence interval of index test
    sliderInput("CI", "Confidence interval for sensitivity of index test", 
                 min = 0, max = 0.5, value = 0.05, step = 0.01),
    
    # Alpha - proba of type 1 error - false pos conclusion
    sliderInput("alpha", "Alpha - probability of type 1 error", 
                min = 0, max = 0.2, value = 0.05, step = 0.01),
    
    # Beta - proba of type 1 error - false neg conclusion
    sliderInput("beta", "Beta - probability of type 2 error", 
                min = 0, max = 0.4, value = 0.2, step = 0.01)
      

  ),
  
  # Show a table summarizing the values entered
  mainPanel(
    
    tableOutput("values2"),
    
    tableOutput("view")
    
     )
))
