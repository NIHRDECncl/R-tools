#
# Calculator for one-sided confidence intervals for proportions

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(PropCIs)
library(httr)
library(tidyverse)


# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Calculate one-sided confidence intervals for proportions"),
   
   # Sidebar 
   sidebarLayout(
      sidebarPanel(
        
        withTags(
          h4("Enter the count and total (i.e. the numerator and denominator of the proportion)")
        ),
        
        numericInput("x",
                     "Count (numerator)",
                     min = 1,
                     max = 10000000000,
                     value = 10),
         
        numericInput("n",
                     "Total (denominator)",
                     min = 1,
                     max = 10000000000,
                     value = 100),
        
        sliderInput("oneCIlevel",
                     "Confidence level",
                     min = 0,
                     max = 1,
                     value = 0.95)
      ),
      
      # show the confidence intervals
      mainPanel(
        textOutput("lb"),
        tags$br(),
        textOutput("ub"),
        tags$br(),
        tags$br(),
        tags$br(),
        tableOutput("t")

        
      )
   )
)

# Define server logic required to compute one-sided confidence intervals for a proportion
server <- function(input, output) {
  
output$t <- renderTable(
  select(OneCI4prop(input$x, input$n, input$oneCIlevel), CILL, ObservedProp, CIUL, level)
         )  

output$lb <- renderText(OneCI4prop(input$x, input$n, input$oneCIlevel)$CIlbtxt)  
output$ub <- renderText(OneCI4prop(input$x, input$n, input$oneCIlevel)$CIubtxt)  

}

############################
#
#  OneCI4prop function to calculate upper and lower bounds for 1-sided confidence intervals for a proportion
#
############################

OneCI4prop <- function(x, n, CIlevel = 0.95) {
  # x = count (numerator)
  # n = total (denominator)
  # CIlevel = level of confidence interval, default of 95%
  
  # set inputs for testing
  # x <- 20
  # n <- 240
  # CIlevel <- 0.95
  
  
  TwoSidedConfLevel <- 1 - (1 - CIlevel)*2 #  2-sided confidence interval for 1-sided CI calculation
  xCI <- PropCIs::scoreci(x, n, TwoSidedConfLevel)  # Wilson’s confidence interval for a single proportion
  
  CI <- as.data.frame(xCI[1], stringsAsFactors = FALSE)
  
  CIlevelPct <- round(CIlevel*100, digits = 0)
  
  prop <- round(x/n, digits = 4)
  lb <- round(CI$conf.int[1], digits = 4)
  ub <- round(CI$conf.int[2], digits = 4)
  
  OneCIub <- paste0(CIlevelPct, "% 1-sided CI upper bound: less than or equal to ", ub, " for observed proportion ", prop)
  OneCIlb <- paste0(CIlevelPct, "% 1-sided CI lower bound: greater than or equal to ", lb, " for observed proportion ", prop)
  
  OneSidedConfInt <- data.frame(CILL = CI$conf.int[1], ObservedProp = x/n, CIUL = CI$conf.int[2], level = CIlevel, CIlbtxt = OneCIlb, CIubtxt = OneCIub, stringsAsFactors = FALSE)
  
  return(OneSidedConfInt)
  
}

############################







# Run the application 
shinyApp(ui = ui, server = server)

