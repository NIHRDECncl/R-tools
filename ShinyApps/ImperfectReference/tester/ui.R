library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins1",
                  "Number of bins1:",
                  min = 1,
                  max = 50,
                  value = 10),
      sliderInput("bins2",
                  "Number of bins2:",
                  min = 1,
                  max = 50,
                  value = 20)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # plotOutput("distPlot")
      tableOutput("Table")
    )
  )
))
