library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)


bcl <- read.csv("bcl-data.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("test"),
  sidebarLayout(
    sidebarPanel(

    ),
    mainPanel(
      tableOutput("sheets"),
      tableOutput("sheets"),
      tableOutput("conditionChoices")
    )  ))


server <- 
  function(input, output, session) {

  path <- "data/ConditionalSurvival.xlsx"
  sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
  names(sheets) <- "sheets"

  metadata4Plots <- read_excel(path, sheets$sheets[1])
  data4Plots <- read_excel(path, sheets$sheets[2])

  # initialise condition choices
  conditionChoices <-
    metadata4Plots$condition %>%
    sort() %>%
    unique()
  
    
    output$sheets <- renderTable(sheets)
    output$conditionChoices <- renderTable(conditionChoices)
  }
shinyApp(ui = ui, server = server)
