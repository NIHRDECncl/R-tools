#ui.R
library(shiny)
tagList(
  navbarPage( "Title",
              tabPanel("Navbar 1",
                       sidebarPanel(
                       ),
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Table",
                                    h4("Iris"),
                                    tableOutput("table")),
                           tabPanel("Text",
                                    h4("VText"),
                                    verbatimTextOutput("vtxt")),
                           tabPanel("Header")
                         )
                       )),
              tabPanel("Navbar 2")))
#server.R
function(input, output) {
  output$table <- renderTable({
    iris
    
  })
  
}