library(shiny)
ui <- fluidPage(
  sliderInput("slider", "Move me", value = 5, 1, 10),
  numericInput("num", "Number", value = 5, 1, 10)
)
server <- function(input, output, session) {
  observe({
    updateNumericInput(session, "num", value = input$slider)
  })
}
shinyApp(ui = ui, server = server)