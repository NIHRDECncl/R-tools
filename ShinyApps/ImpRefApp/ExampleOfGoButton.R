library(shiny)
ui <- pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    actionButton("goButton1", "Go1!"),
    actionButton("goButton2", "Go2!")
  ),
  mainPanel(
    plotOutput("distPlot")
  )
)

server <- function(input, output) {
  dist <- eventReactive(input$goButton1 | input$goButton2, {
    set.seed(123)
    data.frame(
      dist = rnorm(input$obs)
    )
    }, ignoreNULL = FALSE)

  # this looks rather roundabout, but it is meant to be a minimal representative example of the 
  #    clinical accuracy and utility app, which does not show plots until the actionButton is clicked
  output$distPlot <- renderPlot({
    hist(dist()$dist) 
  })
}

shinyApp(ui = ui, server = server)

