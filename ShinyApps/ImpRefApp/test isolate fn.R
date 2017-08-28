
ui <-
  pageWithSidebar(
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    actionButton("GoButton", "Go!")
  ),
  mainPanel(
    plotOutput("distPlot"),
    textOutput("goButtonN")
  )
  )




server <- function(input, output, session) {
  observeEvent(input$GoButton, {
  output$distPlot <- renderPlot({
    # Take a dependency on input$goButton
    # input$goButton
    
    # Use isolate() to avoid dependency on input$obs
    dist <- isolate(input$obs)
    hist(rnorm(dist))
    output$goButtonN <- renderText(input$GoButton)
  })
  },   ignoreNULL = FALSE, ignoreInit = FALSE)
}

shinyApp(ui = ui, server = server)




