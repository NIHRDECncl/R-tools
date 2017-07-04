ui <- fluidPage(
  
  tags$style(HTML(".irs-bar {width: 5%; height: 5px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}")),
  tags$style(HTML(".irs-bar-edge {background: black; border: 1px solid black; height: 5px; border-radius: 5px 5px 5px 5px;}")),
  tags$style(HTML(".irs-line {border: 1px solid black; height: 10px;}")),
  tags$style(HTML(".irs-grid-text {font-family: 'arial'; color: black}")),
  tags$style(HTML(".irs-max {font-family: 'arial'; color: red;}")),
  tags$style(HTML(".irs-min {font-family: 'arial'; color: black;}")),
  tags$style(HTML(".irs-single {color:black; background:#6666ff;}")), 
  tags$style(HTML(".irs-slider {width: 1px; height: 30px; top: 5px;}")),
  
  uiOutput("testSlider")
  
)

server <- function(input, output, session){
  
  output$testSlider <- renderUI({
    sliderInput( 
      inputId="test",
      label=NULL,
      min=1,
      max=10, 
      value=c(5, 7),
      step = 1,
      width='10%'
    ) # close slider input          
  }) # close renderUI
  
}

shinyApp(ui = ui, server=server)