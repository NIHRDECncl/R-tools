###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False postives, False negatives, and True negatives.

library(shiny)
library(ggplot2)

ui<-fluidPage(

  titlePanel(h4("Visually explore the effects of sensitivity, specificity, and prevalence on True positives, False postives, False negatives, True negatives")),
  
  sidebarLayout(
    sidebarPanel(
     
      tags$h3("Input Variables"),
      sliderInput("n", "population", min=1, max=10000, value=1000),
      sliderInput("prevalence", "prevalence of condition", min=0, max=1, value=.1),
      sliderInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90),
      sliderInput("specificity", "specificity of index test", min=0, max=1, value= 0.80),
      checkboxInput("sorted", label = "Population sorted by presence of condition", value = TRUE)
  
    ),
    mainPanel(
      plotOutput("populationPlot"),
      tags$br(),
      plotOutput("testedPlots"),
      verbatimTextOutput("stats"),
      tags$br(),
      verbatimTextOutput("popHead"),
      tags$br(),
      verbatimTextOutput("popTail"),
      
      tags$br(),
      tags$b("Cite as:"),
      tags$br(),
      "Michael Power, Joy Allen.",
      tags$br(),
      tags$em("A web app to explore prevalence, sensitivity, and specificity on Tp, Fp, Fn, and Tn"),
      tags$br(),
      "NIHR Diagnostic Evidence Co-operative Newcastle. August 2016"
    )
  )
)

server<-function(input, output) {
  
  
    Dpos <- reactive({input$n * input$prevalence})
    Dneg <- reactive({input$n - Dpos()})
    
    Tp <- reactive({input$sensitivity * Dpos()})
    Tn <- reactive({input$specificity * Dneg()})
    
    
    Fp <- reactive({(1 - input$sensitivity) * Dpos()})
    Fn <- reactive({(1 - input$specificity) * Dneg()})
    
    TestPos <- reactive({Tp() + Fp()})
    TestNeg <- reactive({Fn() + Tn()})
    
    dx2x2df <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), 
               Fp = Fp(), Fn = Fn(), Tn = Tn(), TestPos = TestPos(), TestNeg = TestNeg())})

    # x <- reactive({runif(input$n, 0, 1)}) # x co-ordinate for plotting population
    # 
    # y <- reactive({  # y co-ordinate for plotting diseased
    #   if (input$sorted) {
    #    return( 
    #      c(
    #       runif(Dpos(), Dneg()/input$n, 1),
    #       runif(Dneg(), 0, Dneg()/input$n)
    #     )
    #   )
    # } else {
    #   return( runif(input$n, 0, 1)) # y co-ordinate for plotting population
    # }
    # })
    
    testResult<- reactive({
      c(
        rep("TruePos", times = Tp()),
        rep("FalsePos", times = Fp() + 1), # why have we lost one Fp here? 
        rep("FalseNeg", times = Fn() + 1), # why have we lost one Fn here? 
        rep("TrueNeg", times = Tn())
      )      
    }) 

    tpx <- reactive({runif(input$n, 0, 1)}) # x co-ordinate for plotting test postives
    tpy <- reactive({runif(input$n, 0, 1)}) # x co-ordinate for plotting test postives
    
    tnx <- reactive({runif(input$n, 0, 1)}) # x co-ordinate for plotting test negatives
    tny <- reactive({runif(input$n, 0, 1)}) # x co-ordinate for plotting test negatives
    
    populationdf <- reactive( {

    return( {
     data.frame(
      ID = 1:input$n,
      condition = c(
        rep("Present", times = TestPos()),
        rep("Absent", times = TestNeg())
      ),
      x = runif(input$n, 0, 1), # x co-ordinate for plotting population
      y = {        # y co-ordinate for plotting diseased
        if (input$sorted) {
          c(
            runif(Dpos(), Dneg()/input$n, 1),
            runif(Dneg(), 0, Dneg()/input$n)
          )
        } else {
            runif(input$n, 0, 1) # y co-ordinate for plotting population
        }
      },
      testResult = testResult(),
      tpx = tpx(), # x co-ordinate for plotting test postives
      tpy = tpy(), # x co-ordinate for plotting test postives

      tnx = tnx(), # x co-ordinate for plotting test negatives
      tny = tny # x co-ordinate for plotting test negatives
    )
  })
})
    
    


  output$populationPlot<-renderPlot({
    par(mfrow=c(1,2))
    p1 <- ggplot(populationdf(), aes(x=x, y=y, color=condition)) + geom_point(size = 4)
    p1 <- p1 + scale_color_manual(values=c("#999999", "#E69F00"))
    p1

  })

  
  output$testedPlots<-renderPlot( {
    par(mfrow=c(1,2))
    p2 <- ggplot(populationdf()[testResult == "TruePos":"FalsePos"], aes(x=tpx, y=tpy, color=condition)) + geom_point(shape=1)
    p2 <- p2 + ggplot(populationdf()[testResult == "FalseNeg":"TrueNeg"], aes(x=tnx, y=tny, color=condition)) + geom_point(shape=1)

  })

  
  output$stats <- renderPrint((dx2x2df()))
 
  output$popHead <- renderPrint((head(populationdf())))
  output$popTail <- renderPrint((tail(populationdf())))
  

}

shinyApp(ui=ui, server=server)