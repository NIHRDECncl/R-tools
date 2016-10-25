###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False positives, False negatives, and True negatives.

library(shiny)
library(ggplot2)
library(dplyr)

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
      verbatimTextOutput("tpx"),
      
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
    
    x <- reactive({runif(input$n, 0, 1)}) # x co-ordinate for plotting population
    yunsorted <- reactive({runif(input$n, 0, 1)}) # y co-ordinate for plotting population
    ysorted <- reactive({
      c(
        runif(Dpos(), Dneg()/input$n, 1),
        runif(Dneg(), 0, Dneg()/input$n)
      )
    })
  
    populationdf <- reactive( {
    return( {
     data.frame(
       ID = 1:input$n,
        condition = c(
          rep("Present", times = TestPos()),
          rep("Absent", times = TestNeg())
        ),
      x = x(), # x co-ordinate for plotting population
      ysorted = ysorted(),
      yunsorted = yunsorted(),
#      y = if(input$sorted) {ysorted} else {yunsorted()}, ## WHY DOES THIS GIVE AN ERROR?
       testResult = c(
         rep("TruePos", times = Tp()),
         rep("FalsePos", times = Fp() + 1), # why have we lost one Fp here? 
         rep("FalseNeg", times = Fn() + 1), # why have we lost one Fn here? 
         rep("TrueNeg", times = Tn())
         )
    )
  })
})
    

    testedPosX <- reactive({runif(Tp() + Fp(), 0, 1)}) # x co-ordinate for plotting test postives
    testedPosY <- reactive({runif(Tp() + Fp(), 0, 1)}) # x co-ordinate for plotting test postives
    
    TestedNegX <- reactive({runif(Fn() + Tn(), 0, 1)}) # x co-ordinate for plotting test negatives
    TestedNegY <- reactive({runif(Fn() + Tn(), 0, 1)}) # x co-ordinate for plotting test negatives
    
    df1 <- reactive({filter(populationdf(), testResult == "TruePos")})
    df2 <- reactive({filter(populationdf(), testResult == "FalsePos")})
    
  output$populationPlot<-renderPlot({
    par(mfrow=c(1,2))
    if (input$sorted) {
      p1 <- ggplot(populationdf(), aes(x=x, y=ysorted, color=condition)) + geom_point(size = 4)
    } else {
      p1 <- ggplot(populationdf(), aes(x=x, y=yunsorted, color=condition)) + geom_point(size = 4)
    }
    p1 <- p1 + scale_color_manual(values=c("#999999", "#E69F00"))
    p1

  })

  
  output$testedPlots<-renderPlot( {
    par(mfrow=c(1,2))
    p2 <- ggplot(df1(), aes(x=testedPosX(), y=testedPosy(), color=condition)) + geom_point(shape=1)
    p2 <- p2 + ggplot(df2(), aes(x=testedNegX(), y=testedNegY(), color=condition)) + geom_point(shape=1)
  })

  
  output$stats <- renderPrint((dx2x2df()))
 
  output$popHead <- renderPrint((head(populationdf())))
  output$popTail <- renderPrint((tail(populationdf())))
  
  output$tpx <- renderPrint((head(TestedNegX())))

}

shinyApp(ui=ui, server=server)