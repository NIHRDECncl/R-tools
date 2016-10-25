###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False postives, False negatives, and True negatives.

library(shiny)
library(ggplot2)

ui<-fluidPage(
  sidebarLayout(
    sidebarPanel(
      
#### input variables
      # n (population size)
      # prevalence (of condition)
      # sensitivity (of index test)
      # specificity (of index test)
      
      
      tags$h3("Input Variables"),
      sliderInput("n", "population", min=1, max=10000, value=1000),
      sliderInput("prevalence", "prevalence of condition", min=0, max=1, value=.25),
      sliderInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90),
      sliderInput("specificity", "specificity of index test", min=0, max=1, value= 0.80)

    ),
    mainPanel(
      tags$h3("Visually explore the effects of sensitivity, specificity, and prevalence"),
      tags$h4("on True positives, False postives, False negatives, True negatives"),
      
      plotOutput("populationPlot"),
      plotOutput("testedPlots"),
      verbatimTextOutput("dx2x2Head"),
      verbatimTextOutput("stats"),
      verbatimTextOutput("populationHead"),
      
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
  
  dx2x2 <- reactive( {
    Dpos <- input$population * input$prevalence
    Dneg <- input$population - Dpos
    
    Tp <- input$sensitivity * Dpos
    Tn <- input$specificity * Dneg
    
    
    Fp <- (1 - input$sensitivity) * Dpos
    Fn <- (1 - input$specificity) * Dneg
    
    TestPos <- Tp + Fp
    TestNeg <- Fp + Tn
    
    return( {
      data.frame(
        Dpos = Dpos, 
        Dneg = Dneg, 
        Tp = Tp, 
        Fp = Fp, 
        Fn = Fn, 
        Tn = Tn, 
        TestPos = TestPos, 
        TestNeg = TestNeg,
        stringsAsFactors = TRUE,
        row.names = NULL
      )
    })
  })      
    population <- reactive( {

      dx2x2df <- dx2x2()
      nTp  = dx2x2$TestPos
      nTn  = dx2x2$TestNeg
      
      data.frame(
       ID = 1:input$n,
        condition = c(
          rep("Present", times = nTp), 
          rep("Absent", times = nTn)
        ),
      x = runif(input$population, 0, 1), # x co-ordinate for plotting population
      y = runif(input$population, 0, 1), # x co-ordinate for plotting population
      
      testResult = c(
        rep("TruePos", dx2x2()$Tp), 
        rep("FalsePos", dx2x2()$Fp), 
        rep("FalseNeg", dx2x2()$Fn), 
        rep("TrueNeg", dx2x2()$Tn) 
        ),
      
      tpx = runif(input$population, 0, 1), # x co-ordinate for plotting test postives
      tpy = runif(input$population, 0, 1), # x co-ordinate for plotting test postives
      
      tnx = runif(input$population, 0, 1), # x co-ordinate for plotting test negatives
      tny = runif(input$population, 0, 1), # x co-ordinate for plotting test negatives
      
      stringsAsFactors = TRUE,
      row.names = NULL
    )
  })

  output$populationPlot<-renderPlot({
    par(mfrow=c(1,2))
    populationdf <- population()
    p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition)) + geom_point(shape=1)
    p1

  })

  output$dx2x2Head <- renderTable(dx2x2())
  
  output$populationHead <- renderText(head(population())) 
    
  output$testedPlots<-renderPlot( {
    par(mfrow=c(1,2))
    populationdf <- population()
    p2 <- ggplot(populationdf[testResult == "TruePos":"FalsePos"], aes(x=tpx, y=tpy, color=condition)) + geom_point(shape=1)
    p2 <- p2 + ggplot(populationdf[testResult == "FalseNeg":"TrueNeg"], aes(x=tnx, y=tny, color=condition)) + geom_point(shape=1)
    
  })

  
  output$stats <- renderTable(dx2x2())
}

shinyApp(ui=ui, server=server)