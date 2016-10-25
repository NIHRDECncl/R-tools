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
      sliderInput("n", "population", min=1, max=1000, value=100),
      sliderInput("prevalence", "prevalence of condition", min=0, max=1, value=.1),
      sliderInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90),
      sliderInput("specificity", "specificity of index test", min=0, max=1, value= 0.80),
      checkboxInput("sorted", label = "Population sorted by presence of condition", value = FALSE)
  
    ),
    mainPanel(
      verbatimTextOutput("temp"),
      tags$br(),

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
  
    Dpos <- reactive({round(input$n * input$prevalence)})
    Dneg <- reactive({round(input$n - Dpos())})
    
    Tp <- reactive({round(input$sensitivity * Dpos())})  ### this might be 1 too much???
    Tn <- reactive({round(input$specificity * Dneg())})
    
    
    Fp <- reactive({round((1 - input$sensitivity) * Dpos())})
    Fn <- reactive({round((1 - input$specificity) * Dneg())})
    
    vLine <- reactive({
        x <- c(Dpos()/input$n, Dpos()/input$n)
        y <- c(0, 1)
        return(data.frame(x,y))
      })
    
    hLine <- reactive({
          x <- c(0, Dpos()/input$n, Dpos()/input$n, 1)
          y <- c(
            Fn()/input$n, 
            Fn()/input$n, 
            Tn()/input$n,
            Tn()/input$n)
          return(data.frame(x,y))
        })
          
                            
    dx2x2df <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Fp = Fp(), Fn = Fn(), Tn = Tn())})
    temp <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Tn = Tn(), Fp = Fp(), Fn = Fn())})
        
    populationdf <- reactive( {
    return( {
     data.frame(
       ID = 1:input$n,
        condition = c(
          rep("Present", times = Dpos()),
          rep("Absent", times = Dneg())
        ),
       
       testResult = c(
         rep("TestPos", times = Tp() + Fp()),
         rep("TestNeg", times = Fn() + Tn()) 
       ),
       
       result = c(
         rep("TruePos", times = Tp()),
         rep("FalseNeg", times = Fn()), 
         rep("FalsePos", times = Fp()), 
         rep("TrueNeg", times = Tn())
       ),
       
      xunsorted = runif(input$n, 0, 1),
      xsorted = c(
        runif(Dpos(), 0, Dpos()/input$n),
        runif(Dneg(), Dpos()/input$n, 1)
        ),
      yunsorted = runif(input$n, 0, 1),
      ysorted = c(
        runif(Tp() -1, (Tn() + Fn())/input$n, 1), ### ???? why is TP() one too much????
        runif(Fn(), 0, (Tn() + Fn())/input$n),
        runif(Fp(), (Tn() + Fn())/input$n), 1,
        runif(Tn(), 0, (Tn() + Fn())/input$n)
      )
    )
  })
})
    


  output$temp <- renderPrint(temp())
  
  
    output$populationPlot<-renderPlot({
    if (input$sorted) {
      p1 <- ggplot(populationdf(), aes(x=xsorted, y=yunsorted, color=condition, shape = condition)) + geom_point(size = 4) 
#      p1 <- p1 + geom_line(color = "red", data = vLine(), aes(x = x, y = y), size = 0.8) #### ??? why does this cause an error? 
    } else {
      p1 <- ggplot(populationdf(), aes(x=xunsorted, y=yunsorted, color=condition, shape = condition)) + geom_point(size = 4)
    }
    p1 <- p1 + scale_color_manual(values=c("#999999", "#E69F00"))
    p1

  })

  
  output$testedPlots<-renderPlot( {

    if (input$sorted) {
      p2 <- ggplot(populationdf(), aes(x=xsorted, y=ysorted, color=result, shape = result)) + geom_point(size = 4) + geom_line(data = hLine(), aes(x = x, y = y), size = 0.8, color = "red") + geom_line(data = vLine(), aes(x = x, y = y), size = 0.8, color = "red") 
      p2 <- p2 + geom_line(data = vLine(), aes(x = x, y = y), size = 0.8) #### ??? why does this cause an error? 
      p2 <- p2 + geom_line(data = hLine(), aes(x = x, y = y), size = 0.8) #### ??? why does this cause an error? 
      
    } else {
      p2 <- ggplot(populationdf(), aes(x=xsorted, y=ysorted, color=testResult, shape = testResult)) + geom_point(size = 4)
    }
    p2 <- p2 + scale_color_manual(values=c("red", "blue", "green", "black"))
    p2
    
  })

  
  output$stats <- renderPrint((dx2x2df()))
 
  output$popHead <- renderPrint((head(populationdf())))
  output$popTail <- renderPrint((tail(populationdf())))
  

}

shinyApp(ui=ui, server=server)