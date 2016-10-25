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
    
    
    Fn <- reactive({round((1 - input$sensitivity) * Dpos())})
    Fp <- reactive({round((1 - input$specificity) * Dneg())})
    
    vx <- 1 #Dpos()/input$n  #?????how can we make this work
    vy <- 0
    vxend <- vx
    vyend <- 1
    
    vdf <- data.frame(vx,vy,vxend,vyend)

    hx1 <- 0
    hy1 <- 0.5 # Fn()/Dpos()
    
    hx2 <- 0.5 # Dpos()/input$n
    hy2 <- hy1
    
    hx3 <- hx2
    hy3 <- 0.5 # Tn/Dneg()
    
    hx4 <- 1
    hy4 <- hy3
    

                            
    dx2x2df <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Fp = Fp(), Fn = Fn(), Tn = Tn())})
    temp <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Tn = Tn(), Fp = Fp(), Fn = Fn())})
        
    populationdf <- reactive( {
      if (input$sorted){
        x = c(
          runif(Dpos(), 0, Dpos()/input$n),
          runif(Dneg(), Dpos()/input$n, 1)
        )
        y = c(
          runif(Tp(), (Tn() + Fn())/input$n,1), ### ???? why is TP() one too much????
          runif(Fn(), 0, (Tn() + Fn())/input$n),
          runif(Fp(), (Tn() + Fn())/input$n, 1),
          runif(Tn(), 0, (Tn() + Fn())/input$n)
        )
      } else {
        x = c(runif(input$n, 0, 1))
        y = c(runif(input$n, 0, 1))
      }
      
      
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
         rep("FalseNeg", times = Fn() ), 
         rep("FalsePos", times = Fp() ), 
         rep("TrueNeg", times = Tn())
       ),
       x, 
       y
    )
  })
})
    


  output$temp <- renderPrint(temp())
  
  
    output$populationPlot<-renderPlot({
      p1 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = condition)) + geom_point(size = 4) +
        scale_color_manual(values=c("#999999", "#E69F00"))
      if (input$sorted) {
       p1 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = condition)) + 
         geom_point(size = 4) +  #         scale_color_manual(values=c("#999999", "#E69F00")) + 
         geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, color = "red", shape = "identity") , size = 0.8)
     }
     p1

  })


  
  output$testedPlots<-renderPlot( {
    v2 <- vLine()
    h2 <- hLine()
    p2 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = testResult)) + geom_point(size = 4)
    if (input$sorted) {
      p2 <- p2 + geom_line(aes(x = v2$vx, y = v2$vy), color = "red" , size = 0.8) + 
        geom_line(aes(x = h2$hx, y = h2$hy), color = "red" , size = 0.8)
    }
    p2 <- p2 + scale_color_manual(values=c("red", "blue", "green", "black"))
    p2
    
  })

  
  output$stats <- renderPrint((dx2x2df()))
 
  output$popHead <- renderPrint((head(populationdf())))
  output$popTail <- renderPrint((tail(populationdf())))
  

}

shinyApp(ui=ui, server=server)