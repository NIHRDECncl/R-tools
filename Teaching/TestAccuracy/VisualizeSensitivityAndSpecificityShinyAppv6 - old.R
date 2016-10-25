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
      checkboxInput("sorted", label = "Population sorted by presence of condition and test result", value = FALSE)
  
    ),
    mainPanel(
      verbatimTextOutput("temp"),
      tags$br(),

      plotOutput("populationPlot"),
      tags$br(),
      plotOutput("testedPlots"),
      verbatimTextOutput("stats"),
      # tags$br(),
      # verbatimTextOutput("popHead"),
      # tags$br(),
      # verbatimTextOutput("popTail"),
      # 
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
    
    vx <- reactive({Dpos()/input$n})
    vy <- 0
    vxend <- reactive({vx()})
    vyend <- 1
    
    vdf <- reactive({return({data.frame(vx = vx(),vy = vy,vxend = vxend(), vyend = vyend)})})

    hx1 <- 0
    hy1 <- reactive({Fn()/Dpos()})
    
    hx2 <- reactive({Dpos()/input$n})
    hy2 <- reactive({hy1()})
    
    hdf1 <-reactive({return({data.frame(hx1 = hx1, hy1 = hy1(), hx2 = hx2(), hy2 = hy2())})})
    
    hx3 <- reactive({hx2()})
    hy3 <- reactive({Tn()/Dneg()})
    
    hx4 <- 1
    hy4 <- reactive({hy3()})
    
    hdf2 <-reactive({return({data.frame(hx3 = hx3(), hy3 = hy3(), hx4 = hx4, hy4 = hy4())})})

                            
    dx2x2df <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Fp = Fp(), Fn = Fn(), Tn = Tn(), Tpos = Tp() + Fp(), Tneg = Fn() + Tn(), row.names = "")})
    temp <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Tn = Tn(), Fp = Fp(), Fn = Fn(), Tpos = Tp() + Fp(), Tneg = Fn() + Tn(), row.names = "")})
    
    marginInsidePlot = 0.01
    
    
    contingencyMatrix <- reactive({
        cmX <- C(
          0.5 * Dpos()/input$n,                # x for Tp
          Dpos()/input$n + 0.5*Dneg()/input$n, # x for Fp
          0.5 * Dpos()/input$n,                # x for Fn
          Dpos()/input$n + 0.5*Dneg()/input$n  # x for Tn
        )
        cmY <- c(
          (Fn() + 0.5 * Tp())/input$n,         # y for Tp
          (Tn() + 0.5 * Fp())/input$n,         # y for Fp
          (0.5*Fn())/input$n,                  # y for Fn
          (0.5*Tn())/input$n                   # y for Tn
        )
        cmLabels <- c(
          paste("Tp = ", Tp()),
          paste("Fp = ", Fp()),
          paste("Fn = ", Fn()),
          paste("Tn = ", Tn())
        )
      return(data.frame(cmX, cmY, cmLabels))
    })
  
    populationdf <- reactive( {
      if (input$sorted){
        x = c(
          runif(Tp(), min = marginInsidePlot, max = Dpos()/input$n) - marginInsidePlot,
          runif(Fn(), min = marginInsidePlot, max = Dpos()/input$n) - marginInsidePlot,
          runif(Fp(), min = Dpos()/input$n + marginInsidePlot, max = 1 - marginInsidePlot),
          runif(Tn(), min = Dpos()/input$n + marginInsidePlot, max = 1 - marginInsidePlot)
        )
        y = c(
          runif(Tp(), min = Fn()/(Tp() + Fn()) + marginInsidePlot, max = 1 - marginInsidePlot),
          runif(Fn(), min = marginInsidePlot, max =  Fn()/(Tp() + Fn())) - marginInsidePlot,
          runif(Fp(), min = Tn()/(Fp() + Tn()) + marginInsidePlot, max = 1 - marginInsidePlot),
          runif(Tn(), min = marginInsidePlot, max = Tn()/(Fp() + Tn()) - marginInsidePlot)
        )
        
      } else {
        x = c(runif(input$n, min = marginInsidePlot, max = 1 - marginInsidePlot))
        y = c(runif(input$n, min = marginInsidePlot, max = 1 - marginInsidePlot))
      }
      
      
    return( {
     data.frame(
       ID = 1:input$n,
        condition = c(
          rep(paste("Present  = ", Dpos()), times = Dpos()),
          rep(paste("Absent = ",Dneg()), times = Dneg())
        ),
       
       testResult = c(
         rep(paste("TestPos = ", Tp() + Fp()), times = Tp() + Fp()),
         rep(paste("TestNeg = ", Fn() + Tn()), times = Fn() + Tn()) 
       ),
       
       result = c(
         rep(paste("TruePos = ", Tp()), times = Tp()),
         rep(paste("FalseNeg = ", Fn()), times = Fn() ), 
         rep(paste("FalsePos = ", Fp()), times = Fp() ), 
         rep(paste("TrueNeg = ", Tn()), times = Tn())
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
         geom_point(size = 4) + #scale_color_manual(values=c("#999999", "#E69F00"))  +
         geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, colour = NULL, shape = NULL), 
                      data = vdf()) 
      }
      
      p1 <- p1 +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        ) +
        labs(x = "", y = "") +
        ggtitle("Population of people with and without a condition")
           p1

  })


  
  output$testedPlots<-renderPlot( {

    p2 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = result)) + geom_point(size = 4)
    if (input$sorted) {
      #   p2 <- p2 + geom_line(data = vdf(),aes(x = vx, y = vy), color = "red" , size = 0.8) # + 
      #  geom_line(aes(x = h2$hx, y = h2$hy), color = "red" , size = 0.8)
       
        p2 <- p2 + geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, colour = NULL, shape = NULL), 
                     data = vdf()) +
        geom_segment(aes(x = hx1, y = hy1, xend = hx2, yend = hy2, colour = NULL, shape = NULL), 
                     data = hdf1()) + 
        geom_segment(aes(x = hx3, y = hy3, xend = hx4, yend = hy4, colour = NULL, shape = NULL), 
                       data = hdf2()) 
                      +
        geom_text(data = contingencyMatrix(), mapping = aes(x=cmX, y=cmY, label = cmLabels))
    }
    p2 <- p2 + scale_color_manual(values=c("red", "blue", "green", "black")) +
        theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        ) +
        labs(x = "", y = "") +
        ggtitle("Population of people with test results: true and false positive; false and true negative")
    p2
    
  })

  
  output$stats <- renderPrint((dx2x2df()))
 
  output$popHead <- renderPrint((head(populationdf())))
  output$popTail <- renderPrint((tail(populationdf())))
  

}

shinyApp(ui=ui, server=server)
