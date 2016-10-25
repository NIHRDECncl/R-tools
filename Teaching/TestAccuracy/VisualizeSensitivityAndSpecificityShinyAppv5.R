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
    
    vx <- reactive({Dpos()/input$n})  #?????how can we make this work
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

                            
    dx2x2df <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Fp = Fp(), Fn = Fn(), Tn = Tn())})
    temp <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Tn = Tn(), Fp = Fp(), Fn = Fn())})
        
    populationdf <- reactive( {
      y = c(runif(input$n, 0, 1))
      if (input$sorted){
        x = c(
          runif(Dpos(), 0, Dpos()/input$n),
          runif(Dneg(), Dpos()/input$n, 1)
        )
      } else {
        x = c(runif(input$n, 0, 1))
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
         geom_point(size = 4) + #scale_color_manual(values=c("#999999", "#E69F00"))  +
         geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, colour = result, shape = result), 
                      data = vdf()) 
      }
      
      p1 <- p1 +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        ) +
        labs(x = "", y = "") +
        ggtitle("People with/without a condition")
           p1

  })


  
  output$testedPlots<-renderPlot( {

    p2 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = testResult)) + geom_point(size = 4)
    if (input$sorted) {
   #   p2 <- p2 + geom_line(data = vdf(),aes(x = vx, y = vy), color = "red" , size = 0.8) # + 
      #  geom_line(aes(x = h2$hx, y = h2$hy), color = "red" , size = 0.8)
       
        p2 <- p2 + geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, colour = result, shape = result), 
                     data = vdf()) +
        geom_segment(aes(x = hx1, y = hy1, xend = hx2, yend = hy2, colour = result, shape = result), 
                     data = hdf1()) + 
        geom_segment(aes(x = hx3, y = hy3, xend = hx4, yend = hy4, colour = result, shape = result), 
                       data = hdf2())
    }
    p2 <- p2 + scale_color_manual(values=c("red", "blue", "green", "black")) +
        theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        ) +
        labs(x = "", y = "") +
        ggtitle("People with/without a condition, testing +ve/-ve")
    p2
    
  })

  
  output$stats <- renderPrint((dx2x2df()))
 
  output$popHead <- renderPrint((head(populationdf())))
  output$popTail <- renderPrint((tail(populationdf())))
  

}

shinyApp(ui=ui, server=server)
