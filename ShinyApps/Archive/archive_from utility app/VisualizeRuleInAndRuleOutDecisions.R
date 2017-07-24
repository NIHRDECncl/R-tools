###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False positives, False negatives, and True negatives.

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)


#######################################################
# formula to calculate confidence interval for a proportion with 
#  y = numerator
#  n = denominator
#  y/n = observed proportion
#
#  Uses adjusted Wald method to ensure CI is calculated appropriately when n is small ( ie <5 )
#
### references:
#   Approximate Is Better than "Exact" for Interval Estimation of Binomial Proportions
#   Alan Agresti; Brent A. Coull
#   The American Statistician, Vol. 52, No. 2. (May, 1998), pp. 119-126.
#   http://www.stat.ufl.edu/~aa/articles/agresti_coull_1998.pdf
#
#   http://www.bmj.com/content/318/7177/193.3
#
#   http://influentialpoints.com/Training/confidence_intervals_of_proportions.htm

ciproportion <- function(y,n) {
  alpha <- 0.05 # specifies 95% interval
  
  z <- qnorm(1-alpha/2)
  pw <- (y+z)/(n+z^2)              # Wilson point estimator
  se <- sqrt(pw*(1-pw)/n)          # est se of pw
  return(data.frame(
    # cill <- qnorm(alpha/2,pw,se),  # lower 95% limit
    # ciul <- qnorm(1-alpha/2,pw,se) # upper 95% limit 
    cill <- max(0, qnorm(alpha/2,pw,se)),  # lower 95% coerce to >= 0
    ciul <- min(1, qnorm(1-alpha/2,pw,se)) # upper 95% coerce to =< 1
  ))
}
#######################################################

#################### ui ###############################

ui<-fluidPage(

  titlePanel(h4("Visually explore the effects of prevalence, sensitivity, and specificity on Rule in and Rule out decisions")),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("GoButton", "Click to update the graphic"),
      tags$br(),
      tags$h3("Input Variables"),
      textInput(inputId = "DxTestName", label = "Name of test", value = "Test"),
      textInput(inputId = "DxRuleInDecision", label = "Decison on basis of +ve test", value = "Start treatment"),
      textInput(inputId = "DxRuleOutDecision", label = "Decison on basis of -ve test", value = "Stop/withhold treatment"),
      sliderInput("n", "population", min=1, max=1000, value=100),
      sliderInput("prevalence", "prior probability (prevalence)", min=0, max=1, value=.1),
      sliderInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90),
      sliderInput("specificity", "specificity of index test", min=0, max=1, value= 0.80),
      sliderInput("DecisionThreshold", "Threshold for decision-making", min=0, max=1, value= 0.80)
    ),
    mainPanel(
      plotOutput("RuleInPlot"),
      tags$br(),
      plotOutput("RuleOutPlot"),
      tags$br(),
      verbatimTextOutput("linesTable"),
      tags$br(),
      tags$b("Cite as:"),
      tags$br(),
      "Michael Power, Joy Allen.",
      tags$br(),
      tags$em("A web app to explore prior probability, sensitivity, and specificity on diagnostic "),
      tags$br(),
      "NIHR Diagnostic Evidence Co-operative Newcastle. September 2016",
      tags$br()
      # verbatimTextOutput("lines")
    )
  )
)

#######################################################

#################    server     ################

server<-function(input, output) {
    
    DxTestName <- eventReactive(input$GoButton, {input$DxTestName})
  
    Dpos <- eventReactive(input$GoButton, {round(input$n * input$prevalence)})
    Dneg <- eventReactive(input$GoButton, {round(input$n - Dpos())})
    
    Tp <- eventReactive(input$GoButton, {round(input$sensitivity * Dpos())})
    Tn <- eventReactive(input$GoButton, {round(input$specificity * Dneg())})
    
    
    Fn <- eventReactive(input$GoButton, {round((1 - input$sensitivity) * Dpos())})
    Fp <- eventReactive(input$GoButton, {round((1 - input$specificity) * Dneg())})


    linesDf <- eventReactive(input$GoButton, {

        return({data.frame(
          PriorAxisX = c(0, 0),
          PriorAxisY <- c(0, 1),
          
          PostAxisX <- c(1, 1),
          PostAxisY <- c(0, 1),
          
          PrevX <- c(0, 1),
          PrevY <- c(input$prevalence, input$prevalence),
          
          DecisionThresholdX <- c(0, 1),
          DecisionThresholdY <- c(input$DecisionThreshold, input$DecisionThreshold),
          
          TestPosX <- c(0, 1),
          TestPosY <- c(input$prevalence, Tp()/(Tp() + Fp())),
          
          TestNegX <- c(0, 1),
          TestNegY <- c(input$prevalence, 1 - Tn()/(Tn() + Fn()))
          )
        })
      })
  marginInsidePlot = 0.01

  
  output$linesTable <- renderPrint(linesDf())
  
  output$RuleInPlot<-renderPlot({
    ggplot(linesDf()) +
      geom_line(aes(x = linesDf()$PriorAxisX, y = linesDf()$PriorAxisY), data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$PostAxisX, y = linesDf()$PostAxisY), data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$PrevX, y = linesDf()$PrevY, colour="coral1"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$DecisionThresholdX, y = linesDf()$DecisionThresholdY, colour="cadetblue"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$TestPosX, y = linesDf()$TestPosY), colour = "firebrick4", size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), colour = "springgreen4", size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position="none"
      ) +
      labs(x = "", y = "") +
      ggtitle(paste(input$RuleInDecision, DxTestName()))
  })

    output$RuleOutPlot<-renderPlot({
      ggplot(linesDf()) +
        geom_line(aes(x = linesDf()$PriorAxisX, y = linesDf()$PriorAxisY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$PostAxisX, y = linesDf()$PostAxisY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$PrevX, y = linesDf()$PrevY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$DecisionThresholdX), y = linesDf()$DecisionThresholdY, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$TestPosX, y = linesDf()$TestPosY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), data = linesDf(), stat = "identity", position = "identity") +
        theme(
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position="none"
        ) +
        labs(x = "", y = "") +
      ggtitle(paste(input$RuleInDecision, input$DxTestName))
  })
  


      #       ### label the cells of the contingency matrix
    #       geom_text(data = contingencyM(), size = 5, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL))
    #
    #     if (input$ciFlag) {
    #       p2 <- p2 +
    # ### rectangles to show 95% CIs
    #         annotate("rect", xmin = linesDf()$vxlci, xmax = linesDf()$vxuci, ymin = 0, ymax = 1,
    #                           colour = "blue", alpha = 0.2) +
    #         annotate("rect", xmin = linesDf()$vxlci, xmax = linesDf()$vxuci, ymin = 0, ymax = 1,
    #                  colour = "blue", alpha = 0.2) +
    #         annotate("rect", xmin = 0, xmax = linesDf()$vx, ymin = linesDf()$hy1lci, ymax = linesDf()$hy1uci,
    #                  colour = "red", alpha = 0.2) +
    #         annotate("rect", xmin = linesDf()$vx, xmax = 1, ymin = linesDf()$hy2lci, ymax = linesDf()$hy2uci,
    #                  colour = "red", alpha = 0.2)
        }

shinyApp(ui=ui, server=server)
