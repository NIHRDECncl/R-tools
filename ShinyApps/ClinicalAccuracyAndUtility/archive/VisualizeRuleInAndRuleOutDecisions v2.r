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

  titlePanel(h4("Explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity")),

  sidebarLayout(
    sidebarPanel(
      actionButton("GoButton", "Click to update the graphic"),
      tags$br(),
      tags$h3("Input Variables"),
      textInput(inputId = "DxCondition", label = "Condition", value = "chronic heart failure"),
      textInput(inputId = "DxTestName", label = "Name of test", value = "CORS test"),
      numericInput("prevalence", "prior probability (prevalence)", min=0, max=1, value=0.30, step = 0.01),
      numericInput("n", "population", min=1, max=1000, value=680),
      numericInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.94, step = 0.01),
      numericInput("specificity", "specificity of index test", min=0, max=1, value= 0.48, step = 0.01),
      textInput(inputId = "RuleInDxDecision", label = "Rule-in decision", value = "refer to HF clinic"),
      numericInput("RuleInDecisionThreshold", "Rule-in PPV threshold", min=0, max=1, value= 0.75, step = 0.01),
      textInput(inputId = "IndeterminateDecision", label = "Indeterminate decision", value = "investigate further"),
      textInput(inputId = "RuleOutDxDecision", label = "Rule-out decision", value = "assess for other causes"),
      numericInput("RuleOutDecisionThreshold", "Rule-out NPV threshold", min=0, max=1, value= 0.05, step = 0.01)
    ),
    mainPanel(
      plotOutput("RuleInOutPlot"),
      tags$br(),
      # verbatimTextOutput("linesTable"),
      tags$br(),
      tags$b("Cite as:"),
      tags$br(),
      "Michael Power, Joy Allen.",
      tags$br(),
      tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity"),
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
  
  # values <- reactiveValues(
  #   default = 0,
  #   DxTestName <- "Test",
  #   DxDecision <- "Starting treatment",
  #   n <- 100,
  #   prevalence <- 0.1,
  #   sensitivity <- 0.9,  
  #   specificity <- 0.8,
  #   RuleInDecisionThreshold <- 0.8
  #   RuleOutDecisionThreshold <- 0.4
  #  )
  
    DxCondition <- eventReactive(input$GoButton, {input$DxCondition})
    DxTestName <- eventReactive(input$GoButton, {input$DxTestName})
    DxRuleInDecision <- eventReactive(input$GoButton, {input$DxRuleInDecision})
    DxRuleOutDecision <- eventReactive(input$GoButton, {input$DxRuleOutDecision})
    
    Dpos <- eventReactive(input$GoButton, {round(input$n * input$prevalence)})
    Dneg <- eventReactive(input$GoButton, {round(input$n - Dpos())})
    
    Tp <- eventReactive(input$GoButton, {round(input$sensitivity * Dpos())})
    Tn <- eventReactive(input$GoButton, {round(input$specificity * Dneg())})
    
    
    Fn <- eventReactive(input$GoButton, {round((1 - input$sensitivity) * Dpos())})
    Fp <- eventReactive(input$GoButton, {round((1 - input$specificity) * Dneg())})
    
    TestPosY <- eventReactive(input$GoButton, {Tp()/(Tp() + Fp())})
    TestNegY <- eventReactive(input$GoButton, {1 - Tn()/(Tn() + Fn())})
    
    
    ### coordinates and labels for contingency matrix graphic
    fixedlabels <- reactive({
      return({data.frame(
        x = c(0.35, 0.9, 0.35, 0.25),
        y = c(
          input$RuleInDecisionThreshold + 0.05, 
          input$prevalence + 0.05, 
          input$RuleOutDecisionThreshold + 0.05,
          0.5),
        labels = c(
          paste0(input$RuleInDecisionThreshold*100, "%  = threshold for rule-in decision: ", input$RuleInDxDecision),
          paste0("Prevalence = ", input$prevalence*100, "%"),
          # paste0("Threshold for rule-in decisions = ", input$RuleOutDecisionThreshold*100, "%  for ", input$RuleOutDxDecision),
          paste0(input$RuleOutDecisionThreshold*100, "%  = threshold for rule-out decision: ", input$RuleOutDxDecision),
          
          paste0("Action when indeterminate: ", input$IndeterminateDecision))
       ) })
    })

    postTestLabels <- reactive({
      return({data.frame(
        x = c(0.9, 0.9),
        y = c(TestPosY() + 0.045, TestNegY()) - 0.035,
        labels = c(
          strwrap(paste0("Prob post +ve test = ", round(TestPosY()*100), "%"), 40),
          strwrap(paste0("Prob post -ve test = ", round(TestNegY()*100), "%"), 40)
        )
      )
      })
    })
    
    

    linesDf <- eventReactive(input$GoButton, {

        return({data.frame(
          PriorAxisX = c(0, 0),
          PriorAxisY <- c(0, 1),
          
          PostAxisX <- c(1, 1),
          PostAxisY <- c(0, 1),
          
          PrevX <- c(0, 1),
          PrevY <- c(input$prevalence, input$prevalence),
          
          RuleInDecisionThresholdX <- c(0, 1),
          RuleInDecisionThresholdY <- c(input$RuleInDecisionThreshold, input$RuleInDecisionThreshold),

          RuleOutDecisionThresholdX <- c(0, 1),
          RuleOutDecisionThresholdY <- c(input$RuleOutDecisionThreshold, input$RuleOutDecisionThreshold),
          
          TestPosX <- c(0, 1),
          TestPosY <- c(input$prevalence, Tp()/(Tp() + Fp())),
          
          TestNegX <- c(0, 1),
          TestNegY <- c(input$prevalence, 1 - Tn()/(Tn() + Fn()))
          )
        })
      })

  marginInsidePlot = 0.01
  
  output$linesTable <- renderPrint(linesDf())
  
  output$RuleInOutPlot<-renderPlot({

    ggplot(linesDf()) +
      geom_line(aes(x = linesDf()$PriorAxisX, y = linesDf()$PriorAxisY), data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$PostAxisX, y = linesDf()$PostAxisY), data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$PrevX, y = linesDf()$PrevY, colour="coral1"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$RuleInDecisionThresholdX, y = linesDf()$RuleInDecisionThresholdY, colour="cadetblue"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$RuleOutDecisionThresholdX, y = linesDf()$RuleOutDecisionThresholdY, colour="springgreen4"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$TestPosX, y = linesDf()$TestPosY), colour = "firebrick4", size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), colour = "springgreen4", size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      theme(
        axis.text.x = element_blank(),
        # axis.text.y = element_blank(),
        # axis.ticks = element_blank(),
        legend.position="none"
      ) +
      labs(x = "", y = paste0("probability of ", DxCondition())) +
      ggtitle(paste("Post-test probabilities after", DxTestName(), "for", DxCondition())) +
      theme(plot.title = element_text(size = 14, face = "bold")) +
      geom_text(data = fixedlabels(), size = 4, aes(x,y,label = labels)) + 
      geom_text(data = postTestLabels(), size = 3, aes(x, y, label = labels))
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
