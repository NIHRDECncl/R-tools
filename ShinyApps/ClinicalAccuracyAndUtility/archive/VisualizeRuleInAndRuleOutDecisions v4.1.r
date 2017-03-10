###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False positives, False negatives, and True negatives.

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(proportion)
library(PropCIs)


#######################################################
# formula to calculate confidence interval for a proportion with 
#  y = numerator
#  n = denominator
#  y/n = observed proportion
#
#  Uses adjusted Wald method to ensure CI is calculated appropriately when n is small ( ie <5 )
#
### references:
#   http://vassarstats.net/clin1.html
#
#   Approximate Is Better than "Exact" for Interval Estimation of Binomial Proportions
#   Alan Agresti; Brent A. Coull
#   The American Statistician, Vol. 52, No. 2. (May, 1998), pp. 119-126.
#   http://www.stat.ufl.edu/~aa/articles/agresti_coull_1998.pdf
#
#   http://www.bmj.com/content/318/7177/193.3
#
#   http://influentialpoints.com/Training/confidence_intervals_of_proportions.htm


ciproportion <- function(y,n) {
  alpha <- 0.5 # specifies 95% interval
  h <- 2 # adding factor
# use ciALTx: Adjusted Logit-Wald method of CI estimation
  return(data.frame(
    cill <- ciALTx(y, n, alpha, h)$LALTx,  # adjusted logit lower 95% confidence limit
    ciul <- ciALTx(y, n, alpha, h)$UALTx  # adjusted logit upper 95% confidence limit
    # ciul <- 0.1528,
    # cill <- 0.0358
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
      textInput(inputId = "DxRuleInDecision", label = "Rule-in decision", value = "refer to HF clinic"),
      numericInput("RuleInDecisionThreshold", "Rule-in PPV threshold", min=0, max=1, value= 0.5, step = 0.01),
      textInput(inputId = "IndeterminateDecision", label = "Indeterminate decision", value = "investigate further"),
      textInput(inputId = "DxRuleOutDecision", label = "Rule-out decision", value = "assess for other causes"),
      numericInput("RuleOutDecisionThreshold", "Rule-out NPV threshold", min=0, max=1, value= 0.025, step = 0.01)
    ),
    mainPanel(
      plotOutput("RuleInOutPlot"),
      tags$br(),
    verbatimTextOutput("linesTable"),
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
    prevalence <- eventReactive(input$GoButton, {input$prevalence})
    n <- eventReactive(input$GoButton, {input$n})
    sensitivity <- eventReactive(input$GoButton, {input$sensitivity})
    specificity <- eventReactive(input$GoButton, {input$specificity})
    RuleInDecisionThreshold <- eventReactive(input$GoButton, {input$RuleInDecisionThreshold})
    IndeterminateDecision <- eventReactive(input$GoButton, {input$IndeterminateDecision})
    RuleOutDecisionThreshold <- eventReactive(input$GoButton, {input$RuleOutDecisionThreshold})

    Dpos <- eventReactive(input$GoButton, {(input$n * input$prevalence)})
    Dneg <- eventReactive(input$GoButton, {(input$n - Dpos())})
    
    Tp <- eventReactive(input$GoButton, {(input$sensitivity * Dpos())})
    Tn <- eventReactive(input$GoButton, {(input$specificity * Dneg())})
    
    Fn <- eventReactive(input$GoButton, {(Dneg() - Tn())})
    Fp <- eventReactive(input$GoButton, {(Dpos() - Tp())})
    
    PPV <- eventReactive(input$GoButton, {Tp()/(Tp() + Fp())})
    NPV <- eventReactive(input$GoButton, {Tn()/(Tn() + Fn())})

    LRp <- eventReactive(input$GoButton, {(input$sensitivity/(1 - input$specificity))})
    LRn <- eventReactive(input$GoButton, {((1 - input$sensitivity)/(input$specificity))})
    
    PreTestOddsP <- eventReactive(input$GoButton, {((input$prevalence)/(1 - input$prevalence))})
    PreTestOddsN <- eventReactive(input$GoButton, {((1 - input$prevalence)/(input$prevalence))})
    
    PostTestOddsP <- eventReactive(input$GoButton, {PreTestOddsP()*LRp()})
    PostTestOddsN <- eventReactive(input$GoButton, {PreTestOddsN()*LRn()})
    
    PostTestProbP <- eventReactive(input$GoButton, {PostTestOddsP()/(PostTestOddsP() + 1)})
    PostTestProbN <- eventReactive(input$GoButton, {PostTestOddsN()/(PostTestOddsN() + 1)})
    
    
            
    ### coordinates and labels for contingency matrix graphic
    fixedlabels <- reactive({
      return({data.frame(
        x = c(0.35, 0.9, 0.35, 0.25),
        y = c(
          RuleInDecisionThreshold() + 0.05, 
          prevalence() + 0.05, 
          RuleOutDecisionThreshold() + 0.05,
          (RuleInDecisionThreshold() + RuleOutDecisionThreshold())/ 2 
          ),
        labels = c(
          paste0(RuleInDecisionThreshold()*100, "%  = threshold for rule-in decision: ", DxRuleInDecision()),
          paste0("Prevalence = ", prevalence()*100, "%"),
          paste0(RuleOutDecisionThreshold()*100, "%  = threshold for rule-out decision: ", DxRuleOutDecision()),
          
          paste0("Action when indeterminate: ", IndeterminateDecision())),
        fillColours = c("firebrick4", "springgreen4")
       ) })
    })

    postTestLabels <- reactive({
      return({data.frame(
        x = c(0.9, 0.9),
        y = c(PostTestProbP() + 0.045, PostTestProbN()) - 0.035,
        labels = c(
          strwrap(paste0("Prob post +ve test = ", round(PostTestProbP()*100), "%"), 40),
          strwrap(paste0("Prob post -ve test = ", round(PostTestProbN()*100), "%"), 40)
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
          RuleInDecisionThresholdY <- c(RuleInDecisionThreshold(), RuleInDecisionThreshold()),

          RuleOutDecisionThresholdX <- c(0, 1),
          RuleOutDecisionThresholdY <- c(RuleOutDecisionThreshold(), RuleOutDecisionThreshold()),
          
          TestPosX <- c(0, 1),
          TestPosY <- c(prevalence(), PostTestProbP()),
          
          TestNegX <- c(0, 1),
          TestNegY <- c(prevalence(), PostTestProbN()),

          # TPY_cil <- c(prevalence(), ciALTx(TestPosY(), Dpos(), 0.05, 2)$LALTx),
          # TPY_ciu <- c(prevalence(), ciALTx(TestPosY(), Dpos(), 0.05, 2)$UALTx),
          # 
          # TNY_cil <- c(prevalence(), ciALTx(TestNegY(), Dneg(), 0.05, 2)$LALTx),
          # TNY_ciu <- c(prevalence(), ciALTx(TestNegY(), Dneg(), 0.05, 2)$UALTx)
                    
          # TNY_cil <- c(prevalence(), ciproportion(TestNegY(),Dneg())$cill),
          # TNY_ciu <- c(prevalence(), ciproportion(TestNegY(),Dneg())$ciul)

          TPY_cil <- c(prevalence(), PostTestProbP() * exactci(Tp(), Dpos(), 0.05)$conf.int[[1]]),
          TPY_ciu <- c(prevalence(), PostTestProbP() * exactci(Tp(), Dpos(), 0.05)$conf.int[[2]]),
          
          TNY_cil <- c(prevalence(), PostTestProbN() * exactci(Tn(), Dneg(), 0.05)$conf.int[[1]]),
          TNY_ciu <- c(prevalence(), PostTestProbN() * exactci(Tn(), Dneg(), 0.05)$conf.int[[2]])
          
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
      geom_line(aes(x = linesDf()$TestPosX, y = linesDf()$TestPosY), size = 1.15, data = linesDf(), stat = "identity", position = "identity", colour = "firebrick4") +
      # geom_ribbon(data = linesDf(), aes(x = linesDf()$TestPosX, ymin = linesDf()$TPY_cil, ymax = linesDf()$TPY_ciu, alpha = 0.03), fill  = "lightsalmon") +
      # geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      # geom_ribbon(data = linesDf(), aes(x = linesDf()$TestNegX, ymin = linesDf()$TNY_cil, ymax = linesDf()$TNY_ciu, alpha = 0.03), fill  = "darkseagreen3") +
      geom_ribbon(data = linesDf(), aes(x = linesDf()$TestPosX, ymin = linesDf()$TPY_cil, ymax = linesDf()$TPY_ciu, alpha = 0.03), fill  = "lightsalmon") +
      geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      geom_ribbon(data = linesDf(), aes(x = linesDf()$TestNegX, ymin = linesDf()$TNY_cil, ymax = linesDf()$TNY_ciu, alpha = 0.03), fill  = "darkseagreen3") +
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
