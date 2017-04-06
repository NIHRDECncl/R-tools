###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False positives, False negatives, True negatives, and confidence intervals.

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(proportion)
library(rsconnect)

#################### ui ###############################

ui<-fluidPage(

  titlePanel(h4("Explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals")),

  sidebarLayout(
    sidebarPanel(
      actionButton("GoButton", "Click to update the graph"),
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
    #   tags$br(),
    # verbatimTextOutput("linesTable"),
      tags$br(),
      tags$b("Cite as:"),
      tags$br(),
      "Michael Power, Joy Allen.",
      tags$br(),
      tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals"),
      tags$br(),
      "NIHR Diagnostic Evidence Co-operative Newcastle. September 2016",
      tags$br()
      # verbatimTextOutput("lines")
    )
  )
)

#######################################################

#################    server     ################

# inputs
#
# DxCondition
# DxTestName
# prevalence
# n
# sensitivity
# specificity
# DxRuleInDecision
# RuleInDecisionThreshold
# IndeterminateDecision
# DxRuleOutDecision
# RuleOutDecisionThreshold

server<-function(input, output) {

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

    Dpos <- eventReactive(input$GoButton, {round(input$n * input$prevalence)})
    Dneg <- eventReactive(input$GoButton, {round(input$n - Dpos())})
    
    Tp <- eventReactive(input$GoButton, {input$sensitivity * Dpos()})
    Tn <- eventReactive(input$GoButton, {input$specificity * Dneg()})
    
    Fn <- eventReactive(input$GoButton, {(1 - input$sensitivity) * Dpos()})
    Fp <- eventReactive(input$GoButton, {(1 - input$specificity) * Dneg()})
    
    PPV <- eventReactive(input$GoButton, {Tp()/(Tp() + Fp())})
    NPV <- eventReactive(input$GoButton, {Tn()/(Tn() + Fn())})
    
    LRp <- eventReactive(input$GoButton, {(input$sensitivity/(1 - input$specificity))})
    LRn <- eventReactive(input$GoButton, {(1 - input$sensitivity)/(input$specificity)})
    
    PreTestOddsP <- eventReactive(input$GoButton, {(input$prevalence)/(1 - input$prevalence)})
    PreTestOddsN <- eventReactive(input$GoButton, {(1 - input$prevalence)/(input$prevalence)})
    
    PostTestOddsP <- eventReactive(input$GoButton, {PreTestOddsP()*LRp()})
    PostTestOddsN <- eventReactive(input$GoButton, {PreTestOddsN()*LRn()})
    
    PostTestProbP <- eventReactive(input$GoButton, {PostTestOddsP()/(PostTestOddsP() + 1)})
    PostTestProbN <- eventReactive(input$GoButton, {PostTestOddsN()/(PostTestOddsN() + 1)})
    
    ### confidence  limits for the post=test probabilities
    
    ### the following are numbers; the same names in the data.frame label 2 item columns (vectors)
    
    TPY_cil <- eventReactive(input$GoButton, {ciALTx(PostTestProbP()*input$n, input$n, 0.05, 2)$LALTx})
    TPY_ciu <- eventReactive(input$GoButton, {ciALTx(PostTestProbP()*input$n, input$n, 0.05, 2)$UALTx})
    TNY_cil <- eventReactive(input$GoButton, {ciALTx(PostTestProbN()*input$n, input$n, 0.05, 2)$LALTx})
    TNY_ciu <- eventReactive(input$GoButton, {ciALTx(PostTestProbN()*input$n, input$n, 0.05, 2)$UALTx})
    

    ### coordinates and labels for contingency matrix graphic
    fixedlabels <- eventReactive(input$GoButton, {
      return({data.frame(
        x = c(0.35, 0.85, 0.35, 0.35),
        y = c(
          RuleInDecisionThreshold() + 0.05, 
          prevalence() + 0.05, 
          RuleOutDecisionThreshold() + 0.05,
          RuleInDecisionThreshold() - 0.05
        ),
        labels = c(
          paste0(RuleInDecisionThreshold()*100, "%  = threshold for rule-in decision: ", DxRuleInDecision()),
          paste0("Prevalence = ", prevalence()*100, "%"),
          paste0(RuleOutDecisionThreshold()*100, "%  = threshold for rule-out decision: ", DxRuleOutDecision()),
          paste0("Action when indeterminate: ", IndeterminateDecision())),
          fillColours = c("firebrick4", "springgreen4")
       ) })
    })

    postTestLabels <- eventReactive(input$GoButton, {
      return({data.frame(
        x = c(0.85, 0.85, 1.05, 1.05, 1.05, 1.05),
        y = c(
          PostTestProbP() + 0.045, 
          PostTestProbN() - 0.035,
          TPY_cil() + 0.01,
          TPY_ciu() - 0.01,
          TNY_cil() + 0.01,
          TNY_ciu() - 0.01),
        
        labels = c(
          strwrap(paste0("Prob post +ve test = ", round(PostTestProbP()*100), "%"), 40),
          strwrap(paste0("Prob post -ve test = ", round(PostTestProbN()*100), "%"), 40),
          paste0(round(TPY_cil() * 100), "%"),
          paste0(round(TPY_ciu() * 100), "%"),
          paste0(round(TNY_cil() * 100), "%"),
          paste0(round(TNY_ciu() * 100), "%")
          # "1", "2", "3", "4"
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
          TestPosY <- c(input$prevalence, PostTestProbP()),
          
          TestNegX <- c(0, 1),
          TestNegY <- c(input$prevalence, PostTestProbN()),

          TPY_cil <- c(input$prevalence, ciALTx(PostTestProbP()*input$n, input$n, 0.05, 2)$LALTx),
          TPY_ciu <- c(input$prevalence, ciALTx(PostTestProbP()*input$n, input$n, 0.05, 2)$UALTx),

          TNY_cil <- c(input$prevalence, ciALTx(PostTestProbN()*input$n, input$n, 0.05, 2)$LALTx),
          TNY_ciu <- c(input$prevalence, ciALTx(PostTestProbN()*input$n, input$n, 0.05, 2)$UALTx)
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
      geom_ribbon(data = linesDf(), aes(x = linesDf()$TestPosX, ymin = linesDf()$TPY_cil, ymax = linesDf()$TPY_ciu, alpha = 0.03), fill  = "lightsalmon") +
      geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
      geom_ribbon(data = linesDf(), aes(x = linesDf()$TestNegX, ymin = linesDf()$TNY_cil, ymax = linesDf()$TNY_ciu, alpha = 0.03), fill  = "darkseagreen3") +
      theme(
        axis.text.x = element_blank(),
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
