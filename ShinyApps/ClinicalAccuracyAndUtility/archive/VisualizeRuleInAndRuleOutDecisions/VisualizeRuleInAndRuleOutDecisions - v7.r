  ###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False positives, False negatives, True negatives, and confidence intervals.

library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)
library(proportion)
library(rsconnect)

###   > setwd("C:/Users/Michael Power/OneDrive/DEC/R tools/Teaching/VisualiseRuleInAndRuleOutDecisions")


#################### confidence interval on a proportion ###############################
#
### this is a wrapper to allow the choice of CI method to be easily changed
#
# x	 - Number of successes
# n	- Number of trials
# alpha - Alpha value (significance level required)
# h	- Adding factor

ciprop <- function(x, n, alpha = 0.05)
{
  # n <- max(n, 2) ### in for debugging
  # 
  # x <- x %>%    ### in for debugging
  #   max(1) %>%
  #   min(n)      # x has to be an integer between 0 and n
  # 
  # n <- ifelse(n <= x, x+10, n)  ### in for debugging
  
  h <- 2 # count required by ciALTx
  return({
    data.frame(
      ciL = proportion::ciALTx(x, n, alpha, h)$LALTx,
      ciU = proportion::ciALTx(x, n, alpha, h)$UALTx
    )
  })
}

DxStats <- function(n, prevalence, sensitivity, specificity) {
  prevalence <- min(prevalence,0.9999)
  prevalence <- max(prevalence,0.0001)

  Dpos <- n * prevalence
  Dneg <- n - Dpos
  
  Tp <- sensitivity * Dpos
  Tn <- specificity * Dneg
  
  Fn <- (1 - sensitivity) * Dpos
  Fp <- (1 - specificity) * Dneg
  
  PPV <- Tp/(Tp + Fp)
  NPV <- Tn/(Tn + Fn)
  
  LRp <- sensitivity/(1 - specificity)
  LRn <- (1 -sensitivity)/(specificity)
  
  PreTestOddsP <- prevalence/(1 - prevalence)
  PreTestOddsN <- (prevalence)/(1 -prevalence)
  
  PostTestOddsP <- PreTestOddsP*LRp
  PostTestOddsN <- PreTestOddsN*LRn
  
  PostTestProbP <- PostTestOddsP/(PostTestOddsP + 1)
  PostTestProbN <- PostTestOddsN/(PostTestOddsN + 1)
  
  TPY_ciL <- ciprop(PostTestProbP * n, n)$ciL
  TPY_ciU <- ciprop(PostTestProbP * n, n)$ciU
  TNY_ciL <- ciprop(PostTestProbN * n, n)$ciL
  TNY_ciU <- ciprop(PostTestProbN * n, n)$ciU
    data.frame(
      Dpos = Dpos,
      Dneg = Dneg,
      
      Tp = Tp,
      Tn = Tn,
      
      Fn = Fn,
      Fp = Fp,
      
      PPV = PPV,
      NPV = NPV,
      
      LRp = LRp,
      LRn = LRn,
      
      PreTestOddsP = PreTestOddsP,
      PreTestOddsN = PreTestOddsN,
      
      PostTestOddsP = PostTestOddsP,
      PostTestOddsN = PostTestOddsP,
      
      PostTestProbP = PostTestProbP,
      PostTestProbN = PostTestProbN,
      
      TPY_ciL = TPY_ciL,
      TPY_ciU = TPY_ciU,
      TNY_ciL = TNY_ciL,
      TNY_ciU = TNY_ciU
    )
}



#################### ui ###############################

ui<-fluidPage(

  titlePanel(h4("Explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals")),

  sidebarLayout(
    sidebarPanel(
      actionButton("GoButton", "Update the graph"),
      tags$br(),
      tags$h3("Input Variables"),
      textInput(inputId = "DxCondition", label = "Condition", value = "Example disease"),
      textInput(inputId = "DxTestName", label = "Name of test", value = "Example test"),
      numericInput("prevalence", "prior probability (prevalence)", min=0, max=1, value=0.4, step = 0.01),
      numericInput("n", "population", min=1, max=1000, value=300),
      numericInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90, step = 0.01),
      numericInput("specificity", "specificity of index test", min=0, max=1, value= 0.80, step = 0.01),
      textInput(inputId = "DxRuleInDecision", label = "Rule-in decision", value = "Example: start treatment"),
      numericInput("RuleInDecisionThreshold", "Rule-in PPV threshold", min=0, max=1, value= 0.5, step = 0.01),
      textInput(inputId = "IndeterminateDecision", label = "Indeterminate decision", value = "Example: investigate further"),
      textInput(inputId = "DxRuleOutDecision", label = "Rule-out decision", value = "Example: rule out the condition"),
      numericInput("RuleOutDecisionThreshold", "Rule-out NPV threshold", min=0, max=1, value= 0.1, step = 0.01)
    ),
    mainPanel(
      plotOutput("RuleInOutPlot"),
      tags$small("Decision threshold = (net harm)/(net harm + net benefit)"),
      tags$br(),
      tags$small("    Net harm = utility(no Rx | no disease) - utility(treatment | no disease)"),
      tags$br(),
      tags$small("    Net benefit = utility(Rx | disease) - utility(no treatment |  disease)"),
      tags$br(),
      tags$br(),
      plotOutput("PrePostProb"),
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
    
    TPY_ciL <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TPY_ciL})
    TPY_ciU <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TPY_ciU})
    TNY_ciL <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TNY_ciL})
    TNY_ciU <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TNY_ciU})
  
    # lines for plot 1
    linesDf <- eventReactive(input$GoButton, {
      ### save stats so don't have to call many times
      Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity) 
      
      data.frame(
        PriorAxisX <- c(0, 0),
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
        TestPosY <- c(input$prevalence, Dx$PostTestProbP),
        
        TestNegX <- c(0, 1),
        TestNegY <- c(input$prevalence, Dx$PostTestProbN),
        
        TPY_ciL <- c(input$prevalence, Dx$TPY_ciL),
        TPY_ciU <- c(input$prevalence, Dx$TPY_ciU),
        
        TNY_ciL <- c(input$prevalence, Dx$TNY_ciL),
        TNY_ciU <- c(input$prevalence, Dx$TNY_ciU)
      )
    })
    
### coordinates and labels for plot 1
    fixedlabels <- eventReactive(input$GoButton, {
      data.frame(
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
       )
    })

    postTestLabels <- eventReactive(input$GoButton, {
      ### save stats so don't have to call many times
      Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)
      Nudge <- 0.02
      if (Dx$TPY_ciU - Dx$TPY_ciL < 3*Nudge) NudgeCIp = +Nudge else NudgeCIp = -Nudge/3
      if (Dx$TNY_ciU - Dx$TNY_ciL < 3*Nudge) NudgeCIn = +Nudge else NudgeCIn = -Nudge/3
      data.frame(
        x = c(0.85, 0.85, 1.05, 1.05, 1.05, 1.05),
        y = c(
          PostTestProbP() + 0.025, 
          PostTestProbN() - 0.025,
          Dx$TPY_ciL - NudgeCIp,
          Dx$TPY_ciU + NudgeCIp,
          Dx$TNY_ciL - NudgeCIn,
          Dx$TNY_ciU + NudgeCIp),
        
        labels = c(
          strwrap(paste0("Prob post +ve test = ", round(PostTestProbP()*100), "%"), 40),
          strwrap(paste0("Prob post -ve test = ", round(PostTestProbN()*100), "%"), 40),
          paste0(round(TPY_ciL() * 100), "%"),
          paste0(round(TPY_ciU() * 100), "%"),
          paste0(round(TNY_ciL() * 10000)/100, "%"),
          paste0(round(TNY_ciU() * 10000)/100, "%")
          # "1", "2", "3", "4"
        )
      )
    
    })
    
#==========================================================
    
    
    output$linesTable <- renderPrint(linesDf())
    
    output$RuleInOutPlot<-renderPlot({
      
      ggplot(linesDf()) +
        geom_line(aes(x = linesDf()$PriorAxisX, y = linesDf()$PriorAxisY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$PostAxisX, y = linesDf()$PostAxisY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$PrevX, y = linesDf()$PrevY, colour="coral1"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$RuleInDecisionThresholdX, y = linesDf()$RuleInDecisionThresholdY, colour="cadetblue"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$RuleOutDecisionThresholdX, y = linesDf()$RuleOutDecisionThresholdY, colour="springgreen4"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$TestPosX, y = linesDf()$TestPosY), size = 1.15, data = linesDf(), stat = "identity", position = "identity", colour = "firebrick4") +
        geom_ribbon(data = linesDf(), aes(x = linesDf()$TestPosX, ymin = linesDf()$TPY_ciL, ymax = linesDf()$TPY_ciU, alpha = 0.03), fill  = "lightsalmon") +
        geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
        geom_ribbon(data = linesDf(), aes(x = linesDf()$TestNegX, ymin = linesDf()$TNY_ciL, ymax = linesDf()$TNY_ciU, alpha = 0.03), fill  = "darkseagreen3") +
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

    graphPre2PostProb <- eventReactive(input$GoButton, {
      x <- seq(from = 0, to = 1, by = 0.02) ### preTest probability along the x-axis
      ### y-axis for post test probability
      ## initialize variables for post test probabilities
      
      yPciL <- x
      yP <- x
      yPciU <- x
      
      yNciL <- x
      yN <- x
      yNciU <- x
      
      for (i in seq_along(x)) {
        Dx <- DxStats(input$n, x[[i]], input$sensitivity, input$specificity) 
      
        yPciL[[i]] <- Dx$TPY_ciL
        yP[[i]]    <- Dx$PostTestProbP
        yPciU[[i]] <- Dx$TPY_ciU
        
        yNciL[[i]] <- Dx$TNY_ciL
        yN[[i]]    <- Dx$PostTestProbN
        yNciU[[i]] <- Dx$TNY_ciU
      }
      
      data.frame(
        x = x,
        yPciL = yPciL,
        yP = yP,
        yPciU = yPciU,
        
        yNciL = yNciL,
        yN = yN,
        yNciU = yNciU
      )
    })

  linesPre2PostProb <- eventReactive(input$GoButton, {
    Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)
    data.frame(
      prevalenceX     = c(input$prevalence, input$prevalence),
      prevalenceY     = c(0, Dx$PostTestProbP),
      
      PostProbPosX    = c(0, input$prevalence),
      PostProbPosYciL = c(Dx$TPY_ciL, Dx$TPY_ciL),
      PostProbPosY    = c(Dx$PostTestProbP, Dx$PostTestProbP),
      PostProbPosYciU = c(Dx$TPY_ciU, Dx$TPY_ciU),

      PostProbNegX    = c(0, input$prevalence),
      PostProbNegYciL = c(Dx$TNY_ciL, Dx$TNY_ciL),
      PostProbNegY    = c(Dx$PostTestProbN, Dx$PostTestProbN),
      PostProbNegYciU = c(Dx$TNY_ciU, Dx$TNY_ciU)
    )
  })

  prepostLabels <- eventReactive(input$GoButton, {
    ### save stats so don't have to call many times
    Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)
    data.frame(
      x = c(0.1, 0.1, input$prevalence),
      y = c(
        PostTestProbP() + 0.035, 
        PostTestProbN() - 0.035,
        0.10),
      
      labels = c(
        strwrap(paste0("Prob post +ve test = ", round(PostTestProbP()*100), "%"), 40),
        strwrap(paste0("Prob post -ve test = ", round(PostTestProbN()*100), "%"), 40),
        paste0("Prevalence = ", round(input$prevalence * 100), "%")
      )
    )
    
  })
  
  
  
  
  output$PrePostProb<-renderPlot({
    ggplot(graphPre2PostProb()) +
      geom_line(aes(x = graphPre2PostProb()$x, y = graphPre2PostProb()$yP), stat = "identity", position = "identity") +
      geom_ribbon(data = graphPre2PostProb(),
                  aes(x = graphPre2PostProb()$x,
                  ymin = graphPre2PostProb()$yPciL,
                  ymax = graphPre2PostProb()$yPciU,
                  alpha = 0.03), fill  = "lightsalmon") +

      geom_line(aes(x = graphPre2PostProb()$x, y = graphPre2PostProb()$yN), data = graphPre2PostProb(), stat = "identity", position = "identity") +
      geom_ribbon(data = graphPre2PostProb(),
                  aes(x = graphPre2PostProb()$x,
                  ymin = graphPre2PostProb()$yNciL,
                  ymax = graphPre2PostProb()$yNciU,
                  alpha = 0.03), fill  = "darkseagreen3") +
      theme(legend.position="none") +
      labs(x = "Pre-test probability (prevalence)", y = paste0("Post test probability after ", DxTestName())) +
      ggtitle(paste("Pre- and post-test probabilities after", DxTestName(), "for", DxCondition())) +
      theme(plot.title = element_text(size = 12, face = "bold")) +


      geom_line(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$prevalenceX,
        y = linesPre2PostProb()$prevalenceY), stat = "identity", position = "identity") +

      geom_line(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$PostProbPosX,
        y = linesPre2PostProb()$PostProbPosY), stat = "identity", position = "identity") +
      geom_ribbon(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$PostProbPosX,
        ymin = linesPre2PostProb()$PostProbPosYciL,
        ymax = linesPre2PostProb()$PostProbPosYciU, alpha = 0.03), fill  = "lightsalmon") +

      geom_line(data = linesPre2PostProb(), aes(x = linesPre2PostProb()$PostProbNegX, y = linesPre2PostProb()$PostProbNegY), stat = "identity", position = "identity") +
      geom_ribbon(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$PostProbNegX,
        ymin = linesPre2PostProb()$PostProbNegYciL,
        ymax = linesPre2PostProb()$PostProbNegYciU, alpha = 0.03), fill  = "darkseagreen3") +
      

    geom_text(data = prepostLabels(), size = 4, aes(x,y,label = labels)) 
      
  })
  }

shinyApp(ui=ui, server=server)
