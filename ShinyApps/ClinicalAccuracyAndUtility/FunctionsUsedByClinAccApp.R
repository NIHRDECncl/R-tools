  ##########################################################
  # FunctionsUsedByClinicalAccuracyApp.R()
  #
  # non-reactive functions used by the Shiny App to explore clinical accuracy measurements
  #
  # installation of colour picker from http://deanattali.com/blog/plot-colour-helper/
  #
  #install.packages("devtools")
  #devtools::install_github("daattali/colourpicker")
  #
  ##########################################################
  #
  # load packages used by the App 
  LoadPackages <- function() {
    library(shiny)
    library(tidyverse) # Imports: broom, DBI, dplyr, forcats, ggplot2, haven, httr, hms, jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl, stringr, tibble, rvest, tidyr, xml2
    library(rsconnect)   # needed to upload to Shinyio
    library(readr)       # needed for GET()
    library(vcd)         # mosaic() plot http://www.statmethods.net/advgraphs/mosaic.html
    library(colourpicker) # http://deanattali.com/blog/plot-colour-helper/ 
    library(shinythemes)
    library(DT)
    library(knitr)
    library(rmarkdown)
    library(shinycssloaders)
    #      library(proportion)  package no longer being maintained :-(
    library(PropCIs)
    library(rsconnect)   # needed to upload to Shinyio
    # ...
  }
  
  
  
  #################### confidence interval on a proportion ###############################
  #
  ### this is a wrapper to allow the choice of CI method to be easily changed
  #
  # use PropCIs::scoreci() to calculate Wilson's confidence interval for a single proportion. 
  #             Score CI based on inverting the asymptotic normal test using the null standard error
  # Arguments: 
  # x	           Number of successes
  # n            Total sample size
  # conf.level   Confidence level
  
  ciprop <- function(x, n, alpha = 0.05)
  {
    
    conf.level <- 1 - alpha
    
    return({
      data.frame(
        ciL = PropCIs::scoreci(x, n, conf.level)$conf.int[1],
        ciU = PropCIs::scoreci(x, n, conf.level)$conf.int[2]
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
    
    PostTestProbP <- PostTestOddsP/(PostTestOddsP + 1) # = PPV
    PostTestProbN <- PostTestOddsN/(PostTestOddsN + 1) # = (1 - NPV)
    
    cidf <- ciprop(PPV * n, n) # CI for post-positive test probability
    TPY_ciL <- cidf$ciL
    TPY_ciU <- cidf$ciU
    
    cidf <- ciprop(PostTestProbN * n, n) # CI for post-negative test probability
    TNY_ciL <- cidf$ciL
    TNY_ciU <- cidf$ciU
    
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
  
  
 
  
  fixedlabels <- function(RuleInDecisionThreshold, prevalence, RuleOutDecisionThreshold, 
                          DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision)  {
    data.frame(
      x = c(0.35, 0.85, 0.35, 0.35),
      y = c(
        RuleInDecisionThreshold + 0.05, 
        prevalence + 0.05, 
        RuleOutDecisionThreshold + 0.05,
        RuleInDecisionThreshold - 0.05
      ),
      labels = c(
        paste0(RuleInDecisionThreshold*100, "%  = threshold for rule-in decision: ", DxRuleInDecision),
        paste0("Prevalence = ", prevalence*100, "%"),
        paste0(RuleOutDecisionThreshold*100, "%  = threshold for rule-out decision: ", DxRuleOutDecision),
        paste0("Action when indeterminate: ", IndeterminateDecision)),
      fillColours = c("firebrick4", "springgreen4")
    )

  }
  
  
  postTestLabels <- function(n, prevalence, sensitivity, specificity){
    ### save stats so don't have to call many times
    
    Dx <- DxStats(n, prevalence, sensitivity, specificity) 
    
    Nudge <- 0.02
    if (Dx$TPY_ciU - Dx$TPY_ciL < 3*Nudge) NudgeCIp = + Nudge else NudgeCIp = - Nudge/3
    if (Dx$TNY_ciU - Dx$TNY_ciL < 3*Nudge) NudgeCIn = + Nudge else NudgeCIn = - Nudge/3
    
   data.frame(
      x = c(0.85, 0.85, 1.05, 1.05, 1.05, 1.05),
      y = c(
        Dx$PostTestProbP + 0.025, # = post +ve test probability 
        Dx$PostTestProbN - 0.025, # = post -ve test probability
        Dx$TPY_ciL - NudgeCIp,
        Dx$TPY_ciU + NudgeCIp,
        Dx$TNY_ciL - NudgeCIn,
        Dx$TNY_ciU + NudgeCIp),
      
      labels = c(
        strwrap(paste0("Prob post +ve test = ", round(Dx$PostTestProbP*100), "%"), 40),
        strwrap(paste0("Prob post -ve test = ", round(Dx$PostTestProbN*100), "%"), 40),
        paste0(round(Dx$TPY_ciL * 100), "%"),
        paste0(round(Dx$TPY_ciU * 100), "%"),
        paste0(round(Dx$TNY_ciL * 10000)/100, "%"),
        paste0(round(Dx$TNY_ciU * 10000)/100, "%")
         )
    )
  }
  
  
  ruleinoutplot <- function(n, prevalence, sensitivity, specificity, RuleInDecisionThreshold, RuleOutDecisionThreshold, #}, 
                            DxCondition,  DxTestName, DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision){
    
    Dx <- DxStats(n, prevalence, sensitivity, specificity) 
   
    linesdf <- data.frame(
      PriorAxisX = c(0, 0),
      PriorAxisY = c(0, 1),
      
      PostAxisX = c(1, 1),
      PostAxisY = c(0, 1),
      
      PrevX = c(0, 1),
      PrevY = c(prevalence, prevalence),
      
      RuleInDecisionThresholdX = c(0, 1),
      RuleInDecisionThresholdY = c(RuleInDecisionThreshold, RuleInDecisionThreshold),
      
      RuleOutDecisionThresholdX = c(0, 1),
      RuleOutDecisionThresholdY = c(RuleOutDecisionThreshold, RuleOutDecisionThreshold),
      
      TestPosX = c(0, 1),
      TestPosY = c(prevalence, Dx$PostTestProbP),
      
      TestNegX = c(0, 1),
      TestNegY = c(prevalence, round(Dx$PostTestProbN,3)), # post -ve test probability
      
      TPY_ciL = c(prevalence, round(Dx$TPY_ciL,3)),
      TPY_ciU = c(prevalence, round(Dx$TPY_ciU,3)),
      
      TNY_ciL = c(prevalence, round(Dx$TNY_ciL,3)),
      TNY_ciU = c(prevalence, round(Dx$TNY_ciU,3))
    )
    
    fixedlabels <- fixedlabels(RuleInDecisionThreshold, prevalence, RuleOutDecisionThreshold, 
                    DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision)

    postTestLabels <- postTestLabels(n, prevalence, sensitivity, specificity)
    
    
    ggplot(linesdf) +
      geom_line(aes(x = PriorAxisX, y = PriorAxisY), data =linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = PostAxisX, y = PostAxisY), data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = PrevX, y = PrevY, colour="coral1"), size = 1.5, data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = RuleInDecisionThresholdX, y = RuleInDecisionThresholdY, colour="cadetblue"), size = 1.5, data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = RuleOutDecisionThresholdX, y = RuleOutDecisionThresholdY, colour="springgreen4"), size = 1.5, data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = TestPosX, y = TestPosY), size = 1.15, data = linesdf, stat = "identity", position = "identity", colour = "firebrick4") +
      geom_ribbon(data = linesdf, aes(x = TestPosX, ymin = TPY_ciL, ymax = TPY_ciU, alpha = 0.03), fill  = "lightsalmon") +
      geom_line(aes(x = TestNegX, y = TestNegY), size = 1.15, data = linesdf, stat = "identity", position = "identity") +
      geom_ribbon(data = linesdf, aes(x = TestNegX, ymin = TNY_ciL, ymax = TNY_ciU, alpha = 0.03), fill  = "darkseagreen3") +
      theme(
       axis.text.x = element_blank(),
        legend.position="none") +
       labs(x = "", y = paste0("probability of ", DxCondition), size = 8) +
       ggtitle(paste("Post-test probabilities after", DxTestName, "for", DxCondition)) +
       theme(plot.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 5), 
             axis.title = element_text(size = 14)) +
     geom_text(size = 3, data = fixedlabels, aes(x,y,label = labels)) + 
      geom_text(data = postTestLabels, aes(x,y, label = labels), size = 4)
  

  }
  
  
  graphPre2PostProb <- function(n, prevalence, sensitivity, specificity) {
    x <- seq(from = 0, to = 1, by = 0.01) ### preTest probability along the x-axis
    ### y-axis for post test probability
    ## initialize variables for post test probabilities
    
    yPciL <- x
    yP <- x
    yPciU <- x
    
    yNciL <- x
    yN <- x
    yNciU <- x
    
    for (i in seq_along(x)) {
      Dx <- DxStats(n, x[[i]], sensitivity, specificity) 
      
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
  }
  
  linesPre2PostProb <- function(n, prevalence, sensitivity, specificity) {
    Dx <- DxStats(n, prevalence, sensitivity, specificity)
    data.frame(
      prevalenceX     = c(prevalence, prevalence),
      prevalenceY     = c(0, Dx$PostTestProbP),
      
      PostProbPosX    = c(0, prevalence),
      PostProbPosYciL = c(Dx$TPY_ciL, Dx$TPY_ciL),
      PostProbPosY    = c(Dx$PostTestProbP, Dx$PostTestProbP),
      PostProbPosYciU = c(Dx$TPY_ciU, Dx$TPY_ciU),
      
      PostProbNegX    = c(0, prevalence),
      PostProbNegYciL = c(Dx$TNY_ciL, Dx$TNY_ciL),
      PostProbNegY    = c(Dx$PostTestProbN, Dx$PostTestProbN),
      PostProbNegYciU = c(Dx$TNY_ciU, Dx$TNY_ciU)
    )
  }
  
  prepostLabels <- function(n, prevalence, sensitivity, specificity) {
    ### save stats so don't have to call many times
    Dx <- DxStats(n, prevalence, sensitivity, specificity)
    data.frame(
      x = c(0.1, 0.1, prevalence),
      y = c(
        Dx$PostTestProbP + 0.035, 
        Dx$PostTestProbN - 0.035,
        0.10),
      
      labels = c(
        strwrap(paste0("Prob post +ve test = ", round(Dx$PostTestProbP*100), "%"), 40),
        strwrap(paste0("Prob post -ve test = ", round(Dx$PostTestProbN*100), "%"), 40),
        paste0("Prevalence = ", round(prevalence * 100), "%")
      )
    )
    
  }
  
  
  
  
 prepostprobplot <- function(n, prevalence, sensitivity, specificity, DxCondition, DxTestName){
   
   graphPre2PostProb <-  graphPre2PostProb(n, prevalence, sensitivity, specificity)
   linesPre2PostProb <- linesPre2PostProb(n, prevalence, sensitivity, specificity)
   prepostLabels <- prepostLabels(n, prevalence, sensitivity, specificity)
   
  ggplot(graphPre2PostProb) +
    geom_line(data = graphPre2PostProb, aes(x = x, y = yP), stat = "identity", position = "identity") +
    geom_ribbon(data = graphPre2PostProb, aes(x = x,ymin = yPciL, ymax = yPciU, alpha = 0.03), fill  = "lightsalmon") +
    geom_line(aes(x = x, y = yN), data = graphPre2PostProb, stat = "identity", position = "identity") +
    geom_ribbon(data = graphPre2PostProb,  aes(x = x, ymin = yNciL, ymax = yNciU, alpha = 0.03), fill  = "darkseagreen3") +
    theme(legend.position="none") +
    labs(x = "Pre-test probability (prevalence)", y = paste0("Post test probability after ", DxTestName)) +
    ggtitle(paste("Pre- and post-test probabilities after", DxTestName, "for", DxCondition)) +
    theme(plot.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14)) +
    
    geom_line(data = linesPre2PostProb, aes(x = prevalenceX,
      y = prevalenceY), stat = "identity", position = "identity") +
    geom_line(data = linesPre2PostProb, aes(x = PostProbPosX,
      y = PostProbPosY), stat = "identity", position = "identity") +
    geom_ribbon(data = linesPre2PostProb, aes(x = PostProbPosX,
      ymin = PostProbPosYciL,  ymax = PostProbPosYciU, alpha = 0.03), fill  = "lightsalmon") +
    geom_line(data = linesPre2PostProb, aes(x = PostProbNegX, y = PostProbNegY), stat = "identity", position = "identity") +
    geom_ribbon(data = linesPre2PostProb, aes(x = PostProbNegX,
      ymin = PostProbNegYciL,
      ymax = PostProbNegYciU, alpha = 0.03), fill  = "darkseagreen3") +
    
    geom_text(data = prepostLabels, size = 4, aes(x,y,label = labels)) 
 }
  
