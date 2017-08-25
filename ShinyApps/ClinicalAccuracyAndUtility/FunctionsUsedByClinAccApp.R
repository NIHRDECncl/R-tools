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
  #===================================================================================================
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
    library(ggrepel)
    # ...
  }
  
  
  #
  #===================================================================================================
  #
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
  #
  #===================================================================================================
  #
  DxStats <- function(n, prevalence, sensitivity, specificity, plot2x2 = FALSE) {
    prevalence <- min(prevalence,0.9999)
    prevalence <- max(prevalence,0.0001)
    
    Dpos <- n * prevalence
    Dneg <- n - Dpos
    
    Tp <- sensitivity * Dpos
    Tn <- specificity * Dneg
    
    TpPct = Tp/n
    TnPct = Tn/n
    
    Fn <- (1 - sensitivity) * Dpos
    Fp <- (1 - specificity) * Dneg
    
    FnPct = Fp/n
    FpPct = Fp/n
    
    TestPos = Tp + Fp
    TestNeg = Tn + Fn
    
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
    
    dx2x2 <- data_frame(
      Dpos = Dpos,
      Dneg = Dneg,
      
      TestPos = TestPos,
      TestNeg = TestNeg,
      
      Tp = Tp,
      Tn = Tn,
      
      Fn = Fn,
      Fp = Fp,
      
      PPV = PPV,
      NPV = NPV,
      
      TpPct = TpPct,
      TnPct = TnPct,
      
      FnPct = FnPct,
      FpPct = FpPct,
      
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
      TNY_ciU = TNY_ciU,
      
      n = n,
      
      barplot = list(NULL)
    )
    if (plot2x2) {
      
      nudgeX <- 1
      nudgeY <- 1.05
      
      dx <- data_frame(
        display = c(
          rep("Tested populations", 8), 
          rep("Tested proportions", 8)),
        population = 
          rep(
            c(
              rep("Pre-testing", 4), 
              rep("Post-positive test", 2), 
              rep("Post-negative test", 2)),
            2),
        result = rep(c("TP", "FN", "FP", "TN", "TP", "FP", "TN", "FN"), 2),
        label = rep(c("TP", "FN", "FP", "TN", "TP", "FP", "TN", "FN"), 2),
        xmin = rep(c(0, 0, 0, 0, 4, 4, 8, 8), 2),
        xmax = rep(c(2, 2, 2, 2, 6, 6, 10, 10), 2),
        ymin = c(0, Tp, (Tp + Fn), (Tp + Fn + Fp), 0, Tp, 0, Tn,
                 0, TpPct, (TpPct + FnPct), (TpPct + FnPct + FpPct), 0, TpPct, 0, TnPct),
        ymax = c(Tp, (Tp + Fn), (Tp + Fn + Fp), n, Tp, TestPos, Tn, TestNeg,
                 TpPct, (TpPct + FnPct), (TpPct + FnPct + FpPct), 1, TpPct, 1, TnPct, 1),
        plotLabels = rep( c(
          "Pre-testing", "", "", "", 
          "Tested +ve", "", 
          "Tested -ve", ""), 2),
        labelX = rep(c(0, 0, 0, 0, 4, 4, 8, 8), 2) + nudgeX,
        labelY = c(n, 0, 0, 0, TestPos, 0 , TestNeg, 0, rep(1, 8))*nudgeY
        
      )
      
      dx2x2$barplot[[1]] <- 
        ggplot(dx, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)) +
        geom_rect(aes(fill = result)) +
        scale_x_continuous(breaks = NULL) +
        facet_wrap(~ dx$display, scales = "free_y", ncol = 2) +
        geom_text(aes(x = labelX, y = labelY, label = plotLabels))
      
    }
    
    return(dx2x2)
  }
  
  
  #
  #===================================================================================================
  #
  
  
  fixedlabels <- function(RuleInDecisionThreshold, prevalence, RuleOutDecisionThreshold, 
                          DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision, disper)  {
    
    if (disper) {
      percent <- 100
      lab <- "%"
    } else {
      percent <- 1
      lab <- ""
    }
    
    data.frame(
      x = c(0.35, 0.85, 0.35, 0.35),
      y = c(
        RuleInDecisionThreshold, #+ 0.05, 
        prevalence,  #  0.05 
        RuleOutDecisionThreshold, #+ 0.05,
        RuleInDecisionThreshold - 0.05
      ),
      labels = c(
        paste0(RuleInDecisionThreshold*percent, lab , " = threshold for rule-in decision: ", DxRuleInDecision),
        paste0("Prevalence = ", prevalence*percent, lab),
        paste0(RuleOutDecisionThreshold*percent, lab, " = threshold for rule-out decision: ", DxRuleOutDecision),
        paste0("Action when indeterminate: ", IndeterminateDecision)),
      fillColours = c("firebrick4", "springgreen4")
    )

  }
  
  #
  #===================================================================================================
  #
  
  postTestLabels <- function(n, prevalence, sensitivity, specificity, disper){
    
    if (disper) {
      percent <- 100
      lab <- "%"
      dig <- 0
    } else {
      percent <- 1
      lab <- ""
      dig <- 2
    }
    
   
    Dx <- DxStats(n, prevalence, sensitivity, specificity) 
    
    Nudge <- 0.02
    if (Dx$TPY_ciU - Dx$TPY_ciL < 3*Nudge) NudgeCIp = + Nudge else NudgeCIp = - Nudge/3
    if (Dx$TNY_ciU - Dx$TNY_ciL < 3*Nudge) NudgeCIn = + Nudge else NudgeCIn = - Nudge/3
    
   data.frame(
      x = c(0.85, 0.85, 1.075, 1.075, 1.075, 1.075),
      y = c(
        Dx$PostTestProbP + 0.05, # = post +ve test probability 
        Dx$PostTestProbN - 0.05, # = post -ve test probability
        Dx$TPY_ciL - NudgeCIp,
        Dx$TPY_ciU + NudgeCIp,
        Dx$TNY_ciL - NudgeCIn,
        Dx$TNY_ciU + NudgeCIp),
      
      labels = c(
        strwrap(paste0("Prob post +ve test = ", round(Dx$PostTestProbP*percent, dig), lab), 40),
        strwrap(paste0("Prob post -ve test = ", round(Dx$PostTestProbN*percent, dig), lab), 40),
        paste0(round(Dx$TPY_ciL*percent, dig), lab),
        paste0(round(Dx$TPY_ciU*percent, dig), lab),
        paste0(round(Dx$TNY_ciL *percent, dig), lab),
        paste0(round(Dx$TNY_ciU *percent, dig), lab)
         )
    )
  }
  
  #
  #===================================================================================================
  #
  
  ruleinoutplot <- function(n, prevalence, sensitivity, specificity, RuleInDecisionThreshold, RuleOutDecisionThreshold, #}, 
                            DxCondition,  DxTestName, DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision, disper){
    
    if (disper) percent = 100
    if (!disper) percent = 1
    
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
                    DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision, disper)

    postTestLabels <- postTestLabels(n, prevalence, sensitivity, specificity, disper)
    
    
    ggplot(linesdf) +
      geom_line(aes(x = PriorAxisX, y = PriorAxisY*percent), data =linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = PostAxisX, y = PostAxisY*percent), data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = PrevX, y = PrevY*percent, colour="coral1"), size = 1.5, data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = RuleInDecisionThresholdX, y = RuleInDecisionThresholdY*percent, colour="cadetblue"), size = 1.5, data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = RuleOutDecisionThresholdX, y = RuleOutDecisionThresholdY*percent, colour="springgreen4"), size = 1.5, data = linesdf, stat = "identity", position = "identity") +
      geom_line(aes(x = TestPosX, y = TestPosY*percent), size = 1.15, data = linesdf, stat = "identity", position = "identity", colour = "firebrick4") +
      geom_ribbon(data = linesdf, aes(x = TestPosX, ymin = TPY_ciL*percent, ymax = TPY_ciU*percent, alpha = 0.03), fill  = "lightsalmon") +
      geom_line(aes(x = TestNegX, y = TestNegY*percent), size = 1.15, data = linesdf, stat = "identity", position = "identity") +
      geom_ribbon(data = linesdf, aes(x = TestNegX, ymin = TNY_ciL*percent, ymax = TNY_ciU*percent, alpha = 0.03), fill  = "darkseagreen3") +
      theme(
       axis.text.x = element_blank(),
        legend.position="none") +
       labs(x = "", y = paste0("Probability of ", DxCondition), size = 8) +
       ggtitle(paste("Post-test probabilities after", DxTestName, "for", DxCondition)) +
       theme(plot.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 12), 
             axis.title = element_text(size = 14)) +
     geom_label_repel(size = 4, data = fixedlabels, aes(x,y*percent,label = labels), 
                label.padding = unit(0.15, "lines"), label.r = unit(0.2, "lines")) + 
      geom_text(data = postTestLabels, aes(x,y*percent,label = labels), size = 4 ) #+ geom_label()  
  #    geom_text_repel(point.padding = NA)
  

  }
  
  #
  #===================================================================================================
  #
  
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

  #
  #===================================================================================================
  #
  
    
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
  #
  #===================================================================================================
  #
  prepostLabels <- function(n, prevalence, sensitivity, specificity, disper) {
    
    if (disper) {
      percent <- 100
      lab <- "%"
      dig <- 0
    } else {
      percent <- 1
      lab <- ""
      dig <- 2
    }
    
    ### save stats so don't have to call many times
    Dx <- DxStats(n, prevalence, sensitivity, specificity)
    data.frame(
      x = c(0.15, 0.15, prevalence),
      y = c(
        Dx$PostTestProbP + 0.035, 
        Dx$PostTestProbN - 0.035,
        0.10),
      
      labels = c(
        strwrap(paste0("Prob post +ve test = ", round(Dx$PostTestProbP*percent, dig), lab), 40),
        strwrap(paste0("Prob post -ve test = ", round(Dx$PostTestProbN*percent, dig), lab), 40),
        paste0("Prevalence = ", round(prevalence * percent, dig), lab)
      )
    )
    
  }
  #
  #===================================================================================================
  #
  
 prepostprobplot <- function(n, prevalence, sensitivity, specificity, DxCondition, DxTestName, disper){
   
   if (disper) percent = 100
   if (!disper) percent = 1
   
   graphPre2PostProb <-  graphPre2PostProb(n, prevalence, sensitivity, specificity)
   linesPre2PostProb <- linesPre2PostProb(n, prevalence, sensitivity, specificity)
   prepostLabels <- prepostLabels(n, prevalence, sensitivity, specificity, disper)
   
  ggplot(graphPre2PostProb) +
    geom_line(data = graphPre2PostProb, aes(x = x*percent, y = yP*percent), stat = "identity", position = "identity") +
    geom_ribbon(data = graphPre2PostProb, aes(x = x*percent,ymin = yPciL*percent, ymax = yPciU*percent, alpha = 0.03), fill  = "lightsalmon") +
    geom_line(aes(x = x*percent, y = yN*percent), data = graphPre2PostProb, stat = "identity", position = "identity") +
    geom_ribbon(data = graphPre2PostProb,  aes(x = x*percent, ymin = yNciL*percent, ymax = yNciU*percent, alpha = 0.03), fill  = "darkseagreen3") +
    theme(legend.position="none") +
    labs(x = "Pre-test probability (prevalence)", y = paste0("Post test probability after ", DxTestName)) +
    ggtitle(paste("Pre- and post-test probabilities after", DxTestName, "for", DxCondition)) +
    theme(plot.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14)) +
    
    geom_line(data = linesPre2PostProb, aes(x = prevalenceX*percent,
      y = prevalenceY*percent), stat = "identity", position = "identity") +
    geom_line(data = linesPre2PostProb, aes(x = PostProbPosX*percent,
      y = PostProbPosY*percent), stat = "identity", position = "identity") +
    geom_ribbon(data = linesPre2PostProb, aes(x = PostProbPosX*percent,
      ymin = PostProbPosYciL*percent,  ymax = PostProbPosYciU*percent, alpha = 0.03), fill  = "lightsalmon") +
    geom_line(data = linesPre2PostProb, aes(x = PostProbNegX*percent, y = PostProbNegY*percent), stat = "identity", position = "identity") +
    geom_ribbon(data = linesPre2PostProb, aes(x = PostProbNegX*percent,
      ymin = PostProbNegYciL*percent,
      ymax = PostProbNegYciU*percent, alpha = 0.03), fill  = "darkseagreen3") +
    geom_label_repel(data = prepostLabels, size = 4, aes(x*percent,y*percent, label = labels),  color = 'black', 
                     label.padding = unit(0.15, "lines"), label.r = unit(0.2, "lines")) #+
  #  geom_label()
 }
  
 #
 #===================================================================================================
 #
 
 predictiveValues <-  function(n, prevalence, sensitivity, specificity, 
                               RuleInDecisionThreshold, RuleOutDecisionThreshold, 
                               DxCondition,  DxTestName, DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision, disper
                             ) {
                    NULL
                  }
  
 