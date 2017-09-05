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
      labs(x = NULL, y = NULL) +
      facet_wrap(~ dx$display, scales = "free_y", ncol = 2) +
      geom_text(aes(x = labelX, y = labelY, label = plotLabels))
    
  }
  
  return(dx2x2)
}


#
#===================================================================================================
#

predictiveValues <-  function(n, prevalence, sensitivity, specificity, 
                              RuleInDecisionThreshold, RuleOutDecisionThreshold, 
                              DxCondition,  DxTestName, DxRuleInDecision, DxRuleOutDecision, IndeterminateDecision, disper) {
  NULL
}


#
#===================================================================================================
#

LoadPackages()

# initialize parameters

n <- 333
prevalence <- 0.4
sensitivity <- 0.9
specificity <- 0.80 
RuleInDecisionThreshold <- 0.7
RuleOutDecisionThreshold <- 0.2
DxCondition <- "Disease"
DxTestName <- "Test"
DxRuleInDecision <- "Treat"
DxRuleOutDecision <- "Stop treatment"
IndeterminateDecision <- "Incestigate further"
disper <- TRUE


#
#===================================================================================================
#
# prepare dataframe for plotting predictive value bar charts


DxStats(n, prevalence, sensitivity, specificity, plot2x2 = TRUE)$barplot[[1]]





  
