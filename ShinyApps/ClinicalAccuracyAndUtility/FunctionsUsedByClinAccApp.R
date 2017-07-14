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
  
  
 
#   # small input boxes
#   textInput3<-function (inputId, label, value = "",...) 
#   {
#     div(style="display:inline-block",
#         tags$label(label, `for` = inputId), 
#         tags$input(id = inputId, type = "text", value = value,...))
#   }
#   
#   
#   
#   
#   # side by side boxes
#   textInput2<-function (inputId, label, value = "",...) 
#   {
#     tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, 
#                                                            type = "text", value = value,...))
#   }
#   
  
  
  ruleinoutplot <- function(n, prevalence, sensitivity, specificity, RuleInDecisionThreshold, RuleOutDecisionThreshold, #}, 
                            DxCondition,  DxTestName){
    
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
       theme(plot.title = element_text(size = 16, face = "bold"), axis.text = element_text(size = 12), 
             axis.title = element_text(size = 14))# +
  #    geom_text(size = 4, aes(x,y,label = fixedlabels)) + 
   #    geom_text(size = 5, aes(x, y, label = postTestLabels))
  

  }
  
  
