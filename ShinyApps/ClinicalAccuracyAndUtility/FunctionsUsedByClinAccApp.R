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
  
  # small input boxes
  textInput3<-function (inputId, label, value = "",...) 
  {
    div(style="display:inline-block",
        tags$label(label, `for` = inputId), 
        tags$input(id = inputId, type = "text", value = value,...))
  }
  
  
  
  
  # side by side boxes
  textInput2<-function (inputId, label, value = "",...) 
  {
    tagList(tags$label(label, `for` = inputId), tags$input(id = inputId, 
                                                           type = "text", value = value,...))
  }