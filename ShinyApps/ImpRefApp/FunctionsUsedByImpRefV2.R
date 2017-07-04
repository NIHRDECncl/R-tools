##########################################################
# FunctionsUsedByImpRefV2.R()
#
# non-reactive functions used by the Shiny App to explore the uncertainties in diagnostic accuracy measurements when there is an Imperfect Reference


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


##########################################################
# ciproportion()
# 
# wrapper for function to calculate confidence interval for a proportion with 
#  y = numerator
#  n = denominator
#  y/n = observed proportion
#

ciproportion <- function(y,n) {
  alpha <- 0.05 # specifies 95% interval
  
  #########
  ###
  #   http://vassarstats.net/clin1.html
  ###
  #
  #  Newcombe, Robert G. "Two-Sided Confidence Intervals for the Single Proportion: 
  #      Comparison of Seven Methods," Statistics in Medicine, 17, 857-872 (1998).
  #
  #  Wilson, E. B. "Probable Inference, the Law of Succession, and Statistical Inference," 
  #      Journal of the American Statistical Association, 22, 209-212 (1927).
  #
  ### more references: 
  # 
  #   Approximate Is Better than "Exact" for Interval Estimation of Binomial Proportions
  #   Alan Agresti; Brent A. Coull
  #   The American Statistician, Vol. 52, No. 2. (May, 1998), pp. 119-126.
  #   http://www.stat.ufl.edu/~aa/articles/agresti_coull_1998.pdf
  #
  #   http://www.bmj.com/content/318/7177/193.3
  #
  #   http://influentialpoints.com/Training/confidence_intervals_of_proportions.htm
  #
  ########
  
  z <- qnorm(1-alpha/2)
  pw <- (y+z)/(n+z^2)              # Wilson point estimator
  se <- sqrt(pw*(1-pw)/n)          # estimated se of pw
  return(data.frame(
    Conf_Low <- max(0, qnorm(alpha/2,pw,se)),  # lower 95% coerce to >= 0
    Conf_high <- min(1, qnorm(1-alpha/2,pw,se)), # upper 95% coerce to =< 1
    method <- "Wilson" # Wilson point estimator for two-sided confidence intervals for a proportion
  ))
}




##########################################################
# initDxAccList()
# 
# initialise and return the list that carries the diagnostic accuracy data, metadata (eg titles, labels), statistics, and plots

initDxAccList <- function() {
  Title <- "overtype with title for tables and plots"
  StudyType <- "cohort" # alternative is "case-control"
  IndexTest <- "overtype with name of index test" # use as label for tables and graphs
  ReferenceTest <- "overtype with name of reference test" # use as label for tables and graphs
  
  # diagnostic accuracy contingency matrix: data, and row and column totals
  DxCM <- data.frame(
    row_names = c("IndexTestPos", "IndexTestNeg", "ColTotal"), # row names in their own column for tidy wrangling
    DPresent = c(NA, NA, NA),
    DAbsent = c(NA, NA, NA),
    RowTotal = c(NA, NA, NA)
  ) 
  row.names(DxCM) <-  c("IndexTestPos", "IndexTestNeg", "ColTotal") # etc rows named for indexing
  
  # Diagnostic  accuracy stats with lower and upper confidence limits or ranges
  DxStats <- data.frame(
    row_names = c("Conf_Low", "Estimate", "Conf_High", "CIMethod"), # row names in their own column for tidy wrangling
    Prevalence = c(NA, NA, NA, NA), # prevalence is needed for PPV and NPV calculations when data is from a case-control study
    Population = c(NA, NA, NA, NA), # population is needed for contingency matrix calculation when direction is from stats
    Sensitivity = c(NA, NA, NA, NA),
    Specificity = c(NA, NA, NA, NA),
    NPV = c(NA, NA, NA, NA),
    PPV = c(NA, NA, NA, NA),
    LRpos = c(NA, NA, NA, NA),
    LRneg = c(NA, NA, NA, NA)
  )
  row.names(DxStats) <-  c("Conf_Low", "Estimate", "Conf_High", "CIMethod") # etc rows named for indexing
            
  MosaicPlot <- plot.new()
  DxAccList <- list(
    Title = Title,
    Subtitle = Subtitle,
    StudyType = StudyType, # "case-control" or "cohort"
    IndexTest = IndexTest,
    ReferenceTest = ReferenceTest,
    DxCM = DxCM,
    DxStats = DxStats,
    MosaicPlot = MosaicPlot
  )
  return(DxAccList)
}

##########################################################
# DxAcc()
# 
# function to complete the contingency table
#          either "FromData": from raw data for Tp, FP, Fn, TN, and study type (cohort or case-control)
#          or "FromStats": from sensitivity, specificity, prevalence, and population
# calculate diagnostic accuracy stats with confidence intervals
# visualise the contingency table as a mosaic plot
#          see http://www.statmethods.net/advgraphs/mosaic.html
#
# the fubction expects DxAccList to have the same structure as that set up by initDxAccList()

DxAcc <- function(DxAccList, direction = "FromStats", CImethod = "estimated range") {

  # direction options = From stats" / "From data"
  # CImethod options = "estimated range" / "proportion"
  # cat(file=stderr(), "DxAccList$DxStats", str(DxAccList$DxStats), "\n" )
  # browser()   
  
  DxStats <-  DxAccList$DxStats
  DxCM <- DxAccList$DxCM

  if(direction == "From data")  {
    # given the core for contingency matrix, complte it and calculate the accuracy stats

    # work with little names
    Tp <- DxCM["IndexTestPos", "DPresent"]
    Fn <- DxCM["IndexTestNeg", "DPresent"]
    Fp <- DxCM["IndexTestPos", "DAbsent"] 
    Tn <- DxCM["IndexTestNeg", "DAbsent"] 
    
    ITpos <- Tp + Fp  # number with index test positive
    ITneg <- Fn + Tn # number with index test negative
    
    Dp <- DxCM["ColTotal", "DPresent"] 
    Dn <- DxCM["ColTotal", "DAbsent"]
    Pop <- Dp + Dn

    # box the results in dataframes, ready for the list to be returned
    # (1) complete the contingency matrix
    
    DxCM["ColTotal", "DPresent"]     <- Dp 
    DxCM["ColTotal", "DAbsent"]      <- Dn
  
    DxCM["IndexTestPos", "RowTotal"] <- ITpos
    DxCM["IndexTestNeg", "RowTotal"] <- ITneg
    
    DxCM["ColTotal", "RowTotal"]     <- Dp + Dn

    # (2) complete the stats dataframe    
    
    DxStats["Estimate","Population"]  <-  Pop
    DxStats["Estimate","Prevalence"]  <-  Dp / Pop
    
    DxStats["Estimate","PPV"] <- Tp / (Tp + Fp)
    DxStats["Estimate","NPV"] <- Tn / (Tn + Fn)
    
    DxStats["Estimate","Sensitivity"] <- Tp / ITpos
    DxStats["Estimate","Specificity"] <- Tn / ITneg
    
    # pack the list for shipping in the return
    DxAccList$DxCM <- DxCM
    DxAccList$DxStats <- DxStats
    
  }
  else  {
    if(direction == "From stats") {
    # given sensitivity, specificity, population, prevalence calculate the contingency matrix and then the other stats

      # work with little names
      Sen <- DxStats["Estimate","Sensitivity"]
      Spec <- DxStats["Estimate","Specificity"]
      Pop <- DxStats["Estimate","Population"]
      Prev <- DxStats["Estimate","Prevalence"]
      
      Dp <- Pop * Prev
      Dn <- Pop - Dp
      
      Tp <- Sen * Dp  # sensitivity = TP/P 
      Fn <- Dp - Tp    # Dp = Tp + Fn
      
      Tn <- Spec * Dn # specificity = Tn/P
      Fp <- Dn - Tn    

      # box the results in dataframes, ready for the list
      DxStats["Estimate","PPV"] <- Tp / (Tp + Fp)
      DxStats["Estimate","NPV"] <- Tn / (Tn + Fn)

      DxCM["IndexTestPos", "DPresent"] <- Tp 
      DxCM["IndexTestNeg", "DPresent"] <- Fn 
      DxCM["IndexTestPos", "DAbsent"]  <- Fp
      DxCM["IndexTestNeg", "DAbsent"]  <- Tn 

      DxCM["IndexTestPos", "RowTotal"] <- Tp + Fp
      DxCM["IndexTestNeg", "RowTotal"] <- Fn + Tn
      
      DxCM["ColTotal", "DPresent"]     <- Dp 
      DxCM["ColTotal", "DAbsent"]      <- Dn
      
      DxCM["ColTotal", "RowTotal"]     <- Dp + Dn
      
      # pack the list for shipping in the return
      DxAccList$DxCM <- DxCM
      DxAccList$DxStats <- DxStats

    }  
    else {
      stop("need to specify the direction of calculation for DxAcc")
    }
  }
  return(DxAccList)  
}



