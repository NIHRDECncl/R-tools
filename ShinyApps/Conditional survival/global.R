###  a ShinyApp to visually explore conditional survival probabilities
# 
if( !exists("LoadPackages", mode="function")) source("FunctionsUsedByClinAccApp.R")

LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)


# tidy formatting with: 
# library(formatR)
# tidy_source(source = "/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/ClinicalAccuracyAndUtility",
# comment = getOption("formatR.comment"),
#                     indent = getOption("formatR.indent", 2), 
#                     output = TRUE, 
#                     text = NULL, 
#                     width.cutoff = 80)
                    
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
  library(readxl)
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

