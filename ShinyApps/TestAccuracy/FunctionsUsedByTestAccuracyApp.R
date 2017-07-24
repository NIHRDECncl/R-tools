##########################################################
# FunctionsUsedByTestAccuracyApp.R()
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
  library(ggrepel)
  # ...
}