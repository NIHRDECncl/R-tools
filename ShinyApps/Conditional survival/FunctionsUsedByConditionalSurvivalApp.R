##########################################################
# FunctionsUsedBySurvivalStatisticsApp.R()
#
# non-reactive functions used by the Shiny App 

##########################################################
#
# load packages used by the App
LoadPackages <- function() {
  library(DT)
  library(shiny)
  library(tidyverse) # Imports: broom, DBI, dplyr, forcats, ggplot2, haven, httr, hms, jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl, stringr, tibble, rvest, tidyr, xml2
  library(rsconnect)   # needed to upload to Shinyio
  library(readr)       # needed for GET()
  library(vcd)         # mosaic() plot http://www.statmethods.net/advgraphs/mosaic.html
  # library(colourpicker) # http://deanattali.com/blog/plot-colour-helper/ 
  library(shinythemes)
  library(shinycssloaders)
  #      library(proportion)  package no longer being maintained :-(
  library(PropCIs)
  library(rsconnect)   # needed to upload to Shinyio
  library(readxl)
  # ...
}

#########################################################

pplot <- function(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = TRUE, showBW = TRUE, facetWrap = TRUE, ncol = 2L)  {
  pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
  pPlot <- pPlot + labs(title = pPlotTitle, x = pXlab, y = pYlab, colour = pLegendTitle, fill = NULL)
  pPlot <- pPlot + theme(plot.title = element_text(size = 12, colour = "darkseagreen4", face = "bold"))
  
  if (facetWrap) 
    pPlot <- pPlot + facet_wrap(~group1, ncol = ncol)
  
  pPlot <- pPlot + geom_line() + geom_point()
  
  if (showBW)
    pPlot <- pPlot + geom_ribbon(
      aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)),
      alpha = 1/10, linetype = 0)

   if (showCI) 
    pPlot <- pPlot + geom_ribbon(
      aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)),
      alpha = 1/10, linetype = 0)
  
  pPlot <- pPlot + scale_fill_discrete(breaks = NULL)
  
  pPlot
}


