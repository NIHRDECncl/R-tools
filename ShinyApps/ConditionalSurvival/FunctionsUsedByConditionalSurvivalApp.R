##########################################################
# FunctionsUsedBySurvivalStatisticsApp.R()
#
# non-reactive functions used by the Shiny App 

##########################################################
#
# load packages used by the App
LoadPackages <- function() {
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
  library(plotly)
  library(DT)
  library(scales)
  # ...
}

#########################################################

pplot <- function(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = TRUE, showBW = TRUE, facetWrap = TRUE, ncol = 2L, group1Name = "Group1", group2Name = "Group2", showPI = NULL)  {
  pPlot <- ggplot(pData, aes(duration, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
  pPlot <- pPlot + labs(title = pPlotTitle, x = pXlab, y = pYlab, colour = pLegendTitle, fill = NULL)
  pPlot <- pPlot + theme(plot.title = element_text(size = 12, colour = "darkseagreen4", face = "bold"))
  pPlot <- pPlot + scale_y_continuous(labels = scales::percent)
  
  if (facetWrap) {
    ifelse (length(levels(factor(pData$group2))) == 0,
            pPlot <- pPlot + facet_wrap(~group1, ncol = ncol),
            pPlot <- pPlot + facet_wrap(~group1 + group2, ncol = ncol)
          )}
    
  pPlot <- pPlot + geom_line() + geom_point()
  
  if (showBW)
    pPlot <- pPlot + geom_ribbon(
      aes(ymin = wbMinProp, ymax = wbMaxProp, fill = factor(legend4Line), colour = factor(legend4Line)),
      alpha = 1/10, linetype = 0)
  
  if(!is.null(showPI)) {
    cRow <- pData %>% # row with clicked point
      filter(curve == showPI$curveNumber, point == showPI$pointNumber)
    print(cRow)
    predInt <- data.frame(
      xmin = c(0, cRow$wbMinDur),
      xmax = c(cRow$duration, cRow$wbMaxDur),
      ymin = c(cRow$wbMinProp, 0),
      ymax = c(cRow$wbMaxProp, cRow$proportion)
      )
    print(predInt)
    pPlot <- pPlot + annotate("rect",
      xmin=predInt$xmin, xmax=predInt$xmax,
      ymin=predInt$ymin, ymax=predInt$ymax,
      fill=c("blue", "red"), alpha=0.1
    )
    }

   if (showCI) 
    pPlot <- pPlot + geom_ribbon(
      aes(ymin = ciMinProp, ymax = ciMaxProp, fill = factor(legend4Line), colour = factor(legend4Line)),
      alpha = 2/10, linetype = 0)
  
  pPlot <- pPlot + scale_fill_discrete(breaks = NULL)
  
  pPlot
}


