################# server for ShinyApp to explore survival aand conditional survival

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



#########################################################

pplot <- function(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, facetWrap)  {
  pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
  pPlot <- pPlot + labs(title = pPlotTitle, x = pXlab, y = pYlab, colour = pLegendTitle, fill = NULL)
  pPlot <- pPlot + theme(plot.title = element_text(size = 12, colour = "darkseagreen4", face = "bold"))
  if (facetWrap) 
   pPlot <- pPlot + facet_wrap(~group1, ncol = 2)

  pPlot <- pPlot + geom_line() + geom_point()
  pPlot <- pPlot + geom_ribbon(
   aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
     alpha = 1/10, linetype = 0)

  pPlot <- pPlot + geom_ribbon(
    aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
    alpha = 1/10, linetype = 0)

  pPlot <- pPlot + scale_fill_discrete(breaks = NULL)

  pPlot
}





############################################################







# read spreadsheet

    wd <- getwd()
    path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
    path <- paste0("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/Conditional survival", "/data/ConditionalSurvival.xlsx")
    sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
    names(sheets) <- "sheets"
    
    metadata4Plots <- read_excel(path, sheets$sheets[1])
    data4Plots <- read_excel(path, sheets$sheets[2])

    #  select data to plot
    
    conditionChoices <- metadata4Plots$condition %>% 
          sort() %>% 
            unique()
    conditionChoice <- conditionChoices[1]
    
    prognosisPlotChoices <- 
      subset(metadata4Plots, condition == conditionChoice & view == "Prognosis")$plotNameAndDataset %>% 
          sort()
    prognosisPlotChoice <- prognosisPlotChoices[1]
    
    datasetChoice <- subset(metadata4Plots, plotNameAndDataset == prognosisPlotChoice)$dataset
    datasetChoice
 
    pData <- subset(data4Plots, plotNameAndDataset == prognosisPlotChoice)
    pMetadata <- subset(metadata4Plots, plotNameAndDataset == prognosisPlotChoice)
      
      csPlotChoices <- 
      subset(metadata4Plots, condition == conditionChoice & view == "Conditional survival")$plotNameAndDataset %>% 
      sort()
      csPlotChoice <- csPlotChoices[1]
  
    csData <- subset(data4Plots, plotNameAndDataset == csPlotChoice)
    csMetadata <- subset(metadata4Plots, plotNameAndDataset == csPlotChoice)
    
    pXlab <- pMetadata$xLabel[1]
    pYlab <- pMetadata$yLabel[1]
    pPlotTitle <- pMetadata$text4Figure[1]
    pLegendTitle <- pMetadata$title4Legend[1] ##########  check
    
# pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
# pPlot <- pPlot + labs(title = pPlotTitle, x = pXlab, y = pYlab, colour = pLegendTitle, fill = NULL)
# pPlot <- pPlot + theme(plot.title = element_text(size = 12, colour = "darkseagreen4", face = "bold"))
# if (sum(is.na(pData$group2)) == 0) 
#   pPlot <- pPlot + facet_wrap(~group1, ncol = 1)
# 
# pPlot <- pPlot + geom_line() + geom_point()
# pPlot <- pPlot + geom_ribbon(
#   aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
#   alpha = 1/10, linetype = 0)
# 
# pPlot <- pPlot + geom_ribbon(
#   aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
#   alpha = 1/10, linetype = 0)
# 
# pPlot <- pPlot + scale_fill_discrete(breaks = NULL)
# 

  
    pplot(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, facetWrap = FALSE)  
    pplot(csData, csPlotTitle, csXlab, csYlab, csLegendTitle, facetWrap = TRUE)  
    


csXlab <- csMetadata$xLabel[1]
csYlab <- csMetadata$yLabel[1]
csPlotTitle <- csMetadata$text4Figure[1]
csLegendTitle <- csMetadata$title4Legend[1] ##########  check

pplot(csData, csPlotTitle, csXlab, csYlab, csLegendTitle, facetWrap = TRUE)  


# csPlot <- ggplot(csData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
# csPlot <- csPlot + labs(title = csPlotTitle, x = csXlab, y = csYlab, colour = csLegendTitle, fill = NULL)
# csPlot <- csPlot + theme(plot.title = element_text(size = 12, colour = "steelblue4", face = "bold"))
# 
# if (sum(is.na(csData$group2)) == 0) 
#   csPlot <- csPlot + facet_wrap(~group1, ncol = 2)
# 
# csPlot <- csPlot + geom_line() + geom_point()
# 
# csPlot <- csPlot + geom_ribbon(
#   aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
#   alpha = 1/10, linetype = 0)
# 
# csPlot <- csPlot + geom_ribbon(
#   aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
#   alpha = 1/10, linetype = 0)
# 
# csPlot <- csPlot + scale_fill_discrete(breaks = NULL)
# 
# csPlot



