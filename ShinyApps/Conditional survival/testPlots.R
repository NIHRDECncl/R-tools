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
    pLegendTitle <- pMetadata$title4Legend[1] ##########  check
    
pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
pPlot <- pPlot + labs(x = pXlab, y = pYlab, colour = pLegendTitle, fill = NULL)

if (sum(is.na(pData$group2)) == 0) 
  pPlot <- pPlot + facet_wrap(~group1, ncol = 1)

pPlot <- pPlot + geom_line() + geom_point()
pPlot <- pPlot + geom_ribbon(
  aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

pPlot <- pPlot + geom_ribbon(
  aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

pPlot <- pPlot + scale_fill_discrete(breaks = NULL)

pPlot




csXlab <- csMetadata$xLabel[1]
csYlab <- csMetadata$yLabel[1]
csLegendTitle <- csMetadata$title4Legend[1] ##########  check

csPlot <- ggplot(csData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
# csPlot <- csPlot + labs(x = csXlab, y = csYlab, colour = "Stage and\nage group", fill = NULL)
csPlot <- csPlot + labs(x = csXlab, y = csYlab, colour = csLegendTitle, fill = NULL)

if (sum(is.na(csData$group2)) == 0) 
  csPlot <- csPlot + facet_wrap(~group1, ncol = 2)

csPlot <- csPlot + geom_line() + geom_point()

csPlot <- csPlot + geom_ribbon(
  aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

csPlot <- csPlot + geom_ribbon(
  aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

csPlot <- csPlot + scale_fill_discrete(breaks = NULL)

csPlot

  
