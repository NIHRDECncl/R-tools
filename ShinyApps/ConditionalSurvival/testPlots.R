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
library(plotly)



#########################################################

  pplot <- function(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = TRUE, showBW = TRUE, facetWrap = TRUE, ncol = 2L, group1Name = "Group1", group2Name = "Group2")  {
  # pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
  pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = group1, fill = group2)) 
  pPlot <- pPlot + labs(title = pPlotTitle, x = pXlab, y = pYlab, colour = pLegendTitle, fill = NULL)
  pPlot <- pPlot + theme(plot.title = element_text(size = 12, colour = "darkseagreen4", face = "bold"))
  
  if (facetWrap) 
   pPlot <- pPlot + facet_wrap(~group1, ncol = ncol)

  pPlot <- pPlot + geom_line() + geom_point()
  
  if (showCI) 
    pPlot <- pPlot + geom_ribbon(
     aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
     alpha = 1/10, linetype = 0)

  if (showBW) 
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
    path <- paste0("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/ConditionalSurvival", "/data/ConditionalSurvival.xlsx")
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
    pLegendTitle <- pMetadata$title4Legend[1]
    pGroup1Name <- pMetadata$group1Name[1]
    pGroup2Name <- pMetadata$group2Name[1]
    
    

    
    pplot(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = FALSE, showBW = FALSE, facetWrap = TRUE, ncol = 2L, group1Name = pGroup1Name, group2Name = pGroup2Name)  
    pplot(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = TRUE, showBW = FALSE, facetWrap = TRUE, ncol = 2L, group1Name = pGroup1Name, group2Name = pGroup2Name)  
    pplot(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = FALSE, showBW = TRUE, facetWrap = TRUE, ncol = 2L, group1Name = pGroup1Name, group2Name = pGroup2Name)  
    pplot(pData, pPlotTitle, pXlab, pYlab, pLegendTitle, showCI = TRUE, showBW = TRUE, facetWrap = TRUE, ncol = 2L, group1Name = pGroup1Name, group2Name = pGroup2Name)  



  csXlab <- csMetadata$xLabel[1]
  csYlab <- csMetadata$yLabel[1]
  csPlotTitle <- csMetadata$text4Figure[1]
  csLegendTitle <- csMetadata$title4Legend[1]
  csGroup1Name <- csMetadata$group1Name[1]
  csGroup2Name <- csMetadata$group2Name[1]
  
  
  ggplotly( 
   pplot(csData, csPlotTitle, csXlab, csYlab, csLegendTitle, showCI = FALSE, showBW = FALSE, facetWrap = TRUE, ncol = 2L, group1Name = csGroup1Name, group2Name = csGroup2Name) 
         )
 # pplot(csData, csPlotTitle, csXlab, csYlab, csLegendTitle, showCI = TRUE, showBW = FALSE, facetWrap = TRUE, ncol = 2L, group1Name = csGroup1Name, group2Name = csGroup2Name)  
  # pplot(csData, csPlotTitle, csXlab, csYlab, csLegendTitle, showCI = FALSE, showBW = TRUE, facetWrap = TRUE, ncol = 2L, group1Name = csGroup1Name, group2Name = csGroup2Name)  
  # pplot(csData, csPlotTitle, csXlab, csYlab, csLegendTitle, showCI = TRUE, showBW = TRUE, facetWrap = TRUE, ncol = 2L, group1Name = csGroup1Name, group2Name = csGroup2Name)  
  # 


