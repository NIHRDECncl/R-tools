################# server for ShinyApp to explore survival aand conditional survival

# read spreadsheet

    wd <- getwd()
    path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
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
    pMetaData <- subset(metadata4Plots, plotNameAndDataset == prognosisPlotChoice)
      
      csPlotChoices <- 
      subset(metadata4Plots, condition == conditionChoice & view == "Conditional survival")$plotNameAndDataset %>% 
      sort()
      csPlotChoice <- csPlotChoices[1]
  
    csData <- subset(data4Plots, plotNameAndDataset == csPlotChoice)
    csMetaData <- subset(metadata4Plots, plotNameAndDataset == csPlotChoice)
    
    pXlab <- pMetaData$xLabel[1]
    pYlab <- pMetaData$yLabel[1]
    pGroup1Name <- pData$legend4Line[1] ##########  check
    
pPlot <- ggplot(pData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
pPlot <- pPlot + labs(x = pXlab, y = pYlab, colour = pGroup1Name, fill = NULL)
pPlot <- pPlot + geom_line() + geom_point()
pPlot <- pPlot + geom_ribbon(
  aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

pPlot <- pPlot + geom_ribbon(
  aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

pPlot <- pPlot + scale_fill_discrete(breaks = NULL)

pPlot




csXlab <- csMetaData$xLabel[1]
csYlab <- csMetaData$yLabel[1]
csGroup1Name <- csData$legend4Line[1] ##########  check

csPlot <- ggplot(csData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
csPlot <- csPlot + labs(x = csXlab, y = csYlab, colour = csGroup1Name, fill = NULL)
csPlot <- csPlot + geom_line() + geom_point()

csPlot <- csPlot + geom_ribbon(
  aes(ymin = wbMin, ymax = wbMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

csPlot <- csPlot + geom_ribbon(
  aes(ymin = ciMin, ymax = ciMax, fill = factor(legend4Line), colour = factor(legend4Line)), 
  alpha = 1/10, linetype = 0)

csPlot <- csPlot + scale_fill_discrete(breaks = NULL)

csPlot

  
