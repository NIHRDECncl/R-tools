################# server for ShinyApp to explore survival aand conditional survival

# read spreadsheet

    wd <- getwd()
    path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
    sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
    names(sheets) <- "sheets"
    
    plotsMetadata <- read_excel(path, sheets$sheets[1])
    plotsData <- read_excel(path, sheets$sheets[2])

    #  select data to plot
    
    conditionChoices <- plotsMetadata$Condition %>% 
          sort %>% 
            unique
    
    conditionChoices
    conditionChoice <- conditionChoices[1]
    
    prognosisPlots <- subset(plotsMetadata, Condition == conditionChoice & View == "Prognosis")$Plot_Dataset %>% 
          sort
    prognosisPlots
    prognosisPlotChoice <- prognosisPlots[1]
    
    datasetChoice <- subset(plotsMetadata, Plot_Dataset == prognosisPlotChoice)$Dataset
    datasetChoice
    
    pData <- subset(plotsData, PlotName == prognosisPlotChoice)

    
    pXlab <- subset(plotsMetadata, Plot_Dataset == prognosisPlotChoice)$Xlab
    pYlab <- subset(plotsMetadata, Plot_Dataset == prognosisPlotChoice)$Ylab
    pGroup1Name <- subset(plotsMetadata, Plot_Dataset == prognosisPlotChoice)$Group1Name
    pGroup2Name <- subset(plotsMetadata, Plot_Dataset == prognosisPlotChoice)$Group2Name
    pPlotTitle <- subset(plotsMetadata, Plot_Dataset == prognosisPlotChoice)$PlotTitle
    
    
conditionalSurvivalPlots <- subset(plotsMetadata, Condition == conditionChoice & View == "Conditional survival" & Dataset == datasetChoice)$Plot_Dataset %>% 
    sort 

conditionalSurvivalPlots
conditionalSurvivalPlotChoice <- conditionalSurvivalPlots[1]

csData <- subset(plotsData, PlotName == conditionalSurvivalPlotChoice)

csXlab <- subset(plotsMetadata, Plot_Dataset == conditionalSurvivalPlotChoice)$Xlab
csYlab <- subset(plotsMetadata, Plot_Dataset == conditionalSurvivalPlotChoice)$Ylab
csGroup1Name <- subset(plotsMetadata, Plot_Dataset == conditionalSurvivalPlotChoice)$Group1Name
csGroup2Name <- subset(plotsMetadata, Plot_Dataset == conditionalSurvivalPlotChoice)$Group2Name
csPlotTitle <- subset(plotsMetadata, Plot_Dataset == conditionalSurvivalPlotChoice)$PlotTitle


pPlot <- ggplot(pData, aes(Time, Proportion, group = Group1, colour = Group1, fill = Group1)) 
pPlot <- pPlot + labs(x = pXlab, y = pYlab, colour = pGroup1Name, fill = NULL)
pPlot <- pPlot + geom_line() + geom_point()
pPlot <- pPlot + geom_ribbon(
  aes(ymin = WBmin, ymax = WBmax, fill = factor(Group1), colour = factor(Group1)), 
  alpha = 1/10, linetype = 0)

pPlot <- pPlot + geom_ribbon(
  aes(ymin = CILL, ymax = CIUL, fill = factor(Group1), colour = factor(Group1)), 
  alpha = 1/10, linetype = 0)

pPlot <- pPlot + scale_fill_discrete(breaks = NULL)

pPlot


                  
  
