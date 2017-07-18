################# server for ShinyApp to explore survival aand conditional survival
server <- function(input, output, session) {
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

  # load data as unreactive objects
  
  path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
  # path <- paste0("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/Conditional survival", "/data/ConditionalSurvival.xlsx")
  sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
  names(sheets) <- "sheets"
  
  metadata4Plots <- read_excel(path, sheets$sheets[1])
  data4Plots <- read_excel(path, sheets$sheets[2])
  
  datasetChoices <- subset(metadata4Plots, plotNameAndDataset == prognosisPlotChoice)$dataset %>% 
    sort() %>% 
    unique()
  
  conditionChoices <- metadata4Plots$condition %>% 
    sort() %>% 
    unique()
  
  prognosisPlotChoices <- 
    subset(metadata4Plots, condition == conditionChoice & view == "Prognosis")$plotNameAndDataset %>% 
    sort()
  
  
  
  
  
  
  
  # render datatables (non-reac tively) so that they can be checked when debuggiing
  # wrap in observe to ensure this is done on start up
  observe({
    output$sheets <- renderDataTable(sheets)
    output$metadata4Plots <- renderDataTable(metadata4Plots)
    output$data4Plots <- renderDataTable(data4Plots)
  })
    
    # initialise condition choices 
    observe({
      updateSelectInput(session, "condition", label = NULL, choices = conditionChoices)
    }) 
    
    
    # reactively update prognosis plot choices
    # build the list
    prognosisPlotChoices <- reactive({
      subset(metadata4Plots, condition == input$condition & view == "Prognosis")$plotNameAndDataset %>% 
      sort()
    })
    # update the picklist in ui
    observeEvent({
      updateSelectInput(session, "prognosisPlot", label = NULL, choices = prognosisPlotChoices())
    }, ignoreNULL = FALSE)
 
    # reactively update dataset used for prognosis plot, so that it is available to select CS plot choices
    datasetChoice <- reactive({
      subset(metadata4Plots, plotNameAndDataset == input$prognosisPlot)$dataset[1]
    })
    
    # reactively choices for conditional survival plot, so that it is available to select CS plot choices
    # build the list
    csPlotChoices <- reactive({
      subset(metadata4Plots, dataset == datasetChoice() & view == "Conditional survival")$plotNameAndDataset %>% 
        sort()
        unique()
    })
    # update the picklist in ui
    observeEvent({
      updateSelectInput(session, "conditionalSurvivalPlot", label = NULL, choices = csPlotChoices())
    }, ignoreNULL = FALSE)
    

    # for tracking - delete later
    output$test <- renderText(input$conditionalSurvivalPlot)  # for tracking - delete later
  
  Legend4Prognosis <- reactive({
    subset(plotsMetadata, Condition == input$condition & PlotName == input$prognosisPlot)$PlotLegend[1] 
  })
  
  # for tracking - delete later
  output$Legend4Prognosis <- renderText(Legend4Prognosis()) # for tracking - delete later
  
  # for tracking - delete later
  output$Legend4ConditionalPlot <- renderText({
    subset(plotsMetadata, 
           Condition == input$condition & PlotName == input$conditionalSurvivalPlot)$PlotLegend[1]
    })

  # subset data for prognosis plot
  pData <- reactive({
    subset(data4Plots, plotNameAndDataset == input$prognosisPlot)
    })
  
  # subset metadata for prognosis plot
  pMetadata <- reactive({
    subset(metadata4Plots, plotNameAndDataset == input$prognosisPlot)
  })
  
  # subset data for conditional survival plot
  csData <- reactive({
    subset(data4Plots, plotNameAndDataset == input$conditionalSurvivalPlot)
  })
  # subset metadata for conditional survival plot
  csMetadata <- reactive({
    subset(metadata4Plots, plotNameAndDataset == input$conditionalSurvivalPlot)
  })
  
  # labels for plots
  pXlab <- reactive({pMetadata$xLabel[1]})
  pYlab <- reactive({pMetadata$yLabel[1]})
  pPlotTitle <- reactive({pMetadata$text4Figure[1]})
  pLegendTitle <-reactive({ pMetadata$title4Legend[1]}) ##########  check
  
  output$pPlot <- renderPlot({
    ggplot(pData(), aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) +
      labs(title = pPlotTitle(), x = pXlab(), y = pYlab(), colour = pLegendTitle(), fill = NULL) +
      theme(plot.title = element_text(size = 12, colour = "darkseagreen4", face = "bold")) +
        if (sum(is.na(pData()$group2)) == 0) 
          facet_wrap(~group1, ncol = 1) +
        geom_line() + geom_point() +
        geom_ribbon(
          aes(ymin = wbMin, ymax = wbMax, 
              fill = factor(legend4Line), colour = factor(legend4Line)), 
          alpha = 1/10, linetype = 0) +
        geom_ribbon(
          aes(ymin = ciMin, ymax = ciMax, 
              fill = factor(legend4Line), colour = factor(legend4Line)), 
          alpha = 1/10, linetype = 0) +
        scale_fill_discrete(breaks = NULL)
  })
  
  
  
  
  
  
  csXlab <- csMetadata$xLabel[1]
  csYlab <- csMetadata$yLabel[1]
  csPlotTitle <- csMetadata$text4Figure[1]
  csLegendTitle <- csMetadata$title4Legend[1] ##########  check
  
  csPlot <- ggplot(csData, aes(time, proportion, group = legend4Line, colour = legend4Line, fill = legend4Line)) 
  csPlot <- csPlot + labs(title = csPlotTitle, x = csXlab, y = csYlab, colour = csLegendTitle, fill = NULL)
  csPlot <- csPlot + theme(plot.title = element_text(size = 12, colour = "steelblue4", face = "bold"))
  
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
  
  
}
