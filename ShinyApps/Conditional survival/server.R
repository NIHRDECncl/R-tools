################# server for ShinyApp to explore survival aand conditional survival
server <- function(input, output, session) {
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

    output$sheets <- renderDataTable(sheets)
    output$metadata4Plots <- renderDataTable(metadata4Plots)
    output$data4Plots <- renderDataTable(data4Plots)
    
    observeEvent({
      updateSelectInput(session, "condition", label = NULL, choices = conditionChoices)
    }, ignoreNULL = FALSE)
    
    prognosisPlotChoices <- reactive({
      subset(metadata4Plots, condition == input$condition & view == "Prognosis")$plotNameAndDataset %>% 
      sort()
    })
    
    observeEvent({
      updateSelectInput(session, "prognosisPlot", label = NULL, choices = prognosisPlotChoices())
    }, ignoreNULL = FALSE)
 
    datasetChoice <- reactive({
      subset(metadata4Plots, plotNameAndDataset == input$prognosisPlot)$dataset[1]
    })
    
    csPlotChoices <- reactive({
      subset(metadata4Plots, dataset == datasetChoice() & view == "Conditional survival")$plotNameAndDataset %>% 
        sort()
        unique()
    })
    
    observeEvent({
      updateSelectInput(session, "conditionalSurvivalPlot", label = NULL, choices = csPlotChoices())
    }, ignoreNULL = FALSE)
    
#########################        =======================
  ############ why is text output not working?????????
  
  output$test <- renderText(input$conditionalSurvivalPlot)
  
  PlotLegendPrognosis <- reactive({
    subset(plotsMetadata, Condition == input$condition & PlotName == input$prognosisPlot)$PlotLegend 
  })
  
  output$LegendPrognosisPlot <- renderText(PlotLegendPrognosis())
  
  output$LegendConditionalPlot <- renderText({
    subset(plotsMetadata, 
           Condition == input$condition & PlotName == input$conditionalSurvivalPlot)$PlotLegend
    })

  output$PlotPrognosis <- renderPlot({
    prognosisData <- 
      prognosisPlotsData %>% 
        filter(Condition == input$condition & PlotName == input$prognosisPlot)
ggplot(prognosisData
  })
  
  output$PlotConditionalSurvival <- renderPlot({
    conditionalSurvivalData <- 
      conditionalSurvivalPlotsData %>% 
      filter(Condition == input$condition & PlotName == input$conditionalSurvivalPlot)

  })
  
  }
