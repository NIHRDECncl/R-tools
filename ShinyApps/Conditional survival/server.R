################# server for ShinyApp to explore survival aand conditional survival

# load data as unreactive objects
browser()
path <- "data/ConditionalSurvival.xlsx"
sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
names(sheets) <- "sheets"
cat(file=stderr(), "spreadsheets", sheets[[1]])


metadata4Plots <- read_excel(path, sheets$sheets[1])
data4Plots <- read_excel(path, sheets$sheets[2])
cat(file=stderr(), "spreadsheets", metadata4Plots)

# initialise condition choices
conditionChoices <- 
  metadata4Plots$condition %>%
  sort() %>%
  unique()

shinyServer( 
  function(input, output, session) {
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

  observe({
    updateSelectInput(session, "condition", label = NULL, choices = conditionChoices)
  })

    datasetChoices <- reactive({
    subset(metadata4Plots, plotNameAndDataset == input$prognosisPlot)$dataset %>%
    sort() %>%
    unique()
})

    # reactively update prognosis plot choices
    # build the list
    prognosisPlotChoices <- reactive({
      subset(metadata4Plots, condition == input$condition & view == "Prognosis")$plotNameAndDataset %>%
      sort()
    })
    # update the picklist in ui
    observe({
      updateSelectInput(session, "prognosisPlot", label = NULL, choices = prognosisPlotChoices())
    })

    # reactively update dataset used for prognosis plot, so that it is available to select CS plot choices
    datasetChoice <- reactive({
      subset(metadata4Plots, plotNameAndDataset == input$prognosisPlot)$dataset[1]
    })

    # reactively update input choices for conditional survival plot
    # build the list
    csPlotChoices <- reactive({
      subset(metadata4Plots, dataset == datasetChoice() & view == "Conditional survival")$plotNameAndDataset %>%
        sort()
        unique()
    })

        # update the picklist in ui
    observe({
      updateSelectInput(session, "conditionalSurvivalPlot", label = NULL, choices = csPlotChoices())
    })
    
    # render datatables so that they can be checked when debugging
    # wrap in observe to ensure this is done on start up
    observe({
      output$sheets <- renderDataTable(sheets)
      output$metadata4Plots <- renderDataTable(metadata4Plots)
      output$data4Plots <- renderDataTable(data4Plots)
      output$prognosisPlotChoices <- renderText(prognosisPlotChoices())
      output$prognosisPlotChoice <- renderText(input$prognosisPlot)
      output$pData <- renderDataTable(pData())
   
      output$csPlotChoices <- renderText(csPlotChoices())
      output$csPlotChoice <- renderText(input$csPlot)
      output$scData <- renderDataTable(csData())
    })
    
    
    
  Legend4Prognosis <- reactive({
    subset(plotsMetadata, Condition == input$condition & PlotName == input$prognosisPlot)$PlotLegend[1]
  })

  output$Legend4Prognosis <- renderText(Legend4Prognosis()) # for tracking - delete later

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
  pLegendTitle <-reactive({ pMetadata$title4Legend[1]}) 
  output$pText4Figure <- renderText(pMetadata$text4Figure[1])
  
  showCI <- reactive({input$showUncertainties[1] == "group averages"})
  showBW <- reactive({input$showUncertainties[2] == "individual best and worst prospects"})

  # plot prognosis
  output$pPlot <- renderPlot({
    pplot(pData(), pPlotTitle(), pXlab(), pYlab(), pLegendTitle(), showCI(), showBW(), input$facetWrap, ncols = 2)  
  })

  # labels for plots
  csXlab <- reactive({csMetadata$xLabel[1]})
  csYlab <- reactive({csMetadata$yLabel[1]})
  csPlotTitle <- reactive({csMetadata$text4Figure[1]})
  csLegendTitle <-reactive({csMetadata$title4Legend[1]}) 
  
  output$csText4Figure <- renderText(csMetadata$text4Figure[1])
  
  # plot conditional survival
  output$csPlot <- renderPlot({
    pplot(csData(), csPlotTitle(), csXlab(), csYlab(), csLegendTitle(), showCI(), showBW(), input$facetWrap, ncols = 2L)  
      })
})
