################# server for ShinyApp to explore survival aand conditional survival

# load data as unreactive objects available to all sessions
# browser()

path <- "data/ConditionalSurvival.xlsx"
sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
names(sheets) <- "sheets"
# cat(file=stderr(), "spreadsheets", sheets[[1]])


metadata4Plots <- read_excel(path, sheets$sheets[1])
data4Plots <- read_excel(path, sheets$sheets[2])
# cat(file=stderr(), "spreadsheets", metadata4Plots)

# initialise condition choices
conditionChoices <- 
  metadata4Plots$condition %>% 
    factor() %>% 
    levels()
  

server <- 
  function(input, output, session) {
  # session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

  # go to 2nd tab on navbarMenu
    observeEvent(input$goToTabI2, {
      updateTabsetPanel(session, "navbarPage",
                        selected = "i2"
      )
    })
    
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
        sort()  %>%
        unique()
    })

        # update the picklist in ui
    observe({
      updateSelectInput(session, "conditionalSurvivalPlot", label = NULL, choices = csPlotChoices())
    })
    
    # render datatables so that they can be checked when debugging
    # wrap in observe to ensure this is done on start up
    observe({
      output$QAsheets <- renderDataTable(sheets)
      output$QAmetadata4Plots <- renderDataTable(metadata4Plots)
      output$QAdata4Plots <- renderDataTable(data4Plots)
      output$QAprognosisPlotChoices <- renderDataTable(data_frame(prognosisPlotChoices()))
      output$QAprognosisPlotChoice <- renderText(input$prognosisPlot)
      output$QApData <- renderDataTable(pData())
   
      output$QAcsPlotChoices <- renderDataTable(data_frame(csPlotChoices()))
      output$QAcsPlotChoice <- renderText(input$conditionalSurvivalPlot)
      output$QAcsData <- renderDataTable(csData())
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
  pXlab <- reactive({pMetadata()$xLabel[1]})
  pYlab <- reactive({pMetadata()$yLabel[1]})
  pPlotTitle <- reactive({pMetadata()$title4Plot[1]})
  pLegendTitle <-reactive({pMetadata()$title4Legend[1]}) 
  output$pText4Figure <- renderText(pMetadata()$text4Figure[1])
  
  # # update input labels for prognosis groups
  # pGroup1Name <-reactive({pMetadata()$group1Name[1]})
  # pGroup2Name <- reactive({pMetadata()$group2Name[1]})
  # pCgroupChoicesV <- reactive({
  #   c(pGroup1Name(), pGroup2Name()) %>% 
  #     factor() %>% 
  #     levels()
  # })
  # pCgroupChoicesN <- reactive({
  #   c(pGroup1Name(), pGroup2Name()) %>% 
  #     factor() %>% 
  #     levels()
  #   })
  # observe(
  #   updateCheckboxGroupInput(session, "pShowGroups", choiceNames = pCgroupChoicesN(), choiceValues = pCgroupChoicesV())
  # )


  output$QApXlab <- renderText(pXlab())
  output$QApYlab <- renderText(pYlab())
  output$QApPlotTitle <- renderText(pPlotTitle())
  output$QApLegendTitle <- renderText(pLegendTitle())
  output$QAptext4Figure <- renderText(pMetadata()$text4Figure[1])
  output$QAshowUncertainties <- renderText(input$showUncertainties)
  
  output$QApGroup1Name <- renderText(pGroup1Name())
  output$QApGroup2Name <- renderText(pGroup2Name())
  output$QApCgroupChoicesN <- renderText(pCgroupChoicesN())
  output$QApCgroupChoicesV <- renderText(pCgroupChoicesV())

  
  showCI <- reactive({ sum(input$showUncertainties == "CI") == 1})
  showBW <- reactive({sum(input$showUncertainties == "BW") == 1})

  # plot prognosis
  

  output$pPlot <- renderPlotly({
    ggplotly(
    pplot(pData(), pPlotTitle(), pXlab(), pYlab(), pLegendTitle(), showCI(), showBW(), input$facetWrap, ncol = 2)  
    , tooltip = c("x", "y")  )
    })

  # labels for conditional survival plots
  csXlab <- reactive({csMetadata()$xLabel[1]})
  csYlab <- reactive({csMetadata()$yLabel[1]})
  csPlotTitle <- reactive({csMetadata()$title4Plot[1]})
  csLegendTitle <-reactive({csMetadata()$title4Legend[1]}) 
  output$csText4Figure <- renderText(csMetadata()$text4Figure[1])
  
  # # update input labels for prognosis groups
  # csGroup1Name <-reactive({csMetadata()$group1Name[1]})
  # csGroup2Name <- reactive({csMetadata()$group2Name[1]})
  # csCgroupChoicesN <- reactive({
  #     c(csGroup1Name(), csGroup2Name()) %>% 
  #       factor() %>% 
  #       levels()
  #   })
  # csCgroupChoicesV <- reactive({
  #   c(csGroup1Name(), csGroup2Name()) %>% 
  #       factor() %>% 
  #       levels()
  #     })
  # 
  # observe(
  #   updateCheckboxGroupInput(session, "csShowGroups", choiceNames = csCgroupChoicesN(), choiceValues = csCgroupChoicesV())
  # )
  
  # plot conditional survival
  output$csPlot <- renderPlotly({
    ggplotly(
    pplot(csData(), csPlotTitle(), csXlab(), csYlab(), csLegendTitle(), showCI(), showBW(), input$facetWrap, ncol = 2L)  
    , tooltip = c("x", "y")  )
    })
}
