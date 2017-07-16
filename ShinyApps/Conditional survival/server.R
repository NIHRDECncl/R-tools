################# server for ShinyApp to explore survival aand conditional survival
server <- function(input, output, session) {
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  observeEvent("", {
    wd <- getwd()
    path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
    sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
    names(sheets) <- "sheets"
    plotsMetadata <- read_excel(path, sheets$sheets[1])
    plotsData <- read_excel(path, sheets$sheets[2])

    output$sheets <- renderDataTable(sheets)
    output$plotsMetadata <- renderDataTable(plotsMetadata)
    output$plotsData <- renderDataTable(plotsData)

    observe({
      c <- plotsMetadata$Condition %>% 
          sort %>% 
            unique
  updateSelectInput(session, "condition", label = NULL, choices = c)
    })
    
observe({
    p <- subset(plotsMetadata, Condition == input$condition & View == "Prognosis")$PlotName %>% 
          sort %>% 
            unique
    updateSelectInput(session, "prognosisPlot", label = NULL, choices = p)
    })
    
observe({
    cs <- subset(plotsMetadata, Condition == input$condition & View == "Conditional survival")$PlotName %>% 
          sort %>% 
            unique
  updateSelectInput(session, "conditionalSurvivalPlot", label = NULL, choices = cs)
      })

  }, ignoreNULL = FALSE)

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
