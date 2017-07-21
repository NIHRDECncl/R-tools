################# server for ShinyApp to explore survival aand conditional survival
server <- function(input, output, session) {
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  observeEvent("", {
    wd <- getwd()
    path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
    sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
    names(sheets) <- "sheets"
    conditions <- read_excel(path, sheets$sheets[1])
    survivalData <- read_excel(path, sheets$sheets[2])
    viewpoints <- read_excel(path, sheets$sheets[3])
    
    output$sheets <- renderDataTable(sheets)
    output$conditions <- renderDataTable(conditions)
    output$survivalData <- renderDataTable(survivalData)
    output$viewpoints <- renderDataTable(viewpoints)
    
  c <- conditions$Condition %>% 
          sort %>% 
            unique
  updateSelectInput(session, "condition", label = NULL, choices = c)
  
observe({
    o <- subset(conditions, Condition == input$condition)$Outcome %>% 
          sort %>% 
            unique
    updateSelectInput(session, "outcome", label = NULL, choices = o)
    })
    
observe({
    g <- subset(conditions, Condition == input$condition & Outcome == input$outcome)$Group %>% 
          sort %>% 
            unique
  updateSelectInput(session, "group", label = NULL, choices = g)
      })

  }, ignoreNULL = FALSE)
  
}
