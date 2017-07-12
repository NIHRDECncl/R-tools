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
    
  }, ignoreNULL = FALSE)
  
}
