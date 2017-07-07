################# server for ShinyApp to explore clinical accuracy and clinical utility    ################

server <- function(input, output, session) {
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  observeEvent("", {
    wd <- getwd()
    path <- paste0(getwd(), "/www/ConditionalSurvival.xlsx")
    sheets <- excel_sheets(path)
    conditions <- read_excel(path, sheets[1])
    survivalData <- read_excel(path, sheets[2])
    output$conditions <- renderTable(conditions)
    output$survivalData <- renderTable(survivalData)
  }, ignoreNULL = FALSE)
  
}
