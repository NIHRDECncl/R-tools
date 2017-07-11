################# server for ShinyApp to explore clinical accuracy and clinical utility    ################

server <- function(input, output, session) {
  
   session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

  }
