

wd <- "H:/Jallens_homearea_DEC/Calculators/R/R-tools/ShinyApps/ImpRefApp/"
setwd(wd)


library(shiny)
ui <- pageWithSidebar(
 
  headerPanel("Click the button"),
  sidebarPanel(
    sliderInput("obs", "Number of observations:",
                min = 0, max = 1000, value = 500),
    actionButton("goButton1", "Go1!"),
    actionButton("goButton2", "Go2!")
  ),
  
  tabPanelh3("Download summary report"),
           p("This document contains all the tables and figures generated from the 
             SAVI analysis of your PSA."),
           radioButtons('format', 'Please select the document format you require', 
                        c('PDF', 'HTML', 'Word'),
                        inline = TRUE),
           downloadButton('downloadReport', 'Download summary report'),
           br(), br(), 
           p("NB generating the document can take some time.")
           ),
   
  mainPanel(
    plotOutput("distPlot")
  )
)

server <- function(input, output) {
  dist <- eventReactive(input$goButton1 | input$goButton2, {
    set.seed(123)
    data.frame(
      dist = rnorm(input$obs)
    )
    }, ignoreNULL = FALSE )

  # this looks rather roundabout, but it is meant to be a minimal representative example of the 
  #    clini cal accuracy and utility app, which does not show plots until the actionButton is clicked
  output$distPlot <- renderPlot({
    hist(dist()$dist) 
  })
  
  
  output$downloadReport <- downloadHandler(
    filename = function() {#"my-report.pdf"
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },
    
    content = function(file) {
      src <- normalizePath('report.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite=TRUE)
      
      library(rmarkdown)
      out <- render(input = 'report.Rmd', #pdf_document()
                    output_format = switch(
                      input$format,
                      PDF = pdf_document(), HTML = html_document(), 
                      Word = word_document())#,
               #     envir = cache
      )
      file.copy(out, file)
    },
    contentType = "text/plain"
  )
  
  
}

shinyApp(ui = ui, server = server)

