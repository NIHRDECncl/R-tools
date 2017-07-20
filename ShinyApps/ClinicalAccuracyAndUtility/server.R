################# server for ShinyApp to explore clinical accuracy and clinical utility    ################

# inputs
#
# DxCondition
# DxTestName
# prevalence
# n
# sensitivity
# specificity
# DxRuleInDecision
# RuleInDecisionThreshold
# IndeterminateDecision
# DxRuleOutDecision
# RuleOutDecisionThreshold

source("global.R")

shinyServer (
   function(input, output, session) {
  
  #####################################
  # CREATE NEW ENVIRONMENT 'cache'    #
  # Initialise cached variable values #
  #####################################
  
  # `cache' is the environment unique to each user visit
  # This is where we will save values that need to persist, 
  # and that can be picked up and included in the report
  
  if(exists("cache")) rm(cache, inherits = TRUE) # we shouldn't need this
  cache <- new.env()
  

   session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  

 
#==========================================================
    
    
    output$linesTable <- renderDataTable(linesDf(), 
                                         options = list(scrollX = TRUE, rownames = FALSE,
                                         dom = 't'))
    
#==========================================================
    
    observeEvent(input$GoButton, {
          output$RuleInOutPlot2<-renderPlot({
        # Sys.sleep(2)
        
        isolate(ruleinoutplot(input$n, input$prevalence, input$sensitivity, input$specificity,
                      input$RuleInDecisionThreshold, input$RuleOutDecisionThreshold, 
                      input$DxCondition, input$DxTestName,  
                      input$DxRuleInDecision, input$DxRuleOutDecision, input$IndeterminateDecision))
      })
   }, ignoreNULL = FALSE)
    
#==========================================================
  
  observeEvent(input$GoButton, {
    output$PrePostProb2<-renderPlot({
      # Sys.sleep(2)
      
      isolate(prepostprobplot(input$n, input$prevalence, input$sensitivity, input$specificity,
                      input$DxCondition, input$DxTestName))
    })
  }, ignoreNULL = FALSE)
    
  

  
  ## Thanks to Mark Strong for this code
  # https://github.com/Sheffield-Accelerated-VoI/SAVI/blob/master/server.R
  
  output$downloadReport <- downloadHandler(
    filename = function() {  #"my-report.pdf"
      paste('report', sep = '.', switch(
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
                      Word = word_document())
      )
      file.copy(out, file)
    },
    contentType = "text/plain"
  )

  
   }

)
