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
  

   session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  
   # check validity
   isValid_num <- eventReactive(input$GoButton,{
    if(!is.null(input$prevalence) & input$prevalence >= 0 & input$prevalence <= 1 &
     !is.null(input$sensitivity) & input$sensitivity >= 0 && input$sensitivity <= 1  &
     !is.null(input$specificity) & input$specificity >= 0 && input$specificity <= 1  &
     !is.null(input$RuleInDecisionThreshold) & input$RuleInDecisionThreshold >= 0 & input$RuleInDecisionThreshold <= 1 &
     !is.null(input$RuleInDecisionThreshold) & input$RuleInDecisionThreshold >= 0 & input$RuleInDecisionThreshold <= 1) {
      return (TRUE)
      } else {
        return(FALSE)
      }
     
   },ignoreNULL = FALSE)
 
#==========================================================
   observeEvent(input$GoButton, {
   output$validtext  <- renderText({
     if(!isValid_num()){
       print("Inputs not valid, please check that the values for
             prevalence, sensitivity, specificity and rule in/out decision thresholds specified, lie between 0 and 1. ")
     } else {
       return(NULL)
     }
     
     
   })
   })
    
  #  output$linesTable <- renderDataTable(linesDf(), 
  #                                       options = list(scrollX = TRUE, rownames = FALSE,
  #                                       dom = 't'))
    
#==========================================================
    
    observeEvent(input$GoButton, {
          output$RuleInOutPlot2<-renderPlot({
        # Sys.sleep(2)
        if(isValid_num()){
        isolate(ruleinoutplot(input$n, input$prevalence, input$sensitivity, input$specificity,
                      input$RuleInDecisionThreshold, input$RuleOutDecisionThreshold, 
                      input$DxCondition, input$DxTestName,  
                      input$DxRuleInDecision, input$DxRuleOutDecision, input$IndeterminateDecision, input$disper))
        }
      })
   }, ignoreNULL = FALSE)
    
#==========================================================
  
  observeEvent(input$GoButton, {
    output$PrePostProb2<-renderPlot({
      # Sys.sleep(2)
      if(isValid_num()){
      isolate(prepostprobplot(input$n, input$prevalence, input$sensitivity, input$specificity,
                      input$DxCondition, input$DxTestName, input$disper))
      }
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
