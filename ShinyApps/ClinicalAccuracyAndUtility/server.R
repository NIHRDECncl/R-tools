###### to trace the execution of reactives at runtime 
###### 
###### 1. at the R console run: options(shiny.reactlog=TRUE) 
###### 2. start the Shiny app
###### 3. run the trace with command-F3
###### 4. step through with -> arrow
###### 
###### https://shiny.rstudio.com/articles/debugging.html

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


shinyServer (
   function(input, output, session) {
  

   session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  
   # check validity
   # isValid_num <- eventReactive(input$GoButton,{
   #  if(!is.null(input$prevalence) & input$prevalence >= 0 & input$prevalence <= 1 &
   #   !is.null(input$sensitivity) & input$sensitivity >= 0 && input$sensitivity <= 1  &
   #   !is.null(input$specificity) & input$specificity >= 0 && input$specificity <= 1  &
   #   !is.null(input$RuleInDecisionThreshold) & input$RuleInDecisionThreshold >= 0 & input$RuleInDecisionThreshold <= 1 &
   #   !is.null(input$RuleInDecisionThreshold) & input$RuleInDecisionThreshold >= 0 & input$RuleInDecisionThreshold <= 1) {
   #    return (TRUE)
   #    } else {
   #      return(FALSE)
   #    }
   #   
   # },ignoreNULL = FALSE)
 
#==========================================================
   # observeEvent(input$GoButton, {
   # output$validtext  <- renderText({
   #   if(!isValid_num()){
   #     print("Inputs not valid, please check that the values for
   #           prevalence, sensitivity, specificity and rule in/out decision thresholds specified, lie between 0 and 1. ")
   #   } else {
   # #     return(NULL)
   # #   }
   #   
   #   
   # })
   # # })
    
  #  output$linesTable <- renderDataTable(linesDf(), 
  #                                       options = list(scrollX = TRUE, rownames = FALSE,
  #                                       dom = 't'))

   #==========================================================
     
     # graph 0: RuleInOutPlot0
     

   observeEvent(input$GoButton, {
     # print(input$GoButton)

#  ------------>>>>>>  I removed isolate() because (i) it doesn't work, and (ii) the manual says that observeEvent performs thew functionof isolate()
     # https://shiny.rstudio.com/reference/shiny/latest/observeEvent.html

     output$RuleInOutPlot0 <- renderPlot({
       # DxStats(input$n, input$prevalence, input$sensitivity, input$specificity, plot2x2 = TRUE)$barplot[[1]]
       DxStats(300, input$prevalence, .9, .85, plot2x2 = TRUE)$barplot[[1]]
     })
   },   ignoreNULL = FALSE, ignoreInit = FALSE)

     # 
     # 
     #  # graph 1: true and false postives; false and true negatives
     # observeEvent(input$GoButton, {
     #   # isolate({
     # 
     #   output$PrePostProb2<-renderPlot({
     #      prepostprobplot(input$n, input$prevalence, input$sensitivity, input$specificity,
     #                               input$DxCondition, input$DxTestName, input$disper)
     #     })
     #   # })  close isolate()
     #  },   ignoreNULL = FALSE)
     # 
     # 
   # # graph 2: decision thresholds comopared to posterior probabailities
   # observeEvent(input$GoButton, {
   #     output$RuleInOutPlot2<-renderPlot({
   #      ruleinoutplot(input$n, input$prevalence, input$sensitivity, input$specificity,
   #                             input$RuleInDecisionThreshold, input$RuleOutDecisionThreshold, 
   #                             input$DxCondition, input$DxTestName,  
   #                             input$DxRuleInDecision, input$DxRuleOutDecision, input$IndeterminateDecision, input$disper)
   #     })
   #  },   ignoreNULL = FALSE)
   
   
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
   })


