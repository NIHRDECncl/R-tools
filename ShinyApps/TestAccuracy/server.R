source("global.R")


shinyServer (
  function(input, output, session) {
    
    session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  
    
    # validity check add in 
    # check validity
    isValid_num <- eventReactive(input$GoButton,{
      if(!is.null(input$prevalence) & input$prevalence >= 0 & input$prevalence <= 1 &
         !is.null(input$sensitivity) & input$sensitivity >= 0 && input$sensitivity <= 1  &
         !is.null(input$specificity) & input$specificity >= 0 && input$specificity <= 1  &
         !is.null(input$n) & input$n >= 0 )
         {
        return (TRUE)
      } else {
        return(FALSE)
      }
      
    },ignoreNULL = FALSE)
    
    
    observeEvent(input$GoButton, {
      output$validtext  <- renderText({
        if(!isValid_num()){
          print("Inputs not valid, please check that the values for
             prevalence, sensitivity and specificity specified, lie between 0 and 1. ")
        } else {
          return(NULL)
        }
        
        
      })
    })

    
    observeEvent(input$GoButton, {
      output$populationPlot<-renderPlot({
        if(isValid_num()){
          isolate(popplot(input$n, input$prevalence, input$sensitivity, input$specificity, 
                          input$sorted, input$ciFlag))
        }
      })
    }, ignoreNULL = FALSE)
    
   
      output$plot1title  <- renderText({
          "Population: people with and without the condition."
      })
  
    
    observeEvent(input$GoButton, {
      output$testedPlots<-renderPlot({
        if(isValid_num()){
          isolate(popplot2(input$n, input$prevalence, input$sensitivity, input$specificity, 
                          input$sorted, input$ciFlag))
        }
      })
    }, ignoreNULL = FALSE)
    
   
 
      output$plot2title  <- renderText({
       "Test accuracy: true and false positives; false \n and true negatives."
      })
 
      output$plot3title  <- renderText({
       "Distribution of index test results: true and false positives; false and true negatives.s"
      })
      
      
      output$distributiontext <- renderText({
        "     For any diagnostic test, there is often a trade-off 
              between sensitivity and specificity. This is a natural consequence 
              of the continuous nature of the outcome of the test result 
              (e.g. the biomarker level present in the blood) 
              and the dichotomous nature of the interpretation of the 
              test result (i.e. positive or negative). The trade-off 
              between maximising sensitivity and maximising specificity 
              (or, equivalently, minimising FN or minimise FP) 
              is what decides the threshold biomarker level during test development. \n
              \n

              A sensitivity and specificity value can be assigned to any 
              given threshold. These values are then plotted graphically 
              to produce a Receiver Operator Characteristic (ROC). 
              In a ROC curve the sensitivity (true positive rate) is 
              plotted against the false positive rate (1- specificity).  
              "
        
        
      })
      
      
    observeEvent(input$GoButton, {
    output$distributionplots <- renderPlot({
       isolate(distributionplots(input$n, input$prevalence, input$sensitivity, input$specificity))
    })
    }, ignoreNULL = FALSE)
    
    
    output$table1title  <- renderText({
      ("2 x 2 contingency table")
    })
    
  
    
    observeEvent(input$GoButton, {
        output$dx2x2Table <- renderTable({
          isolate(dx2x2Table(input$n, input$prevalence, input$sensitivity, input$specificity))
          }, digits = 0)
    
         }, ignoreNULL = FALSE)
    
    output$table2title  <- renderText({
      ("Test accuracy statistics")
    })
    
    observeEvent(input$GoButton, {
        output$pvdf <- renderTable({
          isolate(pvdf(input$n, input$prevalence, input$sensitivity, input$specificity))
        }, digits = 0, rownames = TRUE)
    
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
    
    
    
    
  })
