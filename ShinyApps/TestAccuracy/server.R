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
    
     # observeEvent(input$GoButton, {
     #   updateTabsetPanel(session, "inTabset")#,
     #                   #  selected = paste0("panel", input$GoButton)
     #   
     # }, ignoreNULL = FALSE)
    
 
    
    
    observeEvent(input$GoButton, {
      output$populationPlot<-renderPlot({
        if(isValid_num()){
          isolate(popplot(input$n, input$prevalence, input$sensitivity, input$specificity, 
                          input$sorted, input$ciFlag))
        }
      })
    }, ignoreNULL = FALSE)
    
    
    
    
    observeEvent(input$GoButton, {
      output$testedPlots<-renderPlot({
        if(isValid_num()){
          isolate(popplot2(input$n, input$prevalence, input$sensitivity, input$specificity, 
                          input$sorted, input$ciFlag))
        }
      })
    }, ignoreNULL = FALSE)
    
   
    
    
    observeEvent(input$GoButton, {
    output$distributionplots <- renderPlot({
       isolate(distributionplots(input$n, input$prevalence, input$sensitivity, input$specificity))
    })
    }, ignoreNULL = FALSE)
    
    observeEvent(input$GoButton, {
        output$dx2x2Table <- renderTable({
          isolate(dx2x2Table(input$n, input$prevalence, input$sensitivity, input$specificity))
          }, digits = 0)
    
         }, ignoreNULL = FALSE)
    
    observeEvent(input$GoButton, {
        output$pvdf <- renderTable({
          isolate(pvdf(input$n, input$prevalence, input$sensitivity, input$specificity))
        }, digits = 0)
    
       }, ignoreNULL = FALSE)
    
     
    
    
    
  })
