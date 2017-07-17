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
  
   
    
   observeEvent(input$GoButton, {
     cache$n <- input$n
     cache$prevalence <- input$prevalence
     cache$sensitivity <- input$sensitivity
     cache$specificity <- input$specificity
    # cache$GoButton <- input$GoButton
     cache$RuleInDecisionThreshold <- input$RuleInDecisionThreshold
     cache$RuleOutDecisionThreshold <-  input$RuleOutDecisionThreshold
     cache$DxCondition <- input$DxCondition
     cache$DxTestName <- input$DxTestName
     cache$DxRuleInDecision <- input$DxRuleInDecision
     cache$DxRuleOutDecision <- input$DxRuleOutDecision 
     cache$IndeterminateDecision <- input$IndeterminateDecision
     }
   )
   
  
    Dpos <- reactive({round(cache$n * cache$prevalence)})
    Dneg <- reactive({round(cache$n - Dpos())})

    Tp <- reactive({cache$sensitivity * Dpos()})
    Tn <- reactive({cache$specificity * Dneg()})

    Fn <- reactive({(1 - cache$sensitivity) * Dpos()})
    Fp <- reactive({(1 - cache$specificity) * Dneg()})

    PPV <- reactive({Tp()/(Tp() + Fp())})
    NPV <- reactive({Tn()/(Tn() + Fn())})

    LRp <- reactive({(cache$sensitivity/(1 - cache$specificity))})
    LRn <- reactive({(1 - cache$sensitivity)/(cache$specificity)})
    
    PreTestOddsP <- reactive({(cache$prevalence)/(1 - cache$prevalence)})
    PreTestOddsN <- reactive({(1 - cache$prevalence)/(cache$prevalence)})

    PostTestOddsP <- reactive({PreTestOddsP()*LRp()})
    PostTestOddsN <- reactive({PreTestOddsN()*LRn()})

    PostTestProbP <- reactive({PostTestOddsP()/(PostTestOddsP() + 1)})
    PostTestProbN <- reactive({PostTestOddsN()/(PostTestOddsN() + 1)})

    

    ### confidence  limits for the post=test probabilities
    
    ### the following are numbers; the same names in the data.frame label 2 item columns (vectors)
    
    TPY_ciL <- reactive({DxStats(cache$n, cache$prevalence, cache$sensitivity, cache$specificity)$TPY_ciL})
    TPY_ciU <- reactive({DxStats(cache$n, cache$prevalence, cache$sensitivity, cache$specificity)$TPY_ciU})
    TNY_ciL <- reactive({DxStats(cache$n, cache$prevalence, cache$sensitivity, cache$specificity)$TNY_ciL})
    TNY_ciU <- reactive({DxStats(cache$n, cache$prevalence, cache$sensitivity, cache$specificity)$TNY_ciU})
 
#==========================================================
    
    
    output$linesTable <- renderDataTable(linesDf(), 
                                         options = list(scrollX = TRUE, rownames = FALSE,
                                         dom = 't'))
    
#==========================================================
    
    observeEvent(input$GoButton, {
          output$RuleInOutPlot2<-renderPlot({
        # Sys.sleep(2)
        
        ruleinoutplot(cache$n, cache$prevalence, cache$sensitivity, cache$specificity,
                      cache$RuleInDecisionThreshold, cache$RuleOutDecisionThreshold, 
                      cache$DxCondition, cache$DxTestName,  
                      cache$DxRuleInDecision, cache$DxRuleOutDecision, cache$IndeterminateDecision)
      })
    })
    
#==========================================================
  
  observeEvent(input$GoButton, {
    output$PrePostProb2<-renderPlot({
      # Sys.sleep(2)
      
      prepostprobplot(cache$n, cache$prevalence, cache$sensitivity, cache$specificity,
                    cache$DxCondition, cache$DxTestName)
    })
  })
  
  
  
  # test
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
  
  #}

  
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
                      Word = word_document(),  envir = cache)
      )
      file.copy(out, file)
    },
    contentType = "text/plain"
  )

  
   }

)
