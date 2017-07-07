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

shinyServer <- function(input, output, session) {
  
   # `cache' is the environment unique to each user visit
   # This is where we will save values that need to persist, 
   # and that can be picked up and included in the report
  
   if(exists("cache")) rm(cache, inherits = TRUE) # we shouldn't need this
   cache <- new.env()
  
   session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it
  
#   values <- reactiveValues()
#   go <- function() {
#     values$GoButton <- isolate(values$GoButton) + 1
#   }
#   observe({
#     if (input$GoButton == 0) isolate( go())
#   })
#  
#   
    Dpos <- eventReactive(input$GoButton, {round(input$n * input$prevalence)}, ignoreNULL = FALSE, ignoreInit = FALSE)
    Dneg <- eventReactive(input$GoButton, {round(input$n - Dpos())}, ignoreNULL = FALSE, ignoreInit = FALSE)

    Tp <- eventReactive(input$GoButton, {input$sensitivity * Dpos()}, ignoreNULL = FALSE, ignoreInit = FALSE)
    Tn <- eventReactive(input$GoButton, {input$specificity * Dneg()}, ignoreNULL = FALSE,ignoreInit = FALSE)

    Fn <- eventReactive(input$GoButton, {(1 - input$sensitivity) * Dpos()}, ignoreNULL = FALSE, ignoreInit = FALSE)
    Fp <- eventReactive(input$GoButton, {(1 - input$specificity) * Dneg()}, ignoreNULL = FALSE, ignoreInit = FALSE)

    PPV <- eventReactive(input$GoButton, {Tp()/(Tp() + Fp())}, ignoreNULL = FALSE, ignoreInit = FALSE)
    NPV <- eventReactive(input$GoButton, {Tn()/(Tn() + Fn())}, ignoreNULL = FALSE, ignoreInit = FALSE)

    LRp <- eventReactive(input$GoButton, {(input$sensitivity/(1 - input$specificity))}, ignoreNULL = FALSE, ignoreInit = FALSE)
    LRn <- eventReactive(input$GoButton, {(1 - input$sensitivity)/(input$specificity)}, ignoreNULL = FALSE, ignoreInit = FALSE)
    
    PreTestOddsP <- eventReactive(input$GoButton, {(input$prevalence)/(1 - input$prevalence)}, ignoreNULL = FALSE, ignoreInit = FALSE)
    PreTestOddsN <- eventReactive(input$GoButton, {(1 - input$prevalence)/(input$prevalence)}, ignoreNULL = FALSE, ignoreInit = FALSE)

    PostTestOddsP <- eventReactive(input$GoButton, {PreTestOddsP()*LRp()}, ignoreNULL = FALSE, ignoreInit = FALSE)
    PostTestOddsN <- eventReactive(input$GoButton, {PreTestOddsN()*LRn()}, ignoreNULL = FALSE, ignoreInit = FALSE)

    PostTestProbP <- eventReactive(input$GoButton, {PostTestOddsP()/(PostTestOddsP() + 1)}, ignoreNULL = FALSE, ignoreInit = FALSE)
    PostTestProbN <- eventReactive(input$GoButton, {PostTestOddsN()/(PostTestOddsN() + 1)}, ignoreNULL = FALSE, ignoreInit = FALSE)

    ### confidence  limits for the post=test probabilities
    
    ### the following are numbers; the same names in the data.frame label 2 item columns (vectors)
    
    TPY_ciL <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TPY_ciL}, ignoreNULL = FALSE, ignoreInit = FALSE)
    TPY_ciU <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TPY_ciU}, ignoreNULL = FALSE, ignoreInit = FALSE)
    TNY_ciL <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TNY_ciL}, ignoreNULL = FALSE, ignoreInit = FALSE)
    TNY_ciU <- eventReactive(input$GoButton, {DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)$TNY_ciU}, ignoreNULL = FALSE, ignoreInit = FALSE)
  
    # lines for plot 1
    linesDf <- eventReactive(input$GoButton, {
      ### save stats so don't have to call many times
      Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity) 
      
      return({data.frame(
        PriorAxisX = c(0, 0),
        PriorAxisY = c(0, 1),
        
        PostAxisX = c(1, 1),
        PostAxisY = c(0, 1),
        
        PrevX = c(0, 1),
        PrevY = c(input$prevalence, input$prevalence),
        
        RuleInDecisionThresholdX = c(0, 1),
        RuleInDecisionThresholdY = c(input$RuleInDecisionThreshold, input$RuleInDecisionThreshold),
        
        RuleOutDecisionThresholdX = c(0, 1),
        RuleOutDecisionThresholdY = c(input$RuleOutDecisionThreshold, input$RuleOutDecisionThreshold),
        
        TestPosX = c(0, 1),
        TestPosY = c(input$prevalence, Dx$PostTestProbP),
        
        TestNegX = c(0, 1),
        TestNegY = c(input$prevalence, round(Dx$PostTestProbN,3)), # post -ve test probability
        
        TPY_ciL = c(input$prevalence, round(Dx$TPY_ciL,3)),
        TPY_ciU = c(input$prevalence, round(Dx$TPY_ciU,3)),
        
        TNY_ciL = c(input$prevalence, round(Dx$TNY_ciL,3)),
        TNY_ciU = c(input$prevalence, round(Dx$TNY_ciU,3))
      )})
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
### coordinates and labels for plot 1
    fixedlabels <- eventReactive(input$GoButton, {
      data.frame(
        x = c(0.35, 0.85, 0.35, 0.35),
        y = c(
          input$RuleInDecisionThreshold + 0.05, 
          input$prevalence + 0.05, 
          input$RuleOutDecisionThreshold + 0.05,
          input$RuleInDecisionThreshold - 0.05
        ),
        labels = c(
          paste0(input$RuleInDecisionThreshold*100, "%  = threshold for rule-in decision: ", input$DxRuleInDecision),
          paste0("Prevalence = ", input$prevalence*100, "%"),
          paste0(input$RuleOutDecisionThreshold*100, "%  = threshold for rule-out decision: ", input$DxRuleOutDecision),
          paste0("Action when indeterminate: ", input$IndeterminateDecision)),
          fillColours = c("firebrick4", "springgreen4")
       )
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

    postTestLabels <- eventReactive(input$GoButton, {
      ### save stats so don't have to call many times
      Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)
      Nudge <- 0.02
      if (Dx$TPY_ciU - Dx$TPY_ciL < 3*Nudge) NudgeCIp = + Nudge else NudgeCIp = - Nudge/3
      if (Dx$TNY_ciU - Dx$TNY_ciL < 3*Nudge) NudgeCIn = + Nudge else NudgeCIn = - Nudge/3
      data.frame(
        x = c(0.85, 0.85, 1.05, 1.05, 1.05, 1.05),
        y = c(
          Dx$PostTestProbP + 0.025, # = post +ve test probability 
          Dx$PostTestProbN - 0.025, # = post -ve test probability
          Dx$TPY_ciL - NudgeCIp,
          Dx$TPY_ciU + NudgeCIp,
          Dx$TNY_ciL - NudgeCIn,
          Dx$TNY_ciU + NudgeCIp),
        
        labels = c(
          strwrap(paste0("Prob post +ve test = ", round(Dx$PostTestProbP*100), "%"), 40),
          strwrap(paste0("Prob post -ve test = ", round(Dx$PostTestProbN*100), "%"), 40),
          paste0(round(Dx$TPY_ciL * 100), "%"),
          paste0(round(Dx$TPY_ciU * 100), "%"),
          paste0(round(Dx$TNY_ciL * 10000)/100, "%"),
          paste0(round(Dx$TNY_ciU * 10000)/100, "%")
          # "1", "2", "3", "4"
        )
      )
    
    }, ignoreNULL = FALSE, ignoreInit = FALSE)
    
#==========================================================
    
    
    output$linesTable <- renderDataTable(linesDf(), 
                                         options = list(scrollX = TRUE, rownames = FALSE,
                                         dom = 't'))
    
    output$RuleInOutPlot<-renderPlot({
      # Sys.sleep(2)
      ggplot(linesDf()) +
        geom_line(aes(x = linesDf()$PriorAxisX, y = linesDf()$PriorAxisY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$PostAxisX, y = linesDf()$PostAxisY), data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$PrevX, y = linesDf()$PrevY, colour="coral1"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$RuleInDecisionThresholdX, y = linesDf()$RuleInDecisionThresholdY, colour="cadetblue"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$RuleOutDecisionThresholdX, y = linesDf()$RuleOutDecisionThresholdY, colour="springgreen4"), size = 1.5, data = linesDf(), stat = "identity", position = "identity") +
        geom_line(aes(x = linesDf()$TestPosX, y = linesDf()$TestPosY), size = 1.15, data = linesDf(), stat = "identity", position = "identity", colour = "firebrick4") +
        geom_ribbon(data = linesDf(), aes(x = linesDf()$TestPosX, ymin = linesDf()$TPY_ciL, ymax = linesDf()$TPY_ciU, alpha = 0.03), fill  = "lightsalmon") +
        geom_line(aes(x = linesDf()$TestNegX, y = linesDf()$TestNegY), size = 1.15, data = linesDf(), stat = "identity", position = "identity") +
        geom_ribbon(data = linesDf(), aes(x = linesDf()$TestNegX, ymin = linesDf()$TNY_ciL, ymax = linesDf()$TNY_ciU, alpha = 0.03), fill  = "darkseagreen3") +
        theme(
          axis.text.x = element_blank(),
          legend.position="none"
        ) +
        labs(x = "", y = paste0("probability of ", input$DxCondition)) +
        ggtitle(paste("Post-test probabilities after", input$DxTestName, "for", input$DxCondition)) +
        theme(plot.title = element_text(size = 14, face = "bold")) +
        geom_text(data = fixedlabels(), size = 4, aes(x,y,label = labels)) + 
        geom_text(data = postTestLabels(), size = 3, aes(x, y, label = labels))
    })

    graphPre2PostProb <- eventReactive(input$GoButton, {
      x <- seq(from = 0, to = 1, by = 0.01) ### preTest probability along the x-axis
      ### y-axis for post test probability
      ## initialize variables for post test probabilities
      
      yPciL <- x
      yP <- x
      yPciU <- x
      
      yNciL <- x
      yN <- x
      yNciU <- x
      
      for (i in seq_along(x)) {
        Dx <- DxStats(input$n, x[[i]], input$sensitivity, input$specificity) 
      
        yPciL[[i]] <- Dx$TPY_ciL
        yP[[i]]    <- Dx$PostTestProbP
        yPciU[[i]] <- Dx$TPY_ciU
        
        yNciL[[i]] <- Dx$TNY_ciL
        yN[[i]]    <- Dx$PostTestProbN
        yNciU[[i]] <- Dx$TNY_ciU
      }
      
      data.frame(
        x = x,
        yPciL = yPciL,
        yP = yP,
        yPciU = yPciU,
        
        yNciL = yNciL,
        yN = yN,
        yNciU = yNciU
      )
    }, ignoreNULL = FALSE, ignoreInit = FALSE)

  linesPre2PostProb <- eventReactive(input$GoButton, {
    Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)
    data.frame(
      prevalenceX     = c(input$prevalence, input$prevalence),
      prevalenceY     = c(0, Dx$PostTestProbP),
      
      PostProbPosX    = c(0, input$prevalence),
      PostProbPosYciL = c(Dx$TPY_ciL, Dx$TPY_ciL),
      PostProbPosY    = c(Dx$PostTestProbP, Dx$PostTestProbP),
      PostProbPosYciU = c(Dx$TPY_ciU, Dx$TPY_ciU),

      PostProbNegX    = c(0, input$prevalence),
      PostProbNegYciL = c(Dx$TNY_ciL, Dx$TNY_ciL),
      PostProbNegY    = c(Dx$PostTestProbN, Dx$PostTestProbN),
      PostProbNegYciU = c(Dx$TNY_ciU, Dx$TNY_ciU)
    )
  }, ignoreNULL = FALSE, ignoreInit = FALSE)

  prepostLabels <- eventReactive(input$GoButton, {
    ### save stats so don't have to call many times
    Dx <- DxStats(input$n, input$prevalence, input$sensitivity, input$specificity)
    data.frame(
      x = c(0.1, 0.1, input$prevalence),
      y = c(
        Dx$PostTestProbP + 0.035, 
        Dx$PostTestProbN - 0.035,
        0.10),
      
      labels = c(
        strwrap(paste0("Prob post +ve test = ", round(Dx$PostTestProbP*100), "%"), 40),
        strwrap(paste0("Prob post -ve test = ", round(Dx$PostTestProbN*100), "%"), 40),
        paste0("Prevalence = ", round(input$prevalence * 100), "%")
      )
    )
    
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  
  
  output$PrePostProb <- renderPlot({
 # output$PrePostProb <- eventReactive(input$GoButton, {
    ggplot(graphPre2PostProb()) +
      geom_line(aes(x = graphPre2PostProb()$x, y = graphPre2PostProb()$yP), stat = "identity", position = "identity") +
      geom_ribbon(data = graphPre2PostProb(),
                  aes(x = graphPre2PostProb()$x,
                  ymin = graphPre2PostProb()$yPciL,
                  ymax = graphPre2PostProb()$yPciU,
                  alpha = 0.03), fill  = "lightsalmon") +

      geom_line(aes(x = graphPre2PostProb()$x, y = graphPre2PostProb()$yN), data = graphPre2PostProb(), stat = "identity", position = "identity") +
      geom_ribbon(data = graphPre2PostProb(),
                  aes(x = graphPre2PostProb()$x,
                  ymin = graphPre2PostProb()$yNciL,
                  ymax = graphPre2PostProb()$yNciU,
                  alpha = 0.03), fill  = "darkseagreen3") +
      theme(legend.position="none") +
      labs(x = "Pre-test probability (prevalence)", y = paste0("Post test probability after ", input$DxTestName)) +
      ggtitle(paste("Pre- and post-test probabilities after", input$DxTestName, "for", input$DxCondition)) +
      theme(plot.title = element_text(size = 12, face = "bold")) +


      geom_line(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$prevalenceX,
        y = linesPre2PostProb()$prevalenceY), stat = "identity", position = "identity") +

      geom_line(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$PostProbPosX,
        y = linesPre2PostProb()$PostProbPosY), stat = "identity", position = "identity") +
      geom_ribbon(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$PostProbPosX,
        ymin = linesPre2PostProb()$PostProbPosYciL,
        ymax = linesPre2PostProb()$PostProbPosYciU, alpha = 0.03), fill  = "lightsalmon") +

      geom_line(data = linesPre2PostProb(), aes(x = linesPre2PostProb()$PostProbNegX, y = linesPre2PostProb()$PostProbNegY), stat = "identity", position = "identity") +
      geom_ribbon(data = linesPre2PostProb(), aes(
        x = linesPre2PostProb()$PostProbNegX,
        ymin = linesPre2PostProb()$PostProbNegYciL,
        ymax = linesPre2PostProb()$PostProbNegYciU, alpha = 0.03), fill  = "darkseagreen3") +
      

    geom_text(data = prepostLabels(), size = 4, aes(x,y,label = labels)) 
      
  })
  
  ## Thanks to Mark Strong for this code
  # https://github.com/Sheffield-Accelerated-VoI/SAVI/blob/master/server.R
  
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
                      Word = word_document())
                    #  params = list(plot1 = PrePostProb )
                    #     envir = cache
      )
      file.copy(out, file)
    },
    contentType = "text/plain"
  )
  
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
  
  }
