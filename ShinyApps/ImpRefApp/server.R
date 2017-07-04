# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

shinyServer <- function(input, output, session) {
  
  session$onSessionEnded(stopApp) # it can be annoying that when you close the browser window, the app is still running and you need to manually press “Esc” to kill it

 
  
  # inputs from ui:   
  #   input$Title = "title for tables and graphs"
  #   input$IndexTest = "name of index test"
  #   input$ReferenceTest = "name of reference test"
  # 
  #   input$gPrevalence --- true prevalence
  #   input$iPopulation --- population in the ir (Index cf Reference test) contingency matrix
  #  
  #   input$irSen
  #   input$irSpec
  #  
  #   input$rgSen
  #   input$rgSpec
  # 
  #   input$igSen
  #   input$igSpec
  #
  igDxAcc <-rgDxAcc <- irDxAcc <- initDxAccList() # initialise diagnostic accuracy lists
  
  output$debug1 <- renderPrint({
    sessionInfo()
  })
  
  output$debug2 <- renderPrint({
    irTitle()
  })
  
  # Tabulate (for the index test)
  # true accuracy measures, absolute errors, percentage errors (for mid-ranges of given parameters). And lower and upper uncertainty intervals with 95% limits derived from a probability sensitivity analysis which varies measured and assumed parameters across their limits with PDFs able to be selected by the user from on option list.
  # graphs
  # 1.	Mosaic plots (to be shown in facets) of:
  #   a.	Index test: observed TP, FP, FN, TN
  #   b.	Reference test: assumed TP, FP, FN, TN (derived from sensitivity etc)
  #   c.	Index test: derived true TP, FP, FN, TN
  #   d.	Error matrix: observed – true index test evaluation data
  # 2.	Dependence of measured sensitivity of the index test on true sensitivity of reference test
  #       X = assumed true sensitivity of reference test
  #            (from lower to upper limit)
  #       Y = derived true sensitivity of index test (ribbon with 95% limits derived from probability sensitivity analysis which varies measured and assumed parameters across their limits)
  #       Y = measured sensitivity of index test (ribbon with given limits)
  # 3.	Dependence of measured specificity of the index test on true specificity of reference test
  # 4.	As above for specificity, mutatis mutandis
  # 5.	As above, mutatis mutandis, for predictive values, false positive and false negative rates
  # 6.	Animations of effects of univariate incremental changes in true sensitivity and specificity of reference test
  #
  ##############################################################################################################################
  #
  # Tabulate (for the index test)
  # true accuracy measures, absolute errors, percentage errors (for mid-ranges of given parameters). And lower and upper uncertainty intervals with 95% limits derived from a probability sensitivity analysis which varies measured and assumed parameters across their limits with PDFs able to be selected by the user from on option list.
  # 
  
  # set titles and labels for index and reference tests
  
  irTitle <- eventReactive(input$GoButton, 
                           {
                             paste0("Contingency matrix and diagnostic accuracy stats for ", input$IndexTest, " compared to ", input$ReferenceTest)
                           })
  rgTitle <- eventReactive(input$GoButton, 
                           {
                             paste0("Contingency matrix and diagnostic accuracy stats for ", input$ReferenceTest, " compared to gold standard")
                           })
  igTitle <- eventReactive(input$GoButton, 
                           {
                             paste0("Contingency matrix and diagnostic accuracy stats for ", input$IndexTest, " adjusted for inaccuracies in ", input$ReferenceTest)
                           })
  
  
  IT <- eventReactive(input$GoButton, {
    irDxAcc$Title <- input$Title
    irDxAcc$IndexTest <- input$IndexTest
    irDxAcc$ReferenceTest <- input$ReferenceTest
    
    
    #  set population and prevalence
    irDxAcc$DxStats["Estimate","Prevalence"] <- input$gPrevalence
    irDxAcc$DxStats["Estimate","Population"] <- input$iPopulation
    
    # set sensitivity and specificity
    # use the given range for low and high limits, and their mean for the estimate
    irDxAcc$DxStats["Conf_Low","Sensitivity"] <- input$irSen[1]
    irDxAcc$DxStats["Estimate","Sensitivity"] <- mean(input$irSen) 
    irDxAcc$DxStats["Conf_high","Sensitivity"] <- input$irSen[2]
    
    irDxAcc$DxStats["Conf_Low","Specificity"] <- input$irSpec[1]
    irDxAcc$DxStats["Estimate","Specificity"] <- mean(input$irSpec) 
    irDxAcc$DxStats["Conf_high","Specificity"] <- input$irSpec[2]
    
    # calculate contingency matrix and diagnostic accuracy stats 
    ##### to do: update function to calculate confidence limits 
    irDxAcc <- DxAcc(irDxAcc, direction = "From stats", CImethod = "proportion")
    
    return(irDxAcc)
  })
  
  
  RT <- eventReactive(input$GoButton, {
    rgDxAcc$Title <- input$Title
    rgDxAcc$IndexTest <- input$IndexTest
    rgDxAcc$ReferenceTest <- input$ReferenceTest
    
    #  assume same population and prevalence for reference test as for index test
    rgDxAcc$DxStats["Estimate","Prevalence"] <- input$gPrevalence
    rgDxAcc$DxStats["Estimate","Population"] <- input$iPopulation
    
    # set sensitivity and specificity
    # use the given range for low and high limits, and their mean for the estimate
    rgDxAcc$DxStats["Conf_Low","Sensitivity"] <- input$rgSen[1]
    rgDxAcc$DxStats["Estimate","Sensitivity"] <- mean(input$rgSen) 
    rgDxAcc$DxStats["Conf_high","Sensitivity"] <- input$rgSen[2]
    
    rgDxAcc$DxStats["Conf_Low","Specificity"] <- input$rgSpec[1]
    rgDxAcc$DxStats["Estimate","Specificity"] <- mean(input$rgSpec) 
    rgDxAcc$DxStats["Conf_high","Specificity"] <- input$rgSpec[2]
    
    # calculate contingency matrix and diagnostic accuracy stats 
    rgDxAcc <- DxAcc(rgDxAcc, direction = "From stats", CImethod = "estimated range")
    
    return(rgDxAcc)
  })
  
  # print tables for index test (measured)
  output$ITtitle <- renderText(irTitle())
  output$ITCMTable <- renderTable(IT()$DxCM)
  output$ITStatsTable <- renderTable(IT()$DxStats)
  
  # print tables for reference test (estimated)
  output$RTtitle <- renderText(RTtitle())
  output$RTStatsTable <- renderTable(RT()$DxStats)
  output$RTCMTable <- renderTable(RT()$DxCM)
  
  # print tables for index test (adjusted for imperfect reference test)
  output$ITAtitle <- renderText(ITAtitle())
  output$ITAStatsTable <- renderTable(ITA()$DxStats)
  output$ITACMTable <- renderTable(ITA()$DxCM)
  
  
  
  
  #   input$Title = "title for tables and graphs"
  #   input$IndexTest = "name of index test"
  #   input$ReferenceTest = "name of reference test"
  # 
  #   input$gPrevalence
  #   input$iPopulation
  #  
  #   input$irSen
  #   input$irSpec
  #  
  #   input$rgSen
  #   input$rgSpec
  # 
  #   input$igSen
  #   input$igSpec
  
  
}
