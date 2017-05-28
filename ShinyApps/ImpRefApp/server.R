
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#


# options(shiny.error = browser)

# initialise Dx accuracy list for index test (measured), reference test, index test (true)
#

source("FunctionsUsedByImpRefV2.R", local = TRUE)
LoadPackages()
enableBookmarking("url")

server <- function(input, output, session) {
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark, {
    session$doBookmark()
  })
  
# inputs from ui:   
#   input$Title = "title for tables and graphs"
#   input$IndexTest = "name of index test"
#   input$ReferenceTest = "name of reference test"
# 
#   input$Prevalence
#   input$Population
#  
#   input$ITsenMeas
#   input$ITpecMeas
#  
#   input$RTsenEst
#   input$RTspecEst
# 
  
  ITDxAccMeas <- initDxAccList() 
  RTDxAccEst <- ITDxAccMeas
  ITDxAccTrue <- ITDxAccMeas
  
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
  
  ITtitle <- eventReactive(input$GoButton, 
               {
                 paste0("Contingency matrix and diagnostic accuracy stats for ", input$IndexTest, " compared to ", input$ReferenceTest)
                })
  RTtitle <- eventReactive(input$GoButton, 
                           {
                             paste0("Contingency matrix and diagnostic accuracy stats for ", input$ReferenceTest, " compared to ", input$ReferenceTest)
                           })
  ITAtitle <- eventReactive(input$GoButton, 
                           {
                             paste0("Contingency matrix and diagnostic accuracy stats for ", input$IndexTest, " adjusted for inaccuracies in ", input$ReferenceTest)
                           })
  

    IT <- eventReactive(input$GoButton, {
    ITDxAccMeas$Title <- input$Title
    ITDxAccMeas$Subtitle <- input$Subtitle
    ITDxAccMeas$IndexTest <- input$IndexTest
    ITDxAccMeas$ReferenceTest <- input$ReferenceTest
    
   
    #  set population and prevalence
    ITDxAccMeas$DxStats["Estimate","Prevalence"] <- input$Prevalence
    ITDxAccMeas$DxStats["Estimate","Population"] <- input$Population
    
    # set sensitivity and specificity
    ITDxAccMeas$DxStats["Estimate","Sensitivity"] <- input$ITsenMeas
    ITDxAccMeas$DxStats["Estimate","Specificity"] <- input$ITspecMeas
   
    # calculate contingency matrix and diagnostic accuracy stats 
    ##### to do: update function to calculate confidence limits 
    ITDxAccMeas <- DxAcc(ITDxAccMeas, direction = "From stats", CImethod = "proportion")
    
   return(ITDxAccMeas)
  })

    
    RT <- eventReactive(input$GoButton, {
      RTDxAccEst$Title <- input$Title
      RTDxAccEst$Subtitle <- input$Subtitle
      RTDxAccEst$IndexTest <- input$IndexTest
      RTDxAccEst$ReferenceTest <- input$ReferenceTest
      
 #  assume same population and prevalence for reference test as for index test
      RTDxAccEst$DxStats["Estimate","Prevalence"] <- input$Prevalence
      RTDxAccEst$DxStats["Estimate","Population"] <- input$Population
      
      # set sensitivity and specificity
      # use the given range for low and high limits, and their mean for the estimate
      RTDxAccEst$DxStats["Conf_Low","Sensitivity"] <- input$RTsenEst[1]
      RTDxAccEst$DxStats["Estimate","Sensitivity"] <- mean(input$RTsenEst) 
      RTDxAccEst$DxStats["Conf_high","Sensitivity"] <- input$RTsenEst[2]
      
      RTDxAccEst$DxStats["Conf_Low","Specificity"] <- input$RTspecEst[1]
      RTDxAccEst$DxStats["Estimate","Specificity"] <- mean(input$RTspecEst) 
      RTDxAccEst$DxStats["Conf_high","Specificity"] <- input$RTspecEst[2]

      # calculate contingency matrix and diagnostic accuracy stats 
      RTDxAccEst <- DxAcc(RTDxAccEst, direction = "From stats", CImethod = "estimated range")

      return(RTDxAccEst)
    })
    
  # print tables for index test (measured)
  output$ITtitle <- renderText(ITtitle())
  output$ITStatsTable <- renderTable(IT()$DxStats)
  output$ITCMTable <- renderTable(IT()$DxCM)

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
  #   input$Prevalence
  #   input$Population
  #  
  #   input$ITsenMeas
  #   input$ITpecMeas
  #  
  #   input$RTsenEst
  #   input$RTspecEst
 
}

