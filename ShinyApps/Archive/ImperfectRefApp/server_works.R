#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#install the packages you require
# if (!require(shiny)){
#   install.packages("shiny", repos="http://cran.rstudio.com/") 
#   library("shiny")
# }
library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)

# Define server logic for slider examples

shinyServer(function(input, output) {
  
  # naming conventions for the suffix in 2x2 diagnostic accuracy statistics
  ##   2 => 2x2 table
  ##   A => actual (true) gold standard test
  ##   R => rusty reference test (with assumed 100% sensitivity and 100% specificty)
  ##   I => index (new) test
  ##   IR => index test compared to rusty reference test
  ##   IA => index test compared to actual (true) gold standard
  ##   RA => reference test compared to actual (true) gold standard

  # initialise variables
  
  # SnIR = 0
  # SnIA= SnIR
  # SpIR = SnIR
  # SpIA = SnIR
  # ErrorSen = SnIR
  # ErrorSpec = SnIR
  
    # Reactive expression to compose a data frame containing all of the assumptions
   formula1 <- reactive({

    # Compose data frame   
    SpecSenChoice = if (input$SenSpecFlag == 1) {"specificity"} else {"sensitivity"}
    
    AssumptionsDF <- data.frame(
      Name = c("Actual sensitivity of reference test",
               "Actual specificity of reference test",
               "Actual Prevalence",
               "Graphing choice",
               paste("Actual ", SpecSenChoice, " of index test"),
               if (input$SpreadFlag == 1) {"Show 15% variation in "} else {paste("show 15% variation in index test's ") } # 1=sensitivity, 2=specificity
      ),
      Assumptions = as.character(c(input$SnRA,
                             input$SpRA,
                             input$prevA,
                             if (input$SenSpecFlag == 1) {"Sensitivity"} else {"Specificity"}, 
                             input$SenSpecIRAssumed,
                             if (input$SpreadFlag == 1) {"Prevalence"} else {
                               if (input$SenSpecFlag == 1) {"Specificity"} else {"Sensitivity"}
                               } 
                             )),
      stringsAsFactors=FALSE) 
    

   }) # close reactive
  # Show the assumptions using an HTML table
  output$formula1 <- renderTable( {
    formula1()
  }, include.rownames = FALSE) # close renderTable
  
  source("calfunction2.R")
  d1 <- reactive({estdxacc2(input$SnRA, input$SpRA, input$prevA, input$SenSpecFlag, 
                            input$SenSpecIRAssumed, input$VarFlag)})
  
  rangePlots1 <- reactiveValues(x1 = NULL, y1 = NULL)
  rangePlots2 <- reactiveValues( x2 = NULL, y2 = NULL)
  rangePlots3 <- reactiveValues(x3 = NULL, y3 = NULL)

   output$plot1 <-renderPlot({
    
     dd <- d1()
    
    if (input$SenSpecFlag == 1)  {
      #### plot actual senstivity of index test against measured sensitivity, given:
      ###########          actual specificity of index test
      ###########          actual sensitivity and specificity of rusty reference test
      ###########          actual prevalence
      x1  = dd$SnIR
      x1label <- "Measured sensitivity of index test"
      y1 = dd$SnIA
      y1label <- "Actual sensitivity of index test"
      y2 = dd$ErrorSen
      y2label <- "Error in sensitivity of index test (% points)"
      plotTitle1 <- "Measured sensitivity"
      
      
      if(input$SpreadFlag == 1) {
        prevA1 = input$prevA*0.85
        prevA2 = input$prevA*1.15
        dd2 = estdxacc2(input$SnRA, input$SpRA, prevA1, input$SenSpecFlag, 
                                                 input$SenSpecIRAssumed, input$VarFlag)
        dd3 = estdxacc2(input$SnRA, input$SpRA, prevA2, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        x1_lower = dd2$SnIR
        y1_lower = dd2$SnIA
        y2_lower = dd2$ErrorSen
        
        x1_upper = dd3$SnIR
        y1_upper = dd3$SnIA
        y2_upper = dd3$ErrorSen
        
      } else {
        SpRA1 = input$SpRA*0.85
        SpRA2 = input$SpRA*1.15
        dd2 = estdxacc2(input$SnRA, SpRA1, input$prevA, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        dd3 = estdxacc2(input$SnRA, SpRA2, input$prevA, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        x1_lower = dd2$SnIR
        y1_lower = dd2$SnIA
        y2_lower = dd2$ErrorSen
        
        x1_upper = dd3$SnIR
        y1_upper = dd3$SnIA
        y2_upper = dd3$ErrorSen
        
      }
    } else {
      #### plot actual specificity of index test againsty measured specificity, given:
      ###########          actual sensitivity of index test
      ###########          actual sensitivity and specificity of rusty reference test
      ###########          actual prevalence
      x1 = dd$SpIR
      x1label <- "Measured specificity of index test"
      y1 = dd$SpIA
      y1label <- "Actual specificity of index test"
      y2 = dd$ErrorSpec
      y2label <- "Error in specificity of index test (% points)"
      plotTitle1 <- "Measured specifcity"
      
      if(input$SpreadFlag == 1) {
        prevA1 = input$prevA*0.85
        prevA2 = input$prevA*1.15
        dd2 = estdxacc2(input$SnRA, input$SpRA, prevA1, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        dd3 = estdxacc2(input$SnRA, input$SpRA, prevA2, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        x1_lower = dd2$SpIR
        y1_lower = dd2$SpIA
        y2_lower = dd2$ErrorSpec
        
        x1_upper = dd3$SpIR
        y1_upper = dd3$SpIA
        y2_upper = dd3$ErrorSpec
        
      } else {
        SnRA1 = input$SnRA*0.85
        SnRA2 = input$SnRA*1.15
        dd2 = estdxacc2(SnRA1, input$SpRA, input$prevA, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        dd3 = estdxacc2(SnRA2, input$SpRA, input$prevA, input$SenSpecFlag, 
                        input$SenSpecIRAssumed, input$VarFlag)
        x1_lower = dd2$SpIR
        y1_lower = dd2$SpIA
        y2_lower = dd2$ErrorSpec
        
        x1_upper = dd3$SpIR
        y1_upper = dd3$SpIA
        y2_upper = dd3$ErrorSpec
      }
    } # end else
  

    plotdf = data.frame(x1,y1,y2)
    plotdf_lower = data.frame(x = x1_lower,y = y1_lower,y2_lower)
    plotdf_upper = data.frame(x = x1_upper,y = y1_upper,y2_upper)
    #par(pin=c(2,4))
    p1 <- plot.new()
    p1 <- ggplot(plotdf,aes(plotdf$x1, plotdf$y1))
    p1 <- p1 + geom_line(color = "red", size=1.75) + 
      geom_line(data = plotdf_lower, aes(plotdf_lower$x,plotdf_lower$y),color = "green", size = 0.8) + 
      geom_line(data = plotdf_upper, aes(plotdf_upper$x,plotdf_upper$y), color = "green", size = 0.8) 
   # p1 <- p1 + scale_x_continuous(limits = c(0,1)) +
      #scale_y_continuous(limits = c(0,1))  
    p1 <- p1 + geom_abline(intercept = 0, slope = 1, color = "blue") + labs(x=x1label,y=y1label) +
      coord_cartesian(xlim = rangePlots1$x1, ylim = rangePlots1$y1)
  #  p1 <- p1 + coord_fixed(ratio = 0.75)
    p1 <- p1 + ggtitle(plotTitle1)


    p2 <- plot.new()
    p2 <- ggplot(plotdf,aes(plotdf$x1, plotdf$y2))
    p2 <- p2 + geom_line(color = "red", size = 1.0) + scale_x_continuous(limits = c(0,1)) +
      scale_y_continuous(limits = c(-1,1))
    p2 <- p2 + labs(x=x1label,y=y2label) 
    p2 <- p2 + geom_abline(intercept = 0, slope = 0, color = "blue", size = 1.0) + 
      coord_cartesian(xlim = rangePlots2$x2, ylim = rangePlots2$y2)
    p2 <- p2 + coord_fixed(ratio = 0.25)
    p2 <- p2 + ggtitle("Error in measurement")
    
    y3 <- dd$NRpos
    y4 <- dd$NRneg
    NetRplot <- data.frame(x1, y3, y4)
    
    p3 <- plot.new
    p3 <- ggplot(NetRplot, aes(x = x1)) + 
      geom_line(aes(y = y3, colour = "Reclassified positives"), size = 1.75) + 
      geom_line(aes(y=y4, colour = "Reclassified negatives"), size = 1.75) + 
      scale_colour_manual("",breaks = c("Reclassified positives", "Reclassified negatives"), 
                          values = c("cyan", "pink"))

    p3 <- p3 + labs(x=x1label,y="Net reclassifications") + theme(legend.position = c(0.8, 0.2)) +
             coord_cartesian(xlim = rangePlots3$x3, ylim = rangePlots3$y3)
     

    grid.arrange(p1, p2, p3, ncol = 2, widths = c(4.5,3.5), 
                 layout_matrix = cbind(c(1,1), c(2,3)))
   })
      
   
   observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        rangePlots1$x1 <- c(brush$xmin, brush$xmax)
        rangePlots1$y1 <- c(brush$ymin, brush$ymax)
        rangePlots2$x2 <- c(brush$xmin, brush$xmax)
        rangePlots2$y2 <- c(brush$ymin, brush$ymax)
        rangePlots3$x3 <- c(brush$xmin, brush$xmax)
        rangePlots3$y3 <- c(brush$ymin, brush$ymax)
      } else {
        rangePlots1$x1 <- NULL
        rangePlots1$y1 <- NULL
        rangePlots2$x2 <- NULL
        rangePlots2$y2 <- NULL
        rangePlots3$x3 <- NULL
        rangePlots3$y3 <- NULL
      }
      })
    
   
    
    
    # Reactive expression to compose a data frame containing all of the assumptions
    formula2 <- reactive({
    dd <- d1()
      # Compose data frame  
      # tabulate the reclassifications: (True - Measured Positives) and (True - Measured Negatives)
      NetReclassificationsDF <- data.frame(
        Name = c("(Actual - Measured) true positives for index test",
                 "(Actual - Measured) true negatives for index test"),
        NetReclassifications = as.character(c(dd$NRpos, dd$NRneg)),
        stringsAsFactors=FALSE)
      
      }) # close reactive
    
    # Show the reclassifications using an HTML table
     # output$formula2 <- renderTable(formula2(), include.rownames = FALSE) # close renderTable
      
}) # end Shiny server function
     
     
    


