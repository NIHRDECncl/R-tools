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

## global.R ##

# Define server logic for slider examples

server <- function(input, output, session) {
  
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

      # SpecSenChoice: 1=sensitivity, 2=specificity
    
    SpecSenChoice = if (input$SenSpecFlag == 1) {"specificity"} else {"sensitivity"}
    SpreadChoice = ifelse(input$SpreadFlag1 == 1 | input$SpreadFlag2 == 1, 1, 2) # 1=spread prevalence; 2= spread sensitivity/specificity
  
    AssumptionsDF <- data.frame(
      Name = c("Reference test Actual sensitivity",
               "Reference test Actual specificity",
               "Actual Prevalence",
               "Graphing choice",
               paste("Index test: Actual ", SpecSenChoice),
               if (SpreadChoice == 1)  {"Show 15% variation in prevalence"} 
               else 
                 {paste0("show 15% variation in reference test's ", SpecSenChoice) 
                 } # 1=sensitivity, 2=specificity
      ),
      Assumptions = as.character(c(input$SnRA,
                             input$SpRA,
                             input$prevA,
                             if (input$SenSpecFlag == 1) {"Sensitivity"} else {"Specificity"}, 
                             if (input$SenSpecFlag == 1) {"Actual specificity of index test"} else {"Actual sensitivity of index test"},
                             if (SpreadChoice == 1) {"Prevalence"} 
                               else {
                                 if(input$SenSpecFlag == 1) {"Specificity"} 
                                   else {"Sensitivity"}
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
                            if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                            input$VarFlag)})
  
  rangePlots1 <- reactiveValues(x1 = NULL, y1 = NULL)
  rangePlots2 <- reactiveValues(x2 = NULL, y2 = NULL)
  rangePlots3 <- reactiveValues(x3 = NULL, y3 = NULL)

   # SSF <- reactive({input$SenSpecFlag})
   # SF <- reactive({input$SpreadFlag})
   
   d2 <- reactive({
     SpreadChoice = ifelse(input$SpreadFlag1 == 1 | input$SpreadFlag2 == 1, 1, 2) # 1=spread prevalence; 2= spread sensitivity/specificity
     if(input$SenSpecFlag == 1) {
       if(SpreadChoice == 1) {estdxacc2(input$SnRA, input$SpRA, input$prevA*0.85, input$SenSpecFlag, 
                              if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                              input$VarFlag)

     } else {estdxacc2(input$SnRA, input$SpRA*0.85, input$prevA, input$SenSpecFlag, 
                       if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                       input$VarFlag)
     }
   }
   else {
     if(SpreadChoice == 1) {estdxacc2(input$SnRA, input$SpRA, input$prevA*0.85, input$SenSpecFlag, 
                              if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                              input$VarFlag)
     } else {
       estdxacc2(input$SnRA*0.85, input$SpRA, input$prevA, input$SenSpecFlag, 
                 if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                 input$VarFlag)
     }
   }
   })
   
   d3 <- reactive({
     SpreadChoice = ifelse(input$SpreadFlag1 == 1 | input$SpreadFlag2 == 1, 1, 2) # 1=spread prevalence; 2= spread sensitivity/specificity
     if(input$SenSpecFlag == 1) {
       if(SpreadChoice == 1) {estdxacc2(input$SnRA, input$SpRA, input$prevA*1.15, input$SenSpecFlag, 
                                if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                                input$VarFlag)
       } else {estdxacc2(input$SnRA, input$SpRA*1.15, input$prevA, input$SenSpecFlag, 
                         if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                         input$VarFlag)
       }
     }
     else {
       if(input$SenSpecFlag == 1) {estdxacc2(input$SnRA, input$SpRA, input$prevA*1.15, input$SenSpecFlag, 
                                if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                                input$VarFlag)
       } else {estdxacc2(input$SnRA*1.15, input$SpRA, input$prevA, input$SenSpecFlag, 
                         if (input$SenSpecFlag == 1) {input$SpecIRAssumed} else {input$SenIRAssumed},
                         input$VarFlag)
       }
     }
   })
   
   

   plotdf <- reactive({
     dd <- d1()
    if (input$SenSpecFlag == 1)  {
      #### plot actual senstivity of index test against measured sensitivity, given:
      ###########          actual specificity of index test
      ###########          actual sensitivity and specificity of rusty reference test
      ###########          actual prevalence
      x1  = dd$SnIR
      x1label = "Index test: measured sensitivity"
      y1 = dd$SnIA
      y1label = "Index test: actual sensitivity"
      y2 = dd$ErrorSen
      y2label = "Error in sensitivity of index test (% points)"
      plotTitle1 = "Measured sensitivity"
    } else {
      #### plot actual specificity of index test againsty measured specificity, given:
      ###########          actual sensitivity of index test
      ###########          actual sensitivity and specificity of rusty reference test
      ###########          actual prevalence
      x1 = dd$SpIR
      x1label <- "Index test: Measured specificity"
      y1 = dd$SpIA
      y1label <- "Index test: Actual specificity"
      y2 = dd$ErrorSpec
      y2label <- "Error in specificity of index test (% points)"
      plotTitle1 <- "Measured specifcity"
    }
     return({data.frame(x1, y1, x1label, y1label, y2, y2label, plotTitle1 )
     })
   }) 
     

        
      
  plotdf_limits <- reactive({
    
       if(input$SenSpecFlag == 1) {
         dd2 <- d2()
         dd3 <- d3()
         x1_lower = dd2$SnIR
         y1_lower = dd2$SnIA
         y2_lower = dd2$ErrorSen
        
         x1_upper = dd3$SnIR
         y1_upper = dd3$SnIA
         y2_upper = dd3$ErrorSen
        
      }  else {
        dd2 <- d2()
        dd3 <- d3()
        x1_lower = dd2$SpIR
        y1_lower = dd2$SpIA
        y2_lower = dd2$ErrorSpec
        
        x1_upper = dd3$SpIR
        y1_upper = dd3$SpIA
        y2_upper = dd3$ErrorSpec
        
      } 
     # end else
  return({data.frame(x1_lower, y1_lower, y2_lower, x1_upper, y1_upper, y2_upper)})
})
    

    
    output$plot1 <-renderPlot({

    plotdf1 <- plotdf()
    p1 <- ggplot(data = plotdf(),aes(x = x1, y = y1))
    p1 <- p1 + geom_line(color = "red", size=1.75) + labs(x=plotdf1$x1label,y=plotdf1$y1label) + 
      geom_line(data = plotdf_limits(), aes(x = x1_lower, y = y1_lower),color = "green", size = 0.8)  + 
      geom_line(data = plotdf_limits(), aes(x = x1_upper, y = y1_upper), color = "green", size = 0.8) 
#     p1 <- p1 + scale_x_continuous(limits = c(0,1)) +
#            scale_y_continuous(limits = c(0,1))  
     p1 <- p1 + geom_abline(intercept = 0, slope = 1, color = "blue")  +
       coord_cartesian(xlim = rangePlots1$x, ylim = rangePlots1$y)
#     p1 <- p1 + coord_fixed(ratio = 0.75)
     p1 <- p1 + ggtitle(plotdf1$plotTitle1)
    p1
     })
    
     output$plot2 <-renderPlot({
    # p2 <- plot.new()
     plotdf1 <- plotdf()
     p2 <- ggplot(data = plotdf1,aes(x = x1, y = y2))
     p2 <- p2 + geom_line(color = "red", size = 1.0)+ labs(x=plotdf1$x1label,y=plotdf1$y2label) +
      scale_x_continuous(limits = c(0,1)) +
       scale_y_continuous(limits = c(-1,1))
     p2 <- p2 + geom_abline(intercept = 0, slope = 0, color = "blue", size = 1.0) + 
       coord_cartesian(xlim = rangePlots2$x, ylim = rangePlots2$y)
    # p2 <- p2 + coord_fixed(ratio = 0.25)
     p2 <- p2 + ggtitle("Error in measurement")
     p2
     })
     
     
    output$plot3 <-renderPlot({
    dd <- d1()
    
    if (input$SenSpecFlag == 1)  {  
      x1  = dd$SnIR 
      x1label = "Index test: Measured sensitivity"
    } else {
      x1 = dd$SpIR
      x1label <- "Index test: Measured specificity"
    }
    
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
             coord_cartesian(xlim = rangePlots3$x, ylim = rangePlots3$y)
     

     p3
   })
      
   
   observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        rangePlots1$x <- c(brush$xmin, brush$xmax)
        rangePlots1$y <- c(brush$ymin, brush$ymax)
       
      } else {
        rangePlots1$x <- NULL
        rangePlots1$y <- NULL
      }
      })
   observeEvent(input$plot2_dblclick, {
     brush <- input$plot2_brush
     if (!is.null(brush)) {
       rangePlots2$x <- c(brush$xmin, brush$xmax)
       rangePlots2$y <- c(brush$ymin, brush$ymax)
       
     } else {
       rangePlots2$x <- NULL
       rangePlots2$y <- NULL
     }
   })
   observeEvent(input$plot3_dblclick, {
     brush <- input$plot3_brush
     if (!is.null(brush)) {
       rangePlots3$x <- c(brush$xmin, brush$xmax)
       rangePlots3$y <- c(brush$ymin, brush$ymax)
       
     } else {
       rangePlots3$x <- NULL
       rangePlots3$y <- NULL
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
      
} # end Shiny server function
     
     
    


