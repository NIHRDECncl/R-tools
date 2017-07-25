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
             prevalence, sensitivity, specificity and rule in/out decision thresholds specified, lie between 0 and 1. ")
        } else {
          return(NULL)
        }
        
        
      })
    })
    
#     Dpos <- reactive({round(input$n * input$prevalence)})
#     Dneg <- reactive({round(input$n - Dpos())})
#     
#     Tp <- reactive({round(input$sensitivity * Dpos())})  ### this might be 1 too much???
#     Tn <- reactive({round(input$specificity * Dneg())})
#     
#     
#     Fn <- reactive({round((1 - input$sensitivity) * Dpos())})
#     Fp <- reactive({round((1 - input$specificity) * Dneg())})
#     
    
#     linesDf <- reactive({
#       Dpos <- round((input$n * input$prevalence))
#       Dneg <- round(input$n - Dpos)
#       Fn <- round((1 - input$sensitivity) * Dpos)
#       Tn <- round(input$specificity * Dneg)
#       
#       return({data.frame(
#         ### define computed line segments for vertical line separating Dpos from Dneg    
#         vx = Dpos() /input$n,
#         vxlci = ciproportion(Dpos, input$n)$cill,
#         vxuci = ciproportion(Dpos, input$n)$ciul,
#         
#         ### define computed line segments for horizontal lines separating TestPos from TestNeg    
#         hy1 = Fn/Dpos,  
#         hy1lci = ciproportion(Fn, Dpos)$cill, 
#         hy1uci = ciproportion(Fn, Dpos)$ciul,
#         
#         hy2 = Tn/Dneg,
#         hy2lci = ciproportion(Tn, Dneg)$cill,
#         hy2uci = ciproportion(Tn, Dneg)$ciul)
#       })
#     })
#     
    
#    dx2x2df <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Fp = Fp(), Fn = Fn(), Tn = Tn(), Tpos = Tp() + Fp(), Tneg = Fn() + Tn(), row.names = "")})
#    temp <- reactive({data.frame(Dpos = Dpos(), Dneg = Dneg(), Tp = Tp(), Tn = Tn(), Fp = Fp(), Fn = Fn(), Tpos = Tp() + Fp(), Tneg = Fn() + Tn(), row.names = "")})
    
#     dx2x2Table <- reactive({
#       
#       Dpos <- input$n * input$prevalence
#       Dneg <- input$n - Dpos
#       Fn <- (1 - input$sensitivity) * Dpos
#       Tn <- input$specificity * Dneg
#       Tp <- input$sensitivity * Dpos
#       Fp <- (1 - input$specificity) * Dneg
#       
#       return(
#         data.frame(
#           ConditionPresent = c(Tp, Fn, Dpos),
#           ConditionAbsent = c(Fp, Tn, Dneg),
#           Totals = c(Tp + Fp, Fn + Tn, input$n),
#           row.names = c("Test positive", "Test negative", "Totals")
#         )
#       )
#     })
#     
    cmX1 <- reactive({0.5 * Dpos()/input$n})
    cmX2 <- reactive({Dpos()/input$n + 0.5*Dneg()/input$n})# x for Tp
    cmX3 <- reactive({0.5 * Dpos()/input$n})
    cmX4 <- reactive ({Dpos()/input$n + 0.5*Dneg()/input$n })
    cmX <- reactive({c(cmX1(),cmX2(),cmX1(),cmX2())})
    
    cmY1 <-reactive({ (Fn() + 0.5 * Tp())/input$n})
    cmY2 <-reactive({ (Tn() + 0.5 * Fp())/input$n})
    cmY3 <- reactive({ (0.5*Fn())/input$n})
    cmY4 <- reactive({ (0.5*Tn())/input$n })
    cmY <- reactive({c(cmY1(),cmY1(),cmY2(),cmY2())})
    
    
    marginInsidePlot = 0.01
    
#     ### coordinates and labels for contingency matrix graphic
#     contingencyM <- reactive({
#       Dpos <- round((input$n * input$prevalence))
#       Dneg <- round(input$n - Dpos)
#       Fn <- round((1 - input$sensitivity) * Dpos)
#       Tn <- round(input$specificity * Dneg)
#       Tp <- round(input$sensitivity * Dpos)
#       Fp <- round((1 - input$specificity) * Dneg)
#       
#       return({data.frame(
#         cmX = c(
#           0.5 * Dpos/input$n,          # Tp 
#           (Dpos + 0.5*Dneg)/input$n,   # Fp
#           0.5 * Dpos/input$n,          # Fn
#           (Dpos + 0.5*Dneg)/input$n,   # Tn
#           0.5 * Dpos/input$n,          # ppv
#           (Dpos + 0.5*Dneg)/input$n    # npv
#         ),
#         cmY = c(
#           (Fn + 0.5*Tp)/(Fn + Tp),        #Tp
#           (Tn + 0.5*Fp)/(Tn + Fp),        #Fp
#           0.5 * Fn/(Fn + Tp),             #Fn
#           0.5 * Tn/(Tn + Fp),             #Tn
#           (Fn + 0.5*Tp)/(Fn + Tp) - 0.04, #ppv
#           0.5 * Tn/(Tn + Fp) - 0.04       #npv
#           
#         ), 
#         labs = c(
#           paste("Tp = ", Tp),
#           paste("Fp = ", Fp),
#           paste("Fn = ", Fn),
#           paste("Tn = ", Tn),
#           paste("ppv = ", paste(format(100*Tp / (Tp + Fp), digits = 2),"%", sep = "")),
#           paste("npv = ", paste(format(100*Tp / (Tn + Fn), digits = 2),"%", sep = ""))
#         )
#       )
#       })
#     })
    
#     pvdf <- reactive({
#       Dpos <- round((input$n * input$prevalence))
#       Dneg <- round(input$n - Dpos)
#       Fn <- round((1 - input$sensitivity) * Dpos)
#       Tn <- round(input$specificity * Dneg)
#       Tp <- round(input$sensitivity * Dpos)
#       Fp <- round((1 - input$specificity) * Dneg)
#       
#       marginInsidePlot = 0.01
#       
#       return(
#         data.frame(
#           PredictiveValues = c(
#             paste(format(100*Tp / (Tp + Fp), digits = 3), "%", sep = ""),
#             paste(format(100*Tn / (Tn + Fn), digits = 3), "%", sep = "")
#           ),
#           AtPrevalence = c(paste(format(100*input$prevalence, digits = 2), "%", sep = "")),
#           Measure = c("Sensitivity", "Specificity"),
#           LL95CI = c(
#             paste(trimws(format(100*(input$sensitivity - ciproportion(input$sensitivity, input$n)$cill), digits = 2)), "%", sep = ""),
#             paste(trimws(format(100*(input$specificity - ciproportion(input$specificity, input$n)$cill), digits = 2)), "%", sep = "")),
#           Mid = c(
#             paste(trimws(format(100*input$sensitivity, digits = 2)), "%", sep = ""),
#             paste(trimws(format(100*input$specificity, digits = 3)), "%", sep = "")
#           ),
#           UL95CI = c(
#             paste(trimws(format(100*(input$sensitivity + ciproportion(input$sensitivity, input$n)$ciul), digits = 2)), "%", sep = ""),
#             paste(trimws(format(100*(input$specificity + ciproportion(input$specificity, input$n)$cill), digits = 2)), "%", sep = "")),
#           row.names = c("ppv", "npv")
#         )
#       )
#     })
#     populationdf <- reactive( {
#       
#       Dpos <- round((input$n * input$prevalence))
#       Dneg <- round(input$n - Dpos)
#       Fn <- round((1 - input$sensitivity) * Dpos)
#       Tn <- round(input$specificity * Dneg)
#       Tp <- round(input$sensitivity * Dpos)
#       Fp <- round((1 - input$specificity) * Dneg)
#       
#       if (input$sorted){
#         x = c(
#           runif(Tp, min = marginInsidePlot, max = Dpos/input$n - marginInsidePlot),
#           runif(Fn, min = marginInsidePlot, max = Dpos/input$n - marginInsidePlot),
#           runif(Fp, min = Dpos/input$n + marginInsidePlot, max = 1 - marginInsidePlot),
#           runif(Tn, min = Dpos/input$n + marginInsidePlot, max = 1 - marginInsidePlot)
#         )
#         y = c(
#           runif(Tp, min = Fn/(Tp + Fn) + marginInsidePlot, max = 1 - marginInsidePlot),
#           runif(Fn, min = marginInsidePlot, max =  Fn/(Tp + Fn) - marginInsidePlot),
#           runif(Fp, min = Tn/(Fp + Tn) + marginInsidePlot, max = 1 - marginInsidePlot),
#           runif(Tn, min = marginInsidePlot, max = Tn/(Fp + Tn) - marginInsidePlot)
#         )
#         
#       } else {
#         x = c(runif(input$n, min = marginInsidePlot, max = 1 - marginInsidePlot))
#         y = c(runif(input$n, min = marginInsidePlot, max = 1 - marginInsidePlot))
#       }
#       
#       
#       return( {
#         data.frame(
#           ID = 1:input$n,
#           condition = c(
#             rep(paste("Present  = ", Dpos), times = Dpos),
#             rep(paste("Absent = ",Dneg), times = Dneg)
#           ),
#           conditionShape = c(
#             rep(21, times = Dpos),
#             rep(22, times = Dneg)
#           ),
#           
#           testResult = c(
#             rep(paste("TestPos = ", Tp + Fp), times = Tp + Fp),
#             rep(paste("TestNeg = ", Fn + Tn), times = Fn + Tn) 
#           ),
#           
#           result = c(
#             rep(paste("TruePos = ", Tp), times = Tp),
#             rep(paste("FalseNeg = ", Fn), times = Fn), 
#             rep(paste("FalsePos = ", Fp), times = Fp), 
#             rep(paste("TrueNeg = ", Tn), times = Tn)
#           ),
#           resultShape = c(
#             rep(21, times = Tp),
#             rep(22, times = Fn), 
#             rep(23, times = Fp), 
#             rep(24, times = Tn)
#           ),
#           x, 
#           y
#         )
#       })
#     })
#     
    
    
    observeEvent(input$GoButton, {
      output$populationPlot<-renderPlot({
        if(isValid_num()){
          isolate(popplot(n, prevalence, sensitivity, specificity, sorted, ciFlag))
        }
      })
    }, ignoreNULL = FALSE)

    
#     output$populationPlot<-renderPlot({
#       
#       p1 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = condition)) + geom_point(size = 4) +
#         scale_color_manual(values=c("#999999", "#E69F00")) + coord_fixed()
#       if (input$sorted) {
#         p1 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = condition)) + 
#           geom_point(size = 4) + #scale_color_manual(values=c("#999999", "#E69F00"))  +
#           
#           ### add line segments (with 95% CI)  to separate Condition present from condition absent
#           geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL), 
#                        data = linesDf()) + 
#           
#           
#           ### add in scales for x and y axis 
#           scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
#                              labels = c("0","25%","50%","75%","100%")) + theme(axis.text.x = element_text(size = 15,colour = "azure4")) + 
#           scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
#                              labels = c("0","25%","50%","75%","100%")) + theme(axis.text.y = element_text(size = 15,colour = "azure4")) + 
#           coord_fixed()
#         
#         if (input$ciFlag) {
#           p1 <- p1 + annotate("rect", xmin = linesDf()$vxlci, xmax = linesDf()$vxuci, ymin = 0, ymax = 1,
#                               colour = "deepskyblue", alpha = 0.2)
#         }
#       }
#       if (!input$sorted) {
#         p1 <- p1 +
#           theme(
#             axis.text.x = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks = element_blank()
#           ) 
#       }
#       p1 <- p1 +
#         labs(x = "", y = "", title="Population: people with and without the condition") + 
#         theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
#       p1
#       
#     })
#     
#     
    
    output$testedPlots<-renderPlot( {
      
      p2 <- ggplot(populationdf(), aes(x=x, y=y, color=condition, shape = result)) + geom_point(size = 4) + coord_fixed()
      
      if (input$sorted) {
        p2 <- p2 + 
          ### line to separate Condition present/absent
          geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL), 
                       data = linesDf()) +
          
          ### line to separate Tp from Fn          
          geom_segment(aes(x = 0, y = hy1, xend = vx, yend = hy1, colour = NULL, shape = NULL), 
                       data = linesDf()) + 
          
          ### line to separate Fp from Tn          
          geom_segment(aes(x = vx, y = hy2, xend = 1, yend = hy2, colour = NULL, shape = NULL), 
                       data = linesDf()) + 
          
          ### label the cells of the contingency matrix 
          geom_text(data = contingencyM(), size = 7, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL), 
                    fontface = 2, colour = "gray41") + 
          
          ### add in scales for x and y axis
          scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                             labels = c("0","25%","50%","75%","100%")) + theme(axis.text.x = element_text(size = 15,colour = "azure4")) + 
          scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                             labels = c("0","25%","50%","75%","100%")) + theme(axis.text.y = element_text(size = 15,colour = "azure4")) + 
          coord_fixed()
        
        
        if (input$ciFlag) {
          p2 <- p2 +
            ### rectangles to show 95% CIs
            annotate("rect", xmin = linesDf()$vxlci, xmax = linesDf()$vxuci, ymin = 0, ymax = 1,
                     colour = "deepskyblue", alpha = 0.2) +
            annotate("rect", xmin = linesDf()$vxlci, xmax = linesDf()$vxuci, ymin = 0, ymax = 1,
                     colour = "deepskyblue", alpha = 0.2) +
            annotate("rect", xmin = 0, xmax = linesDf()$vx, ymin = linesDf()$hy1lci, ymax = linesDf()$hy1uci,
                     colour = "darksalmon", alpha = 0.2) +
            annotate("rect", xmin = linesDf()$vx, xmax = 1, ymin = linesDf()$hy2lci, ymax = linesDf()$hy2uci,
                     colour = "darksalmon", alpha = 0.2)      +
            geom_text(data = contingencyM(), size = 7, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL), 
                      fontface = 2, colour = "gray41") 
        }
        
        
      }
      if (!input$sorted) {
        p2 <- p2 +  theme(
          axis.text.x = element_text(),#element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()
        ) 
      }
      p2 <- p2 +
        labs(x = "", y = "", 
             title ="Test accuracy: true and false positives; \nfalse and true negatives, sensitivity, specificity, ...") +
        theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
      #ggtitle("Test accuracy: true and false positives; \n false and true negatives, sensitivity, specificity, ...")
      p2
      
    })
    
    
    distributiondf <- reactive({
      xdist <- seq(0,1,length = 1000)
      mean_pos <- 0.5 - input$sensitivity*0.25
      ydist <- input$prevalence*dnorm(xdist,mean = mean_pos,sd = 0.1)
      xdist2 <- seq(0,1,length = 1000)
      mean_neg <- 0.5 + input$specificity*0.25
      ydist2 <- (1-input$prevalence)*dnorm(xdist2,mean = mean_neg,sd = 0.1)
      
      
      
      return({data.frame(
        xdist = xdist, 
        ydist = ydist,
        xdist2 = xdist2,
        ydist2 = ydist2)
      })
      
    })
    
    ### coordinates and labels for distributions graphic
    distritext <- reactive({
      mean_pos <- 0.5 - input$sensitivity*0.25
      xdist <- seq(0,1,length = 1000)
      ydist <- input$prevalence*dnorm(xdist,mean = mean_pos,sd = 0.05)
      mean_neg <- 0.5 + input$specificity*0.25
      xdist2 <- seq(0,1,length = 1000)
      ydist2 <- (1-input$prevalence)*dnorm(xdist2,mean = mean_neg,sd = 0.05)
      
      Dpos <- round((input$n * input$prevalence))
      Dneg <- round(input$n - Dpos)
      Fn <- round((1 - input$sensitivity) * Dpos)
      Tn <- round(input$specificity * Dneg)
      Tp <- round(input$sensitivity * Dpos)
      Fp <- round((1 - input$specificity) * Dneg)
      
      max1 <- 0.25*max(ydist, na.rm = TRUE)
      max2 <- 0.25*max(ydist2, na.rm = TRUE)
      
      return({data.frame(
        cmX = c(
          mean_pos,
          mean_neg - 0.25,
          mean_pos + 0.25,
          mean_neg
        ),
        cmY = c(
          max1,
          max2*0.25,
          max1*0.25,
          max2
        ), 
        labs = c(
          paste("Tp = ", Tp),
          paste("Fp = ", Fp),
          paste("Fn = ", Fn),
          paste("Tn = ", Tn)
        )
      )
      })
    })
    
    
    output$distributionplots <- renderPlot({
      shade <- rbind(c(0.5,0), subset(distributiondf(), xdist > 0.5), c(distributiondf()[nrow(distributiondf()), "X"], 0))
      shade2 <- rbind(c(0.5,0), subset(distributiondf(), xdist2 > 0.5), c(distributiondf()[nrow(distributiondf()), "X2"], 0))
      distri <- ggplot(distributiondf(), aes(x = xdist2, y = ydist2)) +
        geom_polygon(data = shade2, aes(xdist2, ydist2), fill = "#E69F00")
      distri <- distri + geom_line(colour = "#E69F00")
      distri <- distri + geom_line(aes(x = xdist, y = ydist), colour =  "#999999") + geom_vline(xintercept = 0.5) +
        geom_polygon(data = shade, aes(xdist, ydist), fill = "#999999") + 
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank())
      distri <- distri + 
        geom_text(data = distritext(), size = 5, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL))
      distri
      
    })
    
    
    output$dx2x2Table <- renderTable(dx2x2Table(),digits = 0)
    output$pvdf <- renderTable(pvdf())
    
    
    
    
    
  })
