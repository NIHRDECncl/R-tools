library(ggplot2)
library(dplyr)
library(magrittr)

  
  sorted <- TRUE  
  n <- 100
  sensitivity <- .9
  specificity <- .8
  Dpos <- 10
  Dneg <- n - Dpos 

  Tp <- round(sensitivity * Dpos)  
  Tn <- round(specificity * Dneg)
  
  Fn <- round((1 - sensitivity) * Dpos)
  Fp <- round((1 - specificity) * Dneg)
  
  testNeg <- Fn + Tn
  testPos <- Tp + Fp

  
  marginInsidePlot <- 0.01
  

#  dx2x2df <- reactive({data.frame(Dpos = Dpos, Dneg = Dneg(), Tp = Tp(), Fp = Fp(), Fn = Fn(), Tn = Tn(), Tpos = Tp() + Fp(), Tneg = Fn() + Tn(), row.names = "")})

  
  cmX <- c(
    (0.5 * Dpos/n),                # x for Tp
    (Dpos/n + 0.5*Dneg/n),         # x for Fp
    (0.5 * Dpos/n),                # x for Fn
    (Dpos/n + 0.5*Dneg/n)          # x for Tn
  )
  cmY <- c(
    (Fn + 0.5 * Tp/n),         # y for Tp
    (Tn + 0.5 * Fp)/n,         # y for Fp
    (0.5*Fn/n),                # y for Fn
    (0.5*Tn)/n                 # y for Tn
  )
  cmLabels <- c(
    paste("Tp = ", Tp),
    paste("Fp = ", Fp),
    paste("Fn = ", Fn),
    paste("Tn = ", Tn)
  )
  contingencyMatrix <- data.frame(cmX, cmY, cmLabels)
  contingencyMatrix
  
  
    if (sorted){
      x = c(
        runif(Tp, min = marginInsidePlot, max = Dpos/n) - marginInsidePlot,
        runif(Fn, min = marginInsidePlot, max = Dpos/n) - marginInsidePlot,
        runif(Fp, min = Dpos/n + marginInsidePlot, max = 1 - marginInsidePlot),
        runif(Tn, min = Dpos/n + marginInsidePlot, max = 1 - marginInsidePlot)
      )
      y = c(
        runif(Tp, min = Fn/(Tp + Fn) + marginInsidePlot, max = 1 - marginInsidePlot),
        runif(Fn, min = marginInsidePlot, max =  Fn/(Tp + Fn) - marginInsidePlot),
        runif(Fp, min = Tn/(Fp + Tn) + marginInsidePlot, max = 1 - marginInsidePlot),
        runif(Tn, min = marginInsidePlot, max = Tn/(Fp + Tn) - marginInsidePlot)
      )
      
    } else {
      x = c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
      y = c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
    }
    
  populationdf <-
      data.frame(
        ID = 1:n,
        condition = c(
          rep(paste("Present  = ", Dpos), times = Dpos),
          rep(paste("Absent = ", Dneg), times = Dneg)
        ),
        
        testResult = c(
          rep(paste("TestPos = ", Tp + Fp), times = Tp + Fp),
          rep(paste("TestNeg = ", Fn + Tn), times = Fn + Tn) 
        ),
        
        result = c(
          rep(paste("TruePos = ", Tp), times = Tp),
          rep(paste("FalseNeg = ", Fn), times = Fn), 
          rep(paste("FalsePos = ", Fp), times = Fp), 
          rep(paste("TrueNeg = ", Tn), times = Tn)
        ),
        x, 
        y
      )


  p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = condition)) + geom_point(size = 4) 
      + scale_color_manual(values=c("#999999", "#E69F00")) 
    if (sorted) {
      p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = condition)) 
        + geom_point(size = 4) 
        + scale_color_manual(values=c("#999999", "#E69F00"))  
    #    + geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, colour = NULL, shape = NULL), data = vdf()) 
    }
    
    p1 <- p1 
        + theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        ) 
      + labs(x = "", y = "") 
    + ggtitle("Population of people with and without a condition")
    p1
    
    p2 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = result)) 
    + geom_point(size = 4)
    if (sorted) {
      #   p2 <- p2 + geom_line(data = vdf(),aes(x = vx, y = vy), color = "red" , size = 0.8) # + 
      #  geom_line(aes(x = h2$hx, y = h2$hy), color = "red" , size = 0.8)
      
      # p2 <- p2 
      #   + geom_segment(aes(x = vx, y = vy, xend = vxend, yend = vyend, colour = NULL, shape = NULL), data = vdf()) 
      #   + geom_segment(aes(x = hx1, y = hy1, xend = hx2, yend = hy2, colour = NULL, shape = NULL), 
      #                data = hdf1) 
      #   + geom_segment(aes(x = hx3, y = hy3, xend = hx4, yend = hy4, colour = NULL, shape = NULL), data = hdf2) 
      #              +
      geom_text(data = contingencyMatrix, mapping = aes(x=cmX, y=cmY, label = cmLabels))
    }
    p2 <- p2 
#      + scale_color_manual(values=c("red", "blue", "green", "black")) 
    + theme(
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
      ) 
    + labs(x = "", y = "") +
      ggtitle("Population of people with test results: true and false positive; false and true negative")
    p2
    

  
