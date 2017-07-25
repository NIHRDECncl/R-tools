##########################################################
# FunctionsUsedByTestAccuracyApp.R()
#
# non-reactive functions used by the Shiny App to explore clinical accuracy measurements
#
# installation of colour picker from http://deanattali.com/blog/plot-colour-helper/
#
#install.packages("devtools")
#devtools::install_github("daattali/colourpicker")
#
##########################################################
#
# load packages used by the App 
LoadPackages <- function() {
  library(shiny)
  library(tidyverse) # Imports: broom, DBI, dplyr, forcats, ggplot2, haven, httr, hms, jsonlite, lubridate, magrittr, modelr, purrr, readr, readxl, stringr, tibble, rvest, tidyr, xml2
  library(rsconnect)   # needed to upload to Shinyio
  library(readr)       # needed for GET()
  library(vcd)         # mosaic() plot http://www.statmethods.net/advgraphs/mosaic.html
  library(colourpicker) # http://deanattali.com/blog/plot-colour-helper/ 
  library(shinythemes)
  library(DT)
  library(knitr)
  library(rmarkdown)
  library(shinycssloaders)
  #      library(proportion)  package no longer being maintained :-(
  library(PropCIs)
  library(rsconnect)   # needed to upload to Shinyio
  library(ggrepel)
  # ...
}

marginInsidePlot = 0.01
#################### confidence interval on a proportion ###############################
#
### this is a wrapper to allow the choice of CI method to be easily changed
#
# use PropCIs::scoreci() to calculate Wilson's confidence interval for a single proportion. 
#             Score CI based on inverting the asymptotic normal test using the null standard error
# Arguments: 
# x	           Number of successes
# n            Total sample size
# conf.level   Confidence level

ciprop <- function(x, n, alpha = 0.05)
{
  
  conf.level <- 1 - alpha
  
  return({
    data.frame(
      ciL = PropCIs::scoreci(x, n, conf.level)$conf.int[1],
      ciU = PropCIs::scoreci(x, n, conf.level)$conf.int[2]
    )
  })
}

DxStats <- function(n, prevalence, sensitivity, specificity) {
  #prevalence <- min(prevalence,0.9999)
  #prevalence <- max(prevalence,0.0001)
  
  Dpos <- n * prevalence
  Dneg <- n - Dpos
  
  Tp <- sensitivity * Dpos
  Tn <- specificity * Dneg
  
  Fn <- (1 - sensitivity) * Dpos
  Fp <- (1 - specificity) * Dneg
  
  PPV <- Tp/(Tp + Fp)
  NPV <- Tn/(Tn + Fn) 
  
  LRp <- sensitivity/(1 - specificity)
  LRn <- (1 -sensitivity)/(specificity)
  
  PreTestOddsP <- prevalence/(1 - prevalence)
  PreTestOddsN <- (prevalence)/(1 -prevalence)
  
  PostTestOddsP <- PreTestOddsP*LRp
  PostTestOddsN <- PreTestOddsN*LRn
  
  PostTestProbP <- PostTestOddsP/(PostTestOddsP + 1) # = PPV
  PostTestProbN <- PostTestOddsN/(PostTestOddsN + 1) # = (1 - NPV)
  
  cidf <- ciprop(PPV * n, n) # CI for post-positive test probability
  TPY_ciL <- cidf$ciL
  TPY_ciU <- cidf$ciU
  
  cidf <- ciprop(PostTestProbN * n, n) # CI for post-negative test probability
  TNY_ciL <- cidf$ciL
  TNY_ciU <- cidf$ciU
  
  data.frame(
    Dpos = Dpos,
    Dneg = Dneg,
    
    Tp = Tp,
    Tn = Tn,
    
    Fn = Fn,
    Fp = Fp,
    
    PPV = PPV,
    NPV = NPV,
    
    LRp = LRp,
    LRn = LRn,
    
    PreTestOddsP = PreTestOddsP,
    PreTestOddsN = PreTestOddsN,
    
    PostTestOddsP = PostTestOddsP,
    PostTestOddsN = PostTestOddsP,
    
    PostTestProbP = PostTestProbP,
    PostTestProbN = PostTestProbN,
    
    TPY_ciL = TPY_ciL,
    TPY_ciU = TPY_ciU,
    
    TNY_ciL = TNY_ciL,
    TNY_ciU = TNY_ciU
  )
}




linesDf <- function(n, prevalence, sensitivity, specificity){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
 
 # Dpos <- round((n * input$prevalence))
#  Dneg <- round(input$n - Dpos)
 # Fn <- round((1 - input$sensitivity) * Dpos)
 # Tn <- round(input$specificity * Dneg)
  
  return({data.frame(
    ### define computed line segments for vertical line separating Dpos from Dneg    
    vx = Dx$Dpos /n,
    vxlci = ciprop(Dx$Dpos, n)$ciL,
    vxuci = ciprop(Dx$Dpos, n)$ciU,
    
    ### define computed line segments for horizontal lines separating TestPos from TestNeg    
    hy1 = Dx$Fn/Dx$Dpos,  
    hy1lci = ciprop(Dx$Fn, Dx$Dpos)$ciL, 
    hy1uci = ciprop(Dx$Fn, Dx$Dpos)$ciU,
    
    hy2 = Dx$Tn/Dx$Dneg,
    hy2lci = ciprop(Dx$Tn, Dx$Dneg)$ciL,
    hy2uci = ciprop(Dx$Tn, Dx$Dneg)$ciU)
  })
}


dx2x2Table <- function(n, prevalence, sensitivity, specificity){
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  return(
    data.frame(
      ConditionAbsent = c(Dx$Fp, Dx$Tn, Dx$Dneg),
      Totals = c(Dx$Tp + Dx$Fp, Dx$Fn + Dx$Tn, n),
      row.names = c("Test positive", "Test negative", "Totals")
    )
  )
}


### coordinates and labels for contingency matrix graphic
contingencyM <- function(n, prevalence, sensitivity, specificity){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  
  return({data.frame(
    cmX = c(
      0.5 * Dx$Dpos/n,          # Tp 
      (Dx$Dpos + 0.5*Dx$Dneg)/n,   # Fp
      0.5 * Dx$Dpos/n,          # Fn
      (Dx$Dpos + 0.5*Dx$Dneg)/n,   # Tn
      0.5 * Dx$Dpos/n,          # ppv
      (Dx$Dpos + 0.5*Dx$Dneg)/n    # npv
    ),
    cmY = c(
      (Dx$Fn + 0.5*Dx$Tp)/(Dx$Fn + Dx$Tp),        #Tp
      (Dx$Tn + 0.5*Dx$Fp)/(Dx$Tn + Dx$Fp),        #Fp
      0.5 * Dx$Fn/(Dx$Fn + Dx$Tp),             #Fn
      0.5 * Dx$Tn/(Dx$Tn + Dx$Fp),             #Tn
      (Dx$Fn + 0.5*Dx$Tp)/(Dx$Fn + Dx$Tp) - 0.04, #ppv
      0.5 * Dx$Tn/(Dx$Tn + Dx$Fp) - 0.04       #npv
      
    ), 
    labs = c(
      paste("Tp = ", Dx$Tp),
      paste("Fp = ", Dx$Fp),
      paste("Fn = ", Dx$Fn),
      paste("Tn = ", Dx$Tn),
      paste("ppv = ", paste(format(100*Dx$Tp / (Dx$Tp + Dx$Fp), digits = 2),"%", sep = "")),
      paste("npv = ", paste(format(100*Dx$Tp / (Dx$Tn + Dx$Fn), digits = 2),"%", sep = ""))
    )
  )
  })
}


pvdf <- function(n,prevalence, sensitivity, specificity){

  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  

  
  return(
    data.frame(
      PredictiveValues = c(
        paste(format(100*Dx$Tp / (Dx$Tp + Dx$Fp), digits = 3), "%", sep = ""),
        paste(format(100*Dx$Tn / (Dx$Tn + Dx$Fn), digits = 3), "%", sep = "")
      ),
      AtPrevalence = c(paste(format(100*prevalence, digits = 2), "%", sep = "")),
      Measure = c("Sensitivity", "Specificity"),
      LL95CI = c(
        paste(trimws(format(100*(sensitivity - ciprop(sensitivity, n)$ciL), digits = 2)), "%", sep = ""),
        paste(trimws(format(100*(specificity - ciprop(specificity, n)$ciL), digits = 2)), "%", sep = "")),
      Mid = c(
        paste(trimws(format(100*sensitivity, digits = 2)), "%", sep = ""),
        paste(trimws(format(100*specificity, digits = 3)), "%", sep = "")
      ),
      UL95CI = c(
        paste(trimws(format(100*(sensitivity + ciprop(sensitivity, n)$ciU), digits = 2)), "%", sep = ""),
        paste(trimws(format(100*(specificity + ciprop(specificity, n)$ciU), digits = 2)), "%", sep = "")),
      row.names = c("ppv", "npv")
    )
  )
}

populationdf <- function(n, prevalence, sensitivity, specificity, sorted){
  
  Dx <- DxStats(n, prevalence, sensitivity, specificity) 
  
  if (sorted){
    x = c(
      runif(round(Dx$Tp,0), min = marginInsidePlot, max = Dx$Dpos/n - marginInsidePlot),
      runif(round(Dx$Fn,0), min = marginInsidePlot, max = Dx$Dpos/n - marginInsidePlot),
      runif(round(Dx$Fp,0), min = Dx$Dpos/n + marginInsidePlot, max = 1 - marginInsidePlot),
      runif(round(Dx$Tn,0), min = Dx$Dpos/n + marginInsidePlot, max = 1 - marginInsidePlot)
    )
    y = c(
      runif(round(Dx$Tp,0), min = round(Dx$Fn,0)/(round(Dx$Tp,0) + round(Dx$Fn,0)) + marginInsidePlot, max = 1 - marginInsidePlot),
      runif(round(Dx$Fn,0), min = marginInsidePlot, max =  Dx$Fn/(Dx$Tp + Dx$Fn) - marginInsidePlot),
      runif(round(Dx$Fp,0), min = round(Dx$Tn,0)/(round(Dx$Fp,0) + round(Dx$Tn,0)) + marginInsidePlot, max = 1 - marginInsidePlot),  # need to fix this if FPs less than 1!!
      runif(round(Dx$Tn,0), min = marginInsidePlot, max = Dx$Tn/(Dx$Fp + Dx$Tn) - marginInsidePlot)
    )
    
  } else {
    x = c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
    y = c(runif(n, min = marginInsidePlot, max = 1 - marginInsidePlot))
  }
  
  
  return( {
    data.frame(
      ID = 1:n,
      condition = c(
        rep(paste("Present  = ", Dx$Dpos), times = round(Dx$Dpos,0)),
        rep(paste("Absent = ",Dx$Dneg), times = round(Dx$Dneg,0))
      ),
      conditionShape = c(
        rep(21, times = round(Dx$Dpos,0)),
        rep(22, times = round(Dx$Dneg,0))
      ),
      
      testResult = c(
        rep(paste("TestPos = ", Dx$Tp + Dx$Fp), times = round(Dx$Tp + Dx$Fp,0)),
        rep(paste("TestNeg = ", Dx$Fn + Dx$Tn), times = round(Dx$Fn + Dx$Tn,0)) 
      ),
      
      result = c(
        rep(paste("TruePos = ", Dx$Tp), times = round(Dx$Tp,0)),
        rep(paste("FalseNeg = ", Dx$Fn), times = round(Dx$Fn,0)), 
        rep(paste("FalsePos = ", Dx$Fp), times = round(Dx$Fp,0)), 
        rep(paste("TrueNeg = ", Dx$Tn), times = round(Dx$Tn,0))
      ),
      resultShape = c(
        rep(21, times = round(Dx$Tp,0)), ## need to sort out these!
        rep(22, times = round(Dx$Fn,0)), 
        rep(23, times = round(Dx$Fp,0)), 
        rep(24, times = round(Dx$Tn,0))
      ),
      x, 
      y
    )
  })
}


popplot <- function(n, prevalence, sensitivity, specificity, sorted, ciFlag){

  populationdf <- populationdf(n, prevalence, sensitivity, specificity, sorted)
  linesDf <- linesDf(n, prevalence, sensitivity, specificity)
  
  p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = condition)) + geom_point(size = 4) +
   scale_color_manual(values=c("#999999", "#E69F00")) + coord_fixed()
  if (sorted) {
  p1 <- ggplot(populationdf, aes(x=x, y=y, color=condition, shape = condition)) + 
    geom_point(size = 4) + #scale_color_manual(values=c("#999999", "#E69F00"))  +
    
    ### add line segments (with 95% CI)  to separate Condition present from condition absent
    geom_segment(aes(x = vx, y = 0, xend = vx, yend = 1, colour = NULL, shape = NULL), 
                 data = linesDf) + 
    
    
    ### add in scales for x and y axis 
    scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                       labels = c("0","25%","50%","75%","100%")) + theme(axis.text.x = element_text(size = 15,colour = "azure4")) + 
    scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                       labels = c("0","25%","50%","75%","100%")) + theme(axis.text.y = element_text(size = 15,colour = "azure4")) + 
    coord_fixed()
  
  if (ciFlag) {
    p1 <- p1 + annotate("rect", xmin = linesDf$vxlci, xmax = linesDf$vxuci, ymin = 0, ymax = 1,
                        colour = "deepskyblue", alpha = 0.2)
  }
}
if (!sorted) {
  p1 <- p1 +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    ) 
}
p1 <- p1 +
  labs(x = "", y = "", title="Population: people with and without the condition") + 
  theme(plot.title = element_text(size = rel(1.5), colour = "dodgerblue3"))
p1

}



#####   you are here 25 july 2017.  


