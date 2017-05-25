###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
### on True positives, False positives, False negatives, and True negatives.

library(shiny)
library(ggplot2)
library(dplyr)
library(MESS)

#######################################################
# formula to calculate confidence interval for a proportion with 
#  y = numerator
#  n = denominator
#  y/n = observed proportion
#
#  Uses adjusted Wald method to ensure CI is calculated appropriately when n is small ( ie <5 )
#
### references:
#   Approximate Is Better than "Exact" for Interval Estimation of Binomial Proportions
#   Alan Agresti; Brent A. Coull
#   The American Statistician, Vol. 52, No. 2. (May, 1998), pp. 119-126.
#   http://www.stat.ufl.edu/~aa/articles/agresti_coull_1998.pdf
#
#   http://www.bmj.com/content/318/7177/193.3
#
#   http://influentialpoints.com/Training/confidence_intervals_of_proportions.htm

#source("~/Documents/DEC WORK/Shiny/R tools/R-tools/UsefulScripts/loadpackages.R", local = TRUE)
#######################################################

#################### ui ###############################
ui <- function(request) {fluidPage(


  titlePanel(h4("Visually explore the effects of sensitivity, specificity, and prevalence on True positives, False postives, False negatives, True negatives")),
  
  sidebarLayout(
    sidebarPanel(
     
      tags$h3("Input Variables"),
      sliderInput("n", "population", min=1, max=1000, value=100),
      sliderInput("prevalence", "prevalence of condition", min=0, max=1, value=0.3),
      sliderInput("mean1", "Mean of distribution 1", min=-1, max=1, value= 0, step = 0.1),
      sliderInput("mean2", "Mean of distribution 2", min=-1, max=1, value= 0, step = 0.1),
      sliderInput("sd1", "Standard deviation of distribution 1", min=0.05, max=0.25, value= 0.15),
      sliderInput("sd2", "Standard deviation of distribution 2", min=0.05, max=0.25, value= 0.15),
      sliderInput("threshold", "Threshold bar", min=-2, max=2, value= 0, step = 0.2),
  #  checkboxInput("sorted", label = "Population sorted by presence of condition and test result", value = FALSE),
  #  checkboxInput("ciFlag", label = "Show 95% confidence intervals (when sorted)", value = FALSE)
      bookmarkButton()
    ),
    mainPanel(
      plotOutput("distributionplots"),
      tags$br(),
#     verbatimTextOutput("dx2x2Table"),
#     tags$br(),
      tags$b("Cite as:"),
      tags$br(),
      "Joy Allen, Michael Power.",
      tags$br(),
      tags$em("A web app to explore prevalence, sensitivity, and specificity on Tp, Fp, Fn, and Tn"),
      tags$br(),
      "NIHR Diagnostic Evidence Co-operative Newcastle. August 2016",
      tags$br(),
      verbatimTextOutput("stats2")
    )
  )
)}

#######################################################

#################    server     ################

  server <- function(input, output, session) {

    Dpos <- reactive({round(input$n * input$prevalence)})
    Dneg <- reactive({round(input$n - Dpos())})
    

  distributiondf <- reactive({
    xdist <- seq(-2,2,length = 1000)
    ydist <- input$prevalence*dnorm(xdist,mean = input$mean1,sd = input$sd1)
    xdist2 <- seq(-2,2,length = 1000)
    ydist2 <- (1-input$prevalence)*dnorm(xdist2,mean = input$mean2,sd = input$sd2)
   
    return({data.frame(
      xdist = xdist, 
      ydist = ydist,
      xdist2 = xdist2,
      ydist2 = ydist2)
      })
    
  })
  
  # subset dataframe according to where the threshold is set
  # calculate AUC according to this threshold.  
  distri_threshold <- reactive({
  distri_thres1 <- subset(distributiondf(), xdist < input$threshold)
  distri_thres2 <- subset(distributiondf(), xdist < input$threshold)
  
  return({data.frame(
    Tp = round(input$n*(auc(distri_thres1$xdist, distri_thres1$ydist, type = 'spline'))), 
    Fp = input$prevalence*input$n - round(input$n*(auc(distri_thres1$xdist, distri_thres1$ydist, type = 'spline'))),
    Fn = round(input$n*(auc(distri_thres2$xdist2, distri_thres2$ydist2, type = 'spline'))),
    Tn = input$n*(1-input$prevalence) - round(input$n*(auc(distri_thres2$xdist2, distri_thres2$ydist2, type = 'spline')))
  )})
  
  })
  #
  
  output$distributionplots <- renderPlot({
   #  shade <- subset(distributiondf(), xdist > input$threshold)
    shade <-rbind(c(input$threshold,0), subset(distributiondf(), xdist > input$threshold), c(distributiondf()[nrow(distributiondf()), "X"], 0))
     shade2 <- rbind(c(input$threshold,0), subset(distributiondf(), xdist2 > input$threshold), c(distributiondf()[nrow(distributiondf()), "X2"], 0))
     distri <- ggplot(distributiondf(), aes(x = xdist2, y = ydist2)) +
       geom_polygon(data = shade2, aes(xdist2, ydist2), fill = "#E69F00")
     distri <- distri + geom_line(colour = "#E69F00")
     distri <- distri + geom_line(aes(x = xdist, y = ydist), colour =  "#999999") + geom_vline(xintercept = input$threshold) +
       geom_polygon(data = shade, aes(xdist, ydist), fill = "#999999") + 
               theme(axis.title.x=element_blank(),
              # axis.text.x=element_blank(),
              # axis.ticks.x=element_blank(),
               axis.title.y=element_blank(),
               axis.text.y=element_blank(),
               axis.ticks.y=element_blank())
  #  distri <- distri + 
  #    geom_text(data = distritext(), size = 5, aes(x = cmX, y = cmY, label = labs, colour = NULL, shape = NULL))
     distri
    
  })
  
  output$stats2 <- renderPrint((distri_threshold()))
 
  

}

shinyApp(ui, server, enableBookmarking = "url")
