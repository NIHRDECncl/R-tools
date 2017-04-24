#
# This is the user-interface definition of a Shiny web application. You can run
# the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#

library(shiny)
# Define UI for slider demo application
ui <- function(request) {
  fluidPage(
  h3("Effect of imperfect reference standard on estimated accuracy of the index test"),

  # naming conventions for the suffix in 2x2 diagnostic accuracy statistics
  ##   2 => 2x2 table
  ##   A => actual (true) gold standard test
  ##   R => rusty reference test (with assumed 100% sensitivity and 100% specificty)
  ##   I => index (new) test
  ##   IR => index test compared to rusty reference test
  ##   IA => index test compared to actual (true) gold standard
  ##   RA => rusty reference test compared to actual (true) gold standard


#Sidebar with sliders to explore assumptions
  fluidRow(
    # Actual sensitivity of rusty reference test
    column(
      4,
      
      # Choose to graph true versus measured sensitivity (or specificity) of index test given its specificity (or sensitivity)
      
      radioButtons(
        "SenSpecFlag",
        label = h5("For index test (actual vs measured)"),
        choices = list("Graph sensitivity & explore specificity" = 1, "Graph specificity & explore sensitivity" = 2),
        selected = 1,
        inline = FALSE
      ),
      
      # assumed actual (true) specificity-sensitivty of index test
      
      conditionalPanel(
        condition = "input.SenSpecFlag == 1",
        sliderInput(
        "SpecIRAssumed",
        "Index test: Actual specificity",
        min = 0,
        max = 1,
        value = 0.8,
        step = 0.01
      )),
      
      conditionalPanel(
        condition = "input.SenSpecFlag == 2",
        sliderInput(
          "SenIRAssumed",
          "Index test: Actual sensitivity",
          min = 0,
          max = 1,
          value = 0.8,
          step = 0.01
        )),    
      
      sliderInput(
        "SnRA",
        "Reference test: Actual sensitivity",
        min = 0,
        max = 1,
        value = .8,
        step = 0.01
      ),

      # Actual specificity of rusty reference test
      sliderInput(
        "SpRA",
        "Reference test: Actual specificity",
        min = 0,
        max = 1,
        value = .85,
        step = 0.01
      ),

      # Actual prevalence of condition
      sliderInput(
        "prevA",
        "Actual prevalence",
        min = 0,
        max = 1,
        value = .25,
        step = 0.01
      ),


    # choose 15% spread in either actual prevalence or actual specificity/sensitivity of index test
    
    radioButtons(
      "SpreadFlag",
      label = h5("Show variation with 15% change in:"),
      choices = list("Actual prevalence" = 1, "Actual spec/sens of index test" = 2),
      selected = 1,
      inline = FALSE
    ),
    bookmarkButton()
  ),
        # end column 4

    # Tabulate the assumptions and show the plots
    # column(
    #   8,
    #   box(height = 300, plotOutput("plot1")),
    #   box (title = h5(""), height = 300, plotOutput("plot2")),
    #   box(title = ""), # stop table overlaying graphs
    #   box(title = ""),#
    #   box("",
    #       title = h5("Net reclassification"), height =100, 
    #       tableOutput("formula1")),
    #   box("",
    #     title = h5("Assumptions on which the graphs depend"), height =100, 
    #   tableOutput("formula2"))
    #   
    # ) # end column 8
  
  column(4, plotOutput("plot1", dblclick =  "plot1_dblclick", brush = brushOpts(
    id = "plot1_brush",
    resetOnNew = TRUE))),
  column(4, plotOutput("plot2", dblclick =  "plot2_dblclick", brush = brushOpts(
    id = "plot2_brush",
    resetOnNew = TRUE))),
  column(4, plotOutput("plot3", dblclick =  "plot3_dblclick", brush = brushOpts(
    id = "plot3_brush",
    resetOnNew = TRUE))),
 # column(8, h4("Net reclassifications"), tableOutput("formula2")),
  column(4, h4("Assumptions on which the graphs depend"), tableOutput("formula1")),
  column(8, tags$b("Cite as:"),
          tags$br(),
         "Joy Allen, Michael Power. ", 
         tags$br(),
         tags$b(tags$em("A ShinyApp to explore uncertainties when evaluating a diagnostic test using an imperfect reference standard. ")), 
         tags$br(),
         "July 2016",
         tags$br(),
         "NIHR Diagnostic Evidence Co-operative Newcastle.", 
          tags$br(),
          tags$a(href="http://www.newcastle.dec.nihr.ac.uk/", "www.newcastle.dec.nihr.ac.uk")
         )
  
  
  
  ) # end fluidRow
) # end fluidPage
} # end shinyUI