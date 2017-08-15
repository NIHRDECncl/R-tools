#################### ui for ShinyApp to explore clinical accuracy and utility ###############################

ui <- function(request) {

  navbarPage(h5("Visually explore the effects of sensitivity, specificity, 
                and prevalence on True positives, False postives, False negatives, 
                True negatives"),
  # navbarMenu("Home",
  
   tabPanel("About", includeHTML("www/tab1.html")),
  #  ),
   tabPanel("Test Accuracy",
             
                sidebarPanel(
                  fluidRow(
                  tags$h3("Input Variables"),
                  actionButton("GoButton", "Update the graphs", class = 'middleAlign'),
                  hr(),
                  numericInput("n", "population", min=1, max=1000, value=100),
                  numericInput("prevalence", "prevalence of condition", min=0, max=1, value=.2),
                  numericInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90),
                  numericInput("specificity", "specificity of index test", min=0, max=1, value= 0.80),
                  checkboxInput("sorted", label = "Population sorted by presence of condition and test result", value = FALSE),
                  checkboxInput("ciFlag", label = "Show 95% confidence intervals (when sorted)", value = FALSE)
                  )
                ),
                mainPanel(
                  span(textOutput("validtext"), style="color:red; font-style:italic"),
                  span(textOutput("plot1title"), style="color:#4169E1; font-weight:bold;font-size: 18px"),
                  withSpinner(plotOutput("populationPlot")), 
                  span(textOutput("plot2title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 18px"),
                  withSpinner(plotOutput("testedPlots"))
                  # would prefer for this to be in tabs.
                  #("test",
                  #tabPanel("Population", plotOutput("populationPlot"), plotOutput("testedPlots")),
                  #tabPanel("Distributions", plotOutput("distributionplots"))
                  #)
                  )
     ),
        
  
    tabPanel("2 x 2 table",
      column(12, align="center",
       span(textOutput("table1title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 20px"),
           tableOutput("dx2x2Table"),
       span(textOutput("table2title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 20px"),
           tableOutput("pvdf")
             )
             ),    
   
   tabPanel("Distributions",
             span(textOutput("plot3title"), align = "center", style="color:#4169E1; font-weight:bold;font-size: 18px"),
              withSpinner(plotOutput("distributionplots")),
             textOutput("distributiontext")
            ),

    tabPanel("Download summary report",
             p("This document contains all the tables and figures generated from your input data."),
             radioButtons('format', 'Please select the document format you require', 
                          c('PDF', 'HTML', 'Word'),
                          inline = TRUE),
             downloadButton('downloadReport', 'Download summary report'),
             br(), br(), 
             p("NB generating the document can take some time.")
    ),
  


###################################
#
#     credits as a running footer
#
      tags$br(),
      tags$b("Cite as:"),
      tags$p("Joy Allen, Sara Graziadio and Michael Power."),
      tags$em("A Shiny Tool to explore prevalence, sensitivity, and specificity on Tp, Fp, Fn, and Tn"),
      tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. July 2017"),
      tags$br(),
      tags$img(src = "nihr-logo.jpg", width = "80px", height = "28px", align = "right") # add the NIHR logo)
    
  )
  }
