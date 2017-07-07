#################### ui for ShinyApp to explore clinical accuracy and utility ###############################

ui <- function(request) {
  navbarPage(h4("Explore the clinically useful measures of test accuracy"),
    navbarMenu("Home",
    tabPanel("About", includeHTML("www/tab1.html")), 
    tabPanel("About graph 1", includeHTML("www/tab2.html")),
    tabPanel("About graph 2", includeHTML("www/tab3.html")),
    tabPanel("test",  
    sidebarPanel(
      sliderInput("obs", "Number of observations:",   min = 0, max = 1000, value = 500),
      actionButton("goButton1", "Go1!"),
      actionButton("goButton2", "Go2!")), 
    mainPanel(
      plotOutput("distPlot")
    ))
    ),
    
    tabPanel("Input labels, sensitivity, specificity, etc",
             fluidRow(
               column(4,actionButton("GoButton", "Update the graphs")),
               column(4, bookmarkButton())
             ),
             
             fluidRow(
               hr(),
               column(4, wellPanel(
                                   textInput(inputId = "DxCondition", label = "Name of condition", value = "Example: Disease"),
                                   textInput(inputId = "DxTestName", label = "Name of test", value = "Example: Test")
               )),
               column(4, wellPanel(
                                   numericInput("prevalence", "Prior probability (prevalence)", min=0, max=1, value=0.4, step = 0.01),
                                   numericInput("n", "Study size", min=1, max=1000, value=300)
               )),
               
               column(4, wellPanel(
                                   numericInput("sensitivity", "Sensitivity of test", min=0, max=1, value= 0.90, step = 0.01),
                                   numericInput("specificity", "Specificity of test", min=0, max=1, value= 0.80, step = 0.01)
               ))
             ),
               
               fluidRow( 
                 h5(" Data for clinical decisions"),
                 column(4, wellPanel(
                                     textInput(inputId = "DxRuleInDecision", label = "Rule-in decision", value = "Example: start treatment"),
                                     numericInput("RuleInDecisionThreshold", "Rule-in PPV threshold", min=0, max=1, value= 0.5, step = 0.01)
                 )),
                 
                 column(4, wellPanel(
                                     textInput(inputId = "IndeterminateDecision", label = "Indeterminate decision", value = "Example: investigate further")
                 )),
                 
                 column(4, wellPanel(
                                     textInput(inputId = "DxRuleOutDecision", label = "Rule-out decision", value = "Example: rule out the condition"),
                                     numericInput("RuleOutDecisionThreshold", "Rule-out NPV threshold", min=0, max=1, value= 0.1, step = 0.01)
                 ))
                 
                )
      ),
    
    navbarMenu("Clinical accuracy and utility",
               # need to put inputs in side bar panel
       
        tabPanel("Graph 1: clinical accuracy: pre- and post-test probabilities; predictive values",
                 sidebarPanel(
                   fluidRow(
                     column(4,actionButton("GoButton", "Update the graphs")),
                     column(4, bookmarkButton())
                   )
                 ),
                 mainPanel(     
                  conditionalPanel(
                     condition = "input.GoButton == 0",
                     h5("To see the graphs, go to the Inputs tab and click on the Update graphs button", style="color:red")
                    ),
                 conditionalPanel(
                     condition = "input.GoButton !== 0",
                     withSpinner(plotOutput("PrePostProb"))
                   )
        )),
        tabPanel("Graph 2: clinical utility: post-test probabilities and decision thresholds", 
                 sidebarPanel(
                   
                 ),
                 mainPanel(    
                conditionalPanel(
                   condition = "input.GoButton == 0",
                   h5("To see the graphs, go to the Inputs tab and click on the Update graphs button", style="color:red")
                 ),
               conditionalPanel(
                   condition = "input.GoButton !== 0",
                   withSpinner(plotOutput("RuleInOutPlot"))
               )
             ))),
    
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
      tags$p("Michael Power, Joy Allen."),
      tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals"),
      tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. July 2017"),
      tags$br(),
      tags$img(src = "nihr-logo.jpg", width = "80px", height = "28px", align = "right") # add the NIHR logo)
    
)}
