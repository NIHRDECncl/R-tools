#################### ui ###############################

ui <- function(request) {
  fluidPage(
#  tags$head(tags$style(HTML(mycss))),
  titlePanel(h4("Evaluating a diagnostic test: clinical accuracy and clinical utility")),

  sidebarLayout(
    sidebarPanel(
      actionButton("GoButton", "Update the graph"),
      tags$br(),
      tags$h3("Input Variables"),
      textInput(inputId = "DxCondition", label = "Condition", value = "Example disease"),
      textInput(inputId = "DxTestName", label = "Name of test", value = "Example test"),
      numericInput("prevalence", "prior probability (prevalence)", min=0, max=1, value=0.4, step = 0.01),
      numericInput("n", "population", min=1, max=1000, value=300),
      numericInput("sensitivity", "sensitivity of index test", min=0, max=1, value= 0.90, step = 0.01),
      numericInput("specificity", "specificity of index test", min=0, max=1, value= 0.80, step = 0.01),
      textInput(inputId = "DxRuleInDecision", label = "Rule-in decision", value = "Example: start treatment"),
      numericInput("RuleInDecisionThreshold", "Rule-in PPV threshold", min=0, max=1, value= 0.5, step = 0.01),
      textInput(inputId = "IndeterminateDecision", label = "Indeterminate decision", value = "Example: investigate further"),
      textInput(inputId = "DxRuleOutDecision", label = "Rule-out decision", value = "Example: rule out the condition"),
      numericInput("RuleOutDecisionThreshold", "Rule-out NPV threshold", min=0, max=1, value= 0.1, step = 0.01),
      bookmarkButton()
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("About", includeHTML("www/tab1.html")), 
        tabPanel("About graph 1", includeHTML("www/tab2.html")), 
        tabPanel("Graph 1: clinical accuracy",
                 withSpinner(plotOutput("PrePostProb"))),
        tabPanel("About graph 2", includeHTML("www/tab3.html")), 
        tabPanel("Graph 2: clinical utility", #div(id = "plot-container",
                 withSpinner(plotOutput("RuleInOutPlot")))
        # tabPanel("Tables", dataTableOutput("linesTable"))
      ), 
      tags$br(),
      tags$b("Cite as:"),
      tags$p("Michael Power, Joy Allen."),
      tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals"),
      tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. July 2017"),
      #tgs$br(),
      tags$img(src = "nihr-logo.jpg", width = "80px", height = "28px", align = "right") # add the NIHR logo
    )
  )
)
}
