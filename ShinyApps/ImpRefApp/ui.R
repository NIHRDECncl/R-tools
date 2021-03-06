
# User interface for app to explore unceretainties when the reference standard is imperfect# 
#
# Conventions for naming variables
# Prefixes indicate the group the varible belongs to:
#    i - for variables related to the Index test
#    r - for variables related to the Reference test
#    g - for variables related to the Gold standard
# 
# Examples
#    irSens = the sensitivity of the Index test with the Reference test as the standard
#    rgSpec = the specificity of the Reference test, with the Gold standard as the standard
#    igNNT = the true NNT of Index test as it is compared to the Gold standard


ui <- function(request) {
  navbarPage(
    # about tab
    tabPanel("About", 
             hr(),
             tags$h3("under construction", style="color:red"),
             hr(),
             HTML(AboutHtml),
             tags$blockquote("this page will explain how to use the app to explore the various sources of uncertainty and their effects"),
             value = "About"),
    
    tabPanel("Inputs", 
             tags$style(HTML(".irs-slider {width: 1px; height: 15px; top: 15px;}")),
             
             fluidRow(
               column(8, actionButton("GoButton", "Click to recalculate the graphs after changing input data")),
              
               ######### bookmark button does not work ????????????????????
                column(4, bookmarkButton(
                 title = "Bookmark this application's state and get a URL for saving and sharing.",
                 id = "bookmark"))
             ),
             
             fluidRow(
               hr(),
               column(4, wellPanel(tags$b("Index test"),
                                   sliderInput("irSen", label = "\b \b measured sensitivity", value =c (0.7, 0.9), min = 0, max = 1, step = 0.01, width = "125%"),
                                   sliderInput("irSpec", label = "\b \b measured specificity", value = c(0.8, 0.95), min = 0, max = 1, step = 0.01)
               )),
               column(4, wellPanel(tags$b("Reference test"),
                                   sliderInput("rgSen", label = "\b \b \b \b guestimated sensitivity for PUA", value = c(0.60, 0.75), min = 0, max = 1, step = 0.01, width='100%'),
                                   sliderInput("rgSpec", label = "\b \b \b \b guestimated specificity for PUA", value = c(0.85, 0.99), min = 0, max = 1, step = 0.01, width='100%')
               )),
               
               column(4, wellPanel(tags$b("Index test - true accuracy"),
                                   sliderInput("igSen", label = "\b \b \b \b guestimated sensitivity for PUA of specificity", value = c(0.6, 0.8), min = 0, max = 1, step = 0.01, width='100%'),
                                   sliderInput("igSpec", label = "\b \b \b \b guestimated specificity for PUA of sensitivity", value = c(0.6, 0.75), min = 0, max = 1, step = 0.01, width='100%')
               ))
               
               ),
             
             fluidRow(
               column(4, wellPanel(tags$b("Lables and titles for graphs and tables: Overtype with:"),
                                   textInput("Title", label = NULL, value = "title for outputs", placeholder = "place holder"),
                                   textInput("IndexTest", label = NULL, value = "name of index test"),
                                   textInput("ReferenceTest", label = NULL, value = "name of reference test")
               )),
               
               column(4, wellPanel(tags$b(""),
                                   sliderInput("gPrevalence", label = "True prevalence (estimated range)", value = c(0.1, 0.25), min = 0, max = 1, step = 0.01),
                                   numericInput("nPrev", label = "Number of prevalences for PUA", value = 3, min = 1, max = 10, step = 1)
               )),
               
               column(4, wellPanel(tags$b(""),
                                   numericInput("nSamples", label = "Number of samples for probabilistic analysis of uncertainties", value = 10, min = 2, max = 1000, step = 1),
                                   numericInput("iPopulation", label = "study size (to calculate confidence intervals)", value = 100, min = 10, max = 1000, step = 1)
               ))
             ),
             value = "Inputs"),
    
    # tab for debugging outputs
    
    tabPanel("Debugging", 
             hr(),
             verbatimTextOutput("debug1"),
             hr(),
             verbatimTextOutput("debug2"),
             value = "Debugging"),
    
    
    # tab for tables for Index test (measured)
    navbarMenu("Tables",
               tabPanel("Index test measurments of diagnostic accuracy",
                        #div(id = "plot-container",
                        tags$h5("contingency matrix for index test"),
                        hr(),
                        tags$h3("under construction", style="color:red"),
                        hr(),
                        tags$blockquote("this page will have table of diagostic accuracy statistics"),
            
                        textOutput("ITtitle"),
                       
                        tableOutput("ITCMTable"),
                        hr(),
                        tags$h5("Diagnostic accuracy stats for index test"),
                        hr(),
                        tableOutput("ITStatsTable"),
                        value = "IT measurements"
               ),
               
               # tab for tables for Reference test (estimated)
               tabPanel(" +  Reference test guestimates of diagnostic accuracy",
                        #div(id = "plot-container",
                        tags$h5("contingency matrix for reference test"),
                        hr(),
                        tags$h3("under construction", style="color:red"),
                        hr(),
                        textOutput("RTtitle"),
                        withSpinner(tableOutput("RTCMTable")),
                        hr(),
                        tags$h5("Diagnostic accuracy stats for reference test"),
                        hr(),
                        tableOutput("RTStatsTable"),
                        value = "Reference test estimates"
               ),
               
               # tab for tables for Index test (adjusted)
               tabPanel(" -> Index test guestimates and estimates of true diagnostic accuracy",
                        #div(id = "plot-container",
                        tags$h5("contingency matrix for index test adjusted for imperfect reference test"),
                        hr(),
                        tags$h3("under construction", style="color:red"),
                        hr(),
                        tags$blockquote("this page will have table of diagostic accuracy statistics"),
                        
                        textOutput("ITAtitle"),
                        withSpinner(tableOutput("ITACMTable")),
                        hr(),
                        tags$h5("Diagnostic accuracy stats for adjusted index test"),
                        hr(),
                        tableOutput("ITAStatsTable"),
                        value = "IT adjustments"
               )
    ),
    
    # tab for graphs
    navbarMenu("Graphs",
               tabPanel("Effects of individual variables assuming statistical independence",
                        #div(id = "plot-container",
                        tags$h3("under construction", style="color:red"),
                        hr(),
                        tags$h4("Effects of individual variables assuming statistical independence", style="color:blue"),
                        
                        tags$p(""),
                        tags$h5("Given:"),
                        tags$li("Range of prevalences: gPrevLow – gPrev – gPrevHigh"),
                        tags$li("Index test’s measured sensitivity and specificity (with 95% CIs): irSen, irSpec"),
                        tags$li("Reference test’s guestimated sensitivity and specificity (with ranges): rgSen, rgSpec"),
                        tags$li("Index test’s guestimated true sensitivity and specificity (with ranges): igSen, igSpec"),
                        tags$li("Conditional independence of the results of the index and reference tests"),
                        tags$h5("Facet-plot line and ribbon graphs for the index test with the set of prevalences, (i) for true sensitivity given specificity, and (ii) for true specificity given sensitivity"),
                        tags$li("Y1-s = igSen and igPPV; or igSpec and igNPV:"),
                        tags$li("Y2-s = differentials:  (irSen – igSen) and (irPPV – igPPV); or (irSpec – igSpec) and (irNPV – igNPV)"),
                        tags$li("X1-s = reference test’s sensitivity, with ranges rgSenLow – rgSenHigh"),
                        tags$li("X2-s = reference test’s specificity, with ranges rgSpecLow – rgSpecHigh"),
                        hr(),
                        withSpinner(textOutput("graphs"), type = 6, size = 0.5),
                        value = "IT adjustments"
               ),
               # tab for graphs
               
               tabPanel("Effects of all variables assuming statistical independence",
                        #div(id = "plot-container",
                        tags$h3("under construction", style="color:red"),
                        hr(),
                        tags$h4("Effects of all variables assuming statistical independence", style="color:blue"),
                        tags$p(""),
                        tags$h5("Given:"),
                        tags$li("Range of prevalences: gPrevLow – gPrev – gPrevHigh"),
                        tags$li("Index test’s measured sensitivity and specificity (with 95% CIs): irSen, irSpec"),
                        tags$li("Reference test’s guestimated sensitivity and specificity (with ranges): rgSen, rgSpec"),
                        tags$li("Index test’s guestimated true sensitivity and specificity (with ranges): igSen, igSpec"),
                        tags$li("Conditional independence of the results of the index and reference tests"),
                        tags$li("Number of random samples to perform from each input variable’s PDF: nSamp"),
                        tags$p(""),
                        tags$p(" For each prevalence (gPrevLow – gPrev – gPrevHigh), and separately for the given example values of igSen and igSpec"),
                        tags$p("Facet-plot box and whisker plots for data from nSamp:"),
                        tags$li("Estimates: igSen, igSpec, igPPV, and igNPV"),
                        tags$li("Differentials:  (irSen – igSen) and (irPPV – igPPV); or (irSpec – igSpec) and (irNPV – igNPV)"),
                        hr(),
                        withSpinner(textOutput("graphs"), type = 5, size = 1),
                        value = "IT adjustments"
               ),
               # tab for graphs
               tabPanel("Overall uncertainties assuming statistical dependence",
                        #div(id = "plot-container",
                        tags$h3("under construction", style="color:red"),
                        hr(),
                        tags$h4("Overall uncertainties assuming statistical dependence", style="color:blue"),
                        tags$p(""),
                        tags$h5("Given:"),
                        tags$li("Range of prevalences: gPrevLow – gPrev – gPrevHigh"),
                        tags$li("Index test’s measured sensitivity and specificity (with 95% CIs): irSen, irSpec"),
                        tags$li("Reference test’s guestimated sensitivity and specificity (with ranges): rgSen, rgSpec"),
                        tags$li("Index test’s guestimated true sensitivity and specificity (with ranges): igSen, igSpec"),
                        tags$li("Conditional dependence of the results of the index and reference tests"),
                        tags$li("Probability distribution functions (PDFs) for each of the input variables, and assuming correlations = 0"),
                        tags$li("Number of random samples to perform from each input variable’s PDF: nSamp"),
                        tags$p(""),
                        tags$p(" For each prevalence (gPrevLow – gPrev – gPrevHigh), and separately for the given example values of igSen and igSpec"),
                        tags$p("Facet-plot box and whisker plots for data from Nsamp:"),
                        tags$li("Estimates: igSen, igSpec, igPPV, and igNPV"),
                        tags$li("Differentials:  (irSen – igSen) and (irPPV – igPPV); or (irSpec – igSpec) and (irNPV – igNPV)"),
                        
                        hr(),
                        withSpinner(textOutput("graphs"), type = 8, size = 2),
                        value = "IT adjustments"
               )
    ),
    
    column(12, hr(),
           tags$b("Cite as:"),
           tags$p("Michael Power, Joy Allen."),
           tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals"),
           tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. July 2017"),
           tags$br(),
           tags$img(src = urlNIHRlogo, width = "80px", height = "28px", align = "right")) # add the NIHR logo
    , title = "Explore uncertainties in diagnostic accuracy when evaluating with an imperfect reference")
}