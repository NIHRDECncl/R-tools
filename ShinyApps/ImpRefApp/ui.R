
# This is the user-interface definition of a Shiny web application.
# 
#


# initialise text variables for the "about" tabs
#

# urlTab1 <- "https://onedrive.live.com/download?cid=B2035DBFA124EFE7&resid=B2035DBFA124EFE7%213330&authkey=AAyGG8EPuGbDSdc"
# tab1Html <- content(GET(urlTab1), "text", encoding = "ISO-8859-1")
# 
# urlTab2 <- "https://onedrive.live.com/download?cid=B2035DBFA124EFE7&resid=B2035DBFA124EFE7%213332&authkey=AEung92_Q6bRkaY"
# tab2Html <- content(GET(urlTab2), "text", encoding = "ISO-8859-1")
# 
# urlTab4 <- "https://onedrive.live.com/download?cid=B2035DBFA124EFE7&resid=B2035DBFA124EFE7%213333&authkey=AJcIpWL8ThA4eIg"
# tab4Html <- content(GET(urlTab4), "text", encoding = "ISO-8859-1")

urlNIHRlogo <- "https://qpk2dq-sn3302.files.1drv.com/y3mcTf14jWUWq2c18ry2kwc1vBkCZb_mj3ZTJ_v-9RU6km49qWK-kRM1c9RfAaCjaSIw5IA16oCqE-zy-d4MYPKQgBtoMX8FsXXk-50ePK1vyKoowy_Cd30vofQvNlzVICCiVTc4LFHRjmfvqlLTq7Gw7Rhqybf3j6pnwrn7W03PeI?"

spinner <- "https://onedrive.live.com/download?cid=B2035DBFA124EFE7&resid=B2035DBFA124EFE7%213204&authkey=AClmMWLejVuzT2k"


# conventions for naming variables
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
               tags$blockquote("this page will explain how to use the app to explore the various sources of uncertainty and their effects"),
               value = "About"),
      
      tabPanel("Inputs", 
               tags$style(HTML(".irs-slider {width: 1px; height: 15px; top: 15px;}")),
               
               fluidRow(
                 column(4, wellPanel(tags$b("Overtype with:"),
                                     textInput("Title", label = NULL, value = "title for outputs", placeholder = "place holder"),
                                     textInput("IndexTest", label = NULL, value = "name of index test"),
                                     textInput("ReferenceTest", label = NULL, value = "name of reference test")
                 )),
                 column(4, wellPanel(tags$b(""),
                                     sliderInput("gPrevalence", label = "prevalence", value = c(0.1, 0.25), min = 0, max = 1, step = 0.01),
                                     numericInput("iPopulation", label = "study size", value = 100, min = 10, max = 1000, step = 5)
                 ))),
               
               fluidRow(
                 column(4, wellPanel(tags$b("Index test --- measured values"),
                                     sliderInput("irSen", label = "\b \b measured sensitivity", value =c (0.7, 0.9), min = 0, max = 1, step = 0.01, width = "125%"),
                                     sliderInput("irSpec", label = "\b \b measured specificity", value = c(0.8, 0.95), min = 0, max = 1, step = 0.01)
                 )),
                 column(4, wellPanel(tags$b("Reference test"),
                                     sliderInput("rgSen", label = "\b \b \b \b estimated sensitivity", value = c(0.60, 0.75), min = 0, max = 1, step = 0.01, width='100%'),
                                     sliderInput("rgSpec", label = "\b \b \b \b estimated specificity", value = c(0.85, 0.99), min = 0, max = 1, step = 0.01, width='100%')
                 )),
                 
                 column(4, wellPanel(tags$b("Index test --- estimates of true values"),
                                     sliderInput("igSen", label = "\b \b \b \b estimated sensitivity", value = c(0.6, 0.8), min = 0, max = 1, step = 0.01, width='100%'),
                                     sliderInput("igSpec", label = "\b \b \b \b estimated specificity", value = c(0.6, 0.75), min = 0, max = 1, step = 0.01, width='100%')
                 ))      ),
               
               value = "Inputs"),
      
      
      # tab for tables for Index test (measured)
      navbarMenu("Tables",
      tabPanel("Index test measurments of diagnostic accuracy",
               #div(id = "plot-container",
               #tags$img(src = spinner, id = "loading-spinner"),
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
               #tags$img(src = spinner, id = "loading-spinner"),
               tags$h5("contingency matrix for reference test"),
               hr(),
               tags$h3("under construction", style="color:red"),
               hr(),
               tags$blockquote("this page will have table of diagostic accuracy statistics"),
               
               textOutput("RTtitle"),
               tableOutput("RTCMTable"),
               hr(),
               tags$h5("Diagnostic accuracy stats for reference test"),
               hr(),
               tableOutput("RTStatsTable"),
               value = "Reference test estimates"
      ),
      
      # tab for tables for Index test (adjusted)
      tabPanel(" -> Index test guestimates and estimates of true diagnostic accuracy",
               #div(id = "plot-container",
               #tags$img(src = spinner, id = "loading-spinner"),
               tags$h5("contingency matrix for index test adjusted for imperfect reference test"),
               hr(),
               tags$h3("under construction", style="color:red"),
               hr(),
               tags$blockquote("this page will have table of diagostic accuracy statistics"),
               
               textOutput("ITAtitle"),
               tableOutput("ITACMTable"),
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
               #tags$img(src = spinner, id = "loading-spinner"),
               textOutput("graphs"),
               hr(),
               tags$h3("under construction", style="color:red"),
               hr(),
               tags$blockquote("this page will have graphs of diagostic accuracy statistics"),
               
               hr(),
               value = "IT adjustments"
      ),
      # tab for graphs
      tabPanel("Effects of individual variables assuming statistical dependence",
               #div(id = "plot-container",
               #tags$img(src = spinner, id = "loading-spinner"),
               textOutput("graphs"),
               tags$h3("under construction", style="color:red"),
               hr(),
               tags$blockquote("this page will have graphs of diagostic accuracy statistics"),
               
               hr(),
               value = "IT adjustments"
      ),
      # tab for graphs
      tabPanel("Overall uncertainties assuming statistical dependence",
               #div(id = "plot-container",
               #tags$img(src = spinner, id = "loading-spinner"),
               textOutput("graphs"),
               hr(),
               tags$h3("under construction", style="color:red"),
               hr(),
               tags$blockquote("this page will have graphs of diagostic accuracy statistics"),
               
               value = "IT adjustments"
      )
      ),
    
    hr(),
    column(3, actionButton("GoButton", "Recalculate")),
    column(3, bookmarkButton(
      title = "Bookmark this application's state and get a URL for saving and sharing."),
      id = "bookmark"),
    column(12, hr(),
           tags$b("Cite as:"),
           tags$p("Michael Power, Joy Allen."),
           tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals"),
           tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. September 2016"),
           tags$br(),
           tags$img(src = urlNIHRlogo, width = "80px", height = "28px", align = "right")) # add the NIHR logo
    , title = "Explore uncertainties in diagnostic accuracy when evaluating with an imperfect reference")
}
