
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


shinyUI(fluidPage(
  # tags$head(tags$style(HTML(mycss))),
  # Application title
  titlePanel(h4("App to explore uncertainties due to using an imperfect reference standard")),
 
  tabsetPanel(
    tabPanel("About", value = "About"),
    tabPanel("Index test measurements", 
             #div(id = "plot-container",
             #tags$img(src = spinner, id = "loading-spinner"),
             tags$h5("contingency matrix for index test"), 
             textOutput("ITtitle"),
             tableOutput("ITCMTable"),
             hr(),
             tags$h5("Diagnostic accuracy stats for index test"), 
             hr(),
            tableOutput("ITStatsTable"),
            value = "IT measurements"
           ),
    tabPanel(" + Reference test estimates",
         #div(id = "plot-container",
         #tags$img(src = spinner, id = "loading-spinner"),
         tags$h5("contingency matrix for reference test"),
         textOutput("RTtitle"),
         tableOutput("RTCMTable"),
         hr(),
         tags$h5("Diagnostic accuracy stats for reference test"),
         hr(),
         tableOutput("RTStatsTable"),
         value = "Reference test estimates"
        ),
tabPanel(" -> Index test adjustments",
         #div(id = "plot-container",
         #tags$img(src = spinner, id = "loading-spinner"),
         tags$h5("contingency matrix for index test adjusted for imperfect reference test"),
         textOutput("ITAtitle"),
         tableOutput("ITACMTable"),
         hr(),
         tags$h5("Diagnostic accuracy stats for adjusted index test"),
         hr(),
         tableOutput("ITAStatsTable"),
         value = "IT adjustments"
         ),
tabPanel("Graphs",
         #div(id = "plot-container",
         #tags$img(src = spinner, id = "loading-spinner"),
         textOutput("graphs"),
         hr(),
         value = "IT adjustments"
         )
  ),

  fluidPage(
    fluidRow(
      column(2, wellPanel(tags$b("Overtype with:"),
        textInput("Title", label = NULL, value = "title for outputs", placeholder = "place holder"),
        textInput("IndexTest", label = NULL, value = "name of index test"),
        textInput("ReferenceTest", label = NULL, value = "name of reference test")
        )),
      column(2, wellPanel(tags$b(""),
        numericInput("Prevalence", label = "prevalence", value = 0.1, min = 0, max = 1, step = 0.01),
        numericInput("Population", label = "study size", value = 100, min = 10, max = 1000, step = 5)
        )),
        column(3, wellPanel(tags$b("Index test"),
        numericInput("ITsenMeas", label = "\b \b measured sensitivity", value = 0.9, min = 0, max = 1, step = 0.01, width = "125%"),
        numericInput("ITspecMeas", label = "\b \b measured specificity", value = 0.9, min = 0, max = 1, step = 0.01)
        )),
      column(5, wellPanel(tags$b("Reference test"),
        sliderInput("RTsenEst", label = "\b \b \b \b estimated sensitivity", value = c(0.8, 0.9), min = 0, max = 1, step = 0.01),
        sliderInput("RTspecEst", label = "\b \b \b \b estimated specificity", value = c(0.8, 0.9), min = 0, max = 1, step = 0.01)
        ))      )),
    hr(),
  column(3, actionButton("GoButton", "Recalculate")),
  column(3, bookmarkButton(title = "Bookmark this application's state and get a URL for saving and sharing.")),
  column(12, hr(),
    tags$b("Cite as:"),
    tags$p("Michael Power, Joy Allen."),
    tags$em("A ShinyApp tool to explore dependence of rule-in and rule-out decisions on prevalence, sensitivity, specificity, and confidence intervals"),
    tags$p("NIHR Diagnostic Evidence Co-operative Newcastle. September 2016"),
    tags$br(),
    tags$img(src = urlNIHRlogo, width = "80px", height = "28px", align = "right")) # add the NIHR logo
  )
)

