#################### ui for ShinyApp to explore the meaning of survival statistics ###############################

ui <- function(request) { 
  navbarPage("",
    navbarMenu("Information",
        tabPanel("Do you want to know your prognosis?", 
          tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
          includeHTML("www/Do you want to know your prognosis?.html"), 
          
          # I ran the statement below to get the HTML for a link to the next tab, 
          # and then copied and pasted it into the HTML file for this tab
          # The link works in rStudio, but not in the browser :-(
          
          "Go to ", actionLink("goToTabI2", "What is prognosis?"),
          value = "i1"),
        
        tabPanel("What is prognosis?",
                 includeHTML("www/What is prognosis?.html"), 
                 tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
                 "Go to ", actionLink("goToTabI3", "What will you do with information about your prognosis?"),
                 value = "i2"),
        
        tabPanel("How to use this app to understand and use information about prognosis",
                 includeHTML("www/How to use this app.html"), 
                 tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
                 "Go to ", actionLink("goToTabFacts", "Get the facts and see the uncertainties in prognosis"),
                 br(),
                 "Go to ", actionLink("goToTabI4", "Technical notes"),
                 value = "i3"),

        tabPanel("Technical notes on conditional survival and predictive intervals",
                 includeHTML("www/Technical notes on conditional survival and prediction intervals.html"), 
                 tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
                 "Go to ", actionLink("goToTabFacts", "Get the facts and see the uncertainties in prognosis"),
                 value = "i4")
        ),
    
      tabPanel("Facts",
      
     sidebarLayout(
       sidebarPanel(
        wellPanel(
          selectInput("condition", label = "choose condition", choices = c("Ovarian cancer", "condition b"), selected = "Ovarian cancer"),
          selectInput("prognosisPlot", label = "choose prognosis graph", choices = c("Ovarian cancer 10-yr overall survival (SEER 1988-2001)")),
          selectInput("conditionalSurvivalPlot", label = "choose conditional survival graph", choices = c("Ovarian cancer 5-yr conditional survival by stage (SEER 1988-2001)", "Ovarian cancer 5-yr conditional survival by age-group and stage (SEER 1988-2001)")),
          checkboxGroupInput("showUncertainties", label= "show uncertainties for:", choices = c(
            "group averages" = "CI", 
            "individual best and worst prospects" = "BW"
            )),
        hr(),
        checkboxInput("facetWrap", label = "Plot groups separately", value = FALSE)
        ),
        bookmarkButton(), " ...... ",
        actionButton("goPrint", "Download for printing"),
        hr(),
        h4("It may be necessary to scroll down to see the conditional survival graph")
      ),
      
      mainPanel(
        fluidRow( 
          
        ##### plot the usual minimum information given on prognosis
        hr(),
        column(2, h5("The usual minimal information given on prognosis")),
        column(8, plotOutput("minPlot"))),
        
        fluidRow(
                  
          ##### prognosis plots
          hr(),
          column(2, br(), br(), br(), br(), br(), br(), 
                 textOutput("pText4Figure")),
          column(10, plotlyOutput("pPlot")),
          p("."),
          h5("Click on a point in the chart above to see the uncertainties in predictions for an individual's:"),
          tags$p("(i) Prediction interval for survival rate: their chance of surviving for a given time."),
          tags$p("(ii) Prediction interval for life expectancy: their length of survival."),
          
          verbatimTextOutput("click"),
          
          ##### conditional survival plots
          hr(),
          column(2, br(), br(), br(), br(), br(), br(), 
            textOutput("csText4Figure")),
          column(10, plotlyOutput("csPlot")),
          h5("This chart shows how predicted survival (e.g. life expectancy) usually improves over time."),
          hr()
          )
        ))),

  navbarMenu("Experiences",

             tabPanel("view1 title contributor",
                h6("under construction: contributor list 1")),

                      tabPanel("view2 title contributor",
                h6("under construction: contributor list 2"))),

   navbarMenu("Acknowledgments",
            tabPanel("Developers",
                 h6("under construction: Developers")),

             tabPanel("Data providers",
                h6("under construction: Data providers")),

             tabPanel("Reviewers",
                h6("under construction: Reviewers")),

             tabPanel("Other support",
               h6("under construction: Other support")),
            
            tabPanel("References and Resources",
                     tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
                     includeHTML("www/References and resources.html") 
                     )
  ),


 navbarMenu("Contact/Contribute",
            tabPanel("Contact",
            tags$p("We are unfortunately unable to offer advice, but we are looking for personal experiences that would be useful to the app's visitors."),
            tags$p("We would also welcome contributions of data on more conditions, and suggestions for improving the presentation of prognostic data."),
            tags$p("So, if you would like to contribute a personal experience, data, or suggestions, please email us."),
            tags$br(),
            tags$p("Thank you"),
            tags$hr(),
            tags$blockquote("Michael.Power@ncl.ac.uk"),
            tags$hr(),
            tags$blockquote("Joy.Allen@ncl.ac.uk"),
            tags$hr()),
            
            tabPanel("Contribute additional data on survival",
               h5("under construction: contribute data"),
                h6("Contributors will submit data on a spreadsheet with"),
               h6("* everything in the data input section"),
              h6("* a bibliograpy with references to sources and links to them"),
               h6("* their contact details which will be published")
               ),
             tabPanel("Linking, licencing, and fair use",
                h6("under construction: licensing"))
   ),
 
 
 navbarMenu("Outputs for debugging",
            tabPanel("Sheets",
                     h4("Worksheets in ConditionalSurvival.xlsx"),
                     dataTableOutput("QAsheets")),

            tabPanel("plots metadata",
                     h4("plotsMetadata worksheet"),
                     dataTableOutput("QAmetadata4Plots")),

            tabPanel("Plots data",
                     h4("plotsData worksheet"),
                     dataTableOutput("QAdata4Plots")),

            
            tabPanel("QAprognosisPlotChoices",
                     h4("QAprognosisPlotChoices")
                     # dataTableOutput("QAprognosisPlotChoices")
                     ),
            
            tabPanel("QApData",
                     h4("QApData"),
                     dataTableOutput("QApData")),
            
            tabPanel("QAcsPlotChoices",
                     h4("QAcsPlotChoices"),
                     dataTableOutput("QAcsPlotChoices")),
            
            tabPanel("QAcsPlotChoice",
                     h4("QAcsPlotChoice"),
                     textOutput("QAcsPlotChoice")),
            
            tabPanel("QAcsData",
                     h4("QAcsData"),
                     dataTableOutput("QAcsData")),
   
            tabPanel("QA prognosis plot labels",
                     h4("QA prognosis plot labels"),
                     textOutput("QApXlab"),
                     textOutput("QApYlab"),
                     textOutput("QApPlotTitle"),
                     textOutput("QApLegendTitle"),
                     textOutput("QAptext4Figure"),
                     "show uncertainties: ",
                     textOutput("QAshowUncertainties"),
                     "QApGroup Names",
                     textOutput("QApGroup1Name"),
                     textOutput("QApGroup2Name"),
                     "QApCgroupChoicesN and V",
                     verbatimTextOutput("QApCgroupChoicesN"),
                     verbatimTextOutput("QApCgroupChoicesV")
                     )
            ),

            
            
            
    ###################################
    #
    #     credits as a running footer
    #
    hr(),
    tags$br(),
    tags$b("Cite as:"),
    tags$p("Michael Power, Joy Allen."),
    tags$em("A ShinyApp tool to explain prognosis: what it is, the large uncertainties in any number, and why you should hope for the best, plan for the worst, and act on the average."),
    hr(),
 id = "navbarPage", position = "fixed-top", selected = "i1"
  )}
