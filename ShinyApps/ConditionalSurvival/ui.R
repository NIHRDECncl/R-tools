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
          
          actionLink("goToTabI2", "What will you do with information about your prognosis?"),
         #  
         # br(), br(),
         #  "Our aim is expressed by: ",
         # tags$a(href="https://www.ncbi.nlm.nih.gov/pubmed/15778417", "First do no harm"),
         #    "(which is often wrongly attributed to Hippocrates).",
          value = "i1"),

        tabPanel("What will you do with information about your prognosis?",
                 h6("under construction: Interpreting prognosis statistics"), 
                 tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
                 value = "i2"),

        tabPanel("How to use this app to understand and use information about prognosis",
                 h6("under construction: How to use this app to understand and use information about prognosis"), 
                 tags$style(type="text/css", "body {padding-top: 90px;}"), # padding to make room for fixed navbar
                 value = "i3")
        ),
    
      tabPanel("Facts",
      
     sidebarLayout(
       sidebarPanel(
        wellPanel(
          selectInput("condition", label = "choose condition", choices = c("Ovarian cancer", "condition b"), selected = "Ovarian cancer"),
          selectInput("prognosisPlot", label = "choose prognosis graph", choices = c("Ovarian cancer 10-yr overall survival (SEER 1988-2001)")),
          selectInput("conditionalSurvivalPlot", label = "choose conditional survival graph", choices = c("Ovarian cancer 5-yr conditional survival by stage (SEER 1988-2001)", "Ovarian cancer 5-yr conditional survival by age-group and stage (SEER 1988-2001)")),
          checkboxGroupInput("showUncertainties", label= "show uncertainties for:", choices = c("group averages" = "CI", "individual best and worst prospects" = "BW")),
          
          # checkboxGroupInput("pShowGroups", label= "Prognosis grouping", 
          #                    choiceNames = c("pGroup1", "pGroup2"),
          #                    choiceValues = c("pG1", "pG2")
          # ),
          # 
          # checkboxGroupInput("csShowGroups", label= "Conditional survival grouping", 
          #                    choiceNames = c("csGroup1", "csGroup2"),
          #                    choiceValues = c("csG1", "csG2")
          # ),
        hr(),
        checkboxInput("facetWrap", label = "Plot groups separately", value = FALSE)
        ),
        bookmarkButton(), " ...... ",
        actionButton("goPrint", "Download for printing")
      ),
      
      mainPanel(
        fluidRow(
          ##### prognosis plots
          hr(),
          column(2, br(), br(), br(), br(), br(), br(), 
                 textOutput("pText4Figure")),
          column(10, plotlyOutput("pPlot")),
          
          ##### conditional survival plots
          hr(),
          column(2, br(), br(), br(), br(), br(), br(), 
                 textOutput("csText4Figure")),
          column(10, plotlyOutput("csPlot")),
          hr()
          )
        ))),

    navbarMenu("Advice",

               tabPanel("view1 title contributor",
                        h6("under construction: contributor list 1")),

               tabPanel("view2 title contributor",
                        h6("under construction: contributor list 2"))),

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
               h6("under construction: Other support"))
  ),


 navbarMenu("Contribute",
            tabPanel("Contribute addotional data on survival",
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
    tags$em("A ShinyApp tool to show how survival statistics could be interpreted by patients"),
    hr(),
 id = "navbarPage", position = "fixed-top", selected = "i1"
  )}
