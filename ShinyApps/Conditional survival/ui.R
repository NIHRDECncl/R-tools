#################### ui for ShinyApp to explore the meaning of survival statistics ###############################

ui <- function(request) { 
  navbarPage("",
    navbarMenu("Information",

        tabPanel("Do you want to know your prognosis?",
          h6("under construction: Do you want to know your prognosis?")),
        # renderText("test"),

        tabPanel("What will you do with information about your prognosis?",
                 h6("under construction: Interpreting prognosis statistics")),

        tabPanel("How to use this app to understand and use information about prognosis",
                 h6("under construction: How to use this app to understand and use information about prognosis"))
        ),
    
      tabPanel("Facts",
      
     sidebarLayout(
       sidebarPanel(
        wellPanel(
          selectInput("condition", label = "choose condition", choices = c("Ovarian cancer", "condition b"), selected = "Ovarian cancer"),
          selectInput("prognosisPlot", label = "choose prognosis graph", choices = c("Ovarian cancer 10-yr overall survival (SEER 1988-2001)")),
          selectInput("conditionalSurvivalPlot", label = "choose conditional survival graph", choices = c("Ovarian cancer 5-yr conditional survival by stage (SEER 1988-2001)", "Ovarian cancer 5-yr conditional survival by age-group and stage (SEER 1988-2001)")),
          checkboxGroupInput("showUncertainties", label= "show uncertainties", choices = c("in the average prognosis", "best and worst cases for individuals"), selected = NULL,
                             inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL),
          checkboxInput("facetPlot", label = "show separate plots", value = FALSE)
        ),
        bookmarkButton(), " ...... ",
        actionButton("goPrint", "Download for printing")
      ),
      
      mainPanel(
        fluidRow(
          ##### pronosis plots
          hr(),
          column(4, renderText("pText4Figure")),
          column(8, renderPlot("pPlot")),
          hr()
          ),
        
        fluidRow(
          ##### conditional survival plots
          hr(),  
          column(4, renderText("LegendConditionalPlot")),
          column(8, renderPlot("csPlot")),
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
                h6("under construction: licensing"))),

 navbarMenu("Outputs for debugging",
            tabPanel("Sheets",
                     h4("Worksheets in ConditionalSurvival.xlsx"),
                     dataTableOutput("sheets")),

            tabPanel("plots metadata",
                     h4("plotsMetadata worksheet"),
                     dataTableOutput("metadata4Plots")),

            tabPanel("Plots data",
                     h4("plotsData worksheet"),
                     dataTableOutput("data4Plots")),

            tabPanel("Prognosis plot data",
                     br("plot title"), textOutput("pPlot"),
                     br("plot choices"),textOutput("prognosisPlotChoices"),
                     br("plot choice"), textOutput("prognosisPlotChoice"),
                     dataTableOutput("pData")
                     ),
            
            tabPanel("Conditional plot data",
                     br("plot title"), textOutput("csPlot"),
                     br("plot choices"),textOutput("csPlotChoices"),
                     br("plot choice"), textOutput("csPlotChoice"),
                     dataTableOutput("pData")
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
    hr()
  ))}
