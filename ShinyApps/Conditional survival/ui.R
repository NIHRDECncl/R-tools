#################### ui for ShinyApp to explore the meaning of survival statistics ###############################

ui <- function(request) { 
  navbarPage("",
    navbarMenu("About",
        tabPanel("Introduction",
          h6("under construction: Introduction")
                        ),
         tabPanel("Interpreting prognosis statistics",
           h6("under construction: Interpreting prognosis statistics"))),       
      tabPanel("Explore prognosis",
      sidebarLayout(
       sidebarPanel(
        wellPanel(
          selectInput("condition", label = "Condition", choices = c("generic", "ovarian cancer: high grade, stage IIIc")),
          selectInput("outcome", label = "Outcome", choices = c("Survival", "Disease-free survical")),
          selectInput("group", label = "Subgroup", choices = c("age", "stage", "histology"))
        ),
        bookmarkButton(), " ...... ",
        actionButton("goPrint", "Print")
      ),
      mainPanel(
        fluidRow(
          column(6,
                  tags$br(),
                  tags$img(src = "Figure 1. Ten-year survival ovarian cancer.png",
                      width = "300px", height = "300px", align = "left")),
          column(6,
        tags$img(src = "Figure 2. Five-year conditional survival ovarian cancer.png", 
                 width = "300px", height = "300px", align = "left")))
      ))),
  navbarMenu("Viewpoints", 
            tabPanel("view1 title contributor",
                h6("under construction: contributor list 1")),
            tabPanel("view1 title contributor",
                h6("under construction: contributor list 2"))),
 navbarMenu("Contacts",
            tabPanel("Maintainers",
                 h6("under construction: maintainers")
            ),
            tabPanel("Developers",
                h6("under construction: developers"))),
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
            tabPanel("Conditions",
                     h4("Conditions worksheet"),
                     dataTableOutput("conditions")),
            tabPanel("Survival data",
                     h4("survivalData worksheet"),
                     dataTableOutput("survivalData")),
            tabPanel("Viewpoints",
                     h4("Viewpoints worksheet"),
                     dataTableOutput("Viewpoints"))),
 
    
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
  )}
