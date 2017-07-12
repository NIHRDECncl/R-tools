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
           h6("under construction: inputs"),
           actionButton("goPrint", "Print"),
           bookmarkButton()
         ),
        h5("under construction: choices for picklists will be read from a spreadsheet; when the choice is made, the input data will be dynakically populated"),
        wellPanel(
          selectInput("condition", label = "Condition", choices = c("generic", "ovarian cancer: high grade, stage IIIc")),
          selectInput("outcome", label = "Outcome", choices = c("Survival", "Disease-free survical")),
          selectInput("group", label = "Subgroup", choices = c("age", "stage", "histology"))
        )
      ),
      mainPanel(
        wellPanel(
                  h6("under construction: plots"),
                              
                  tags$br("lines A1, A2, A3  will show survival curves over the horizon for the different treatment strategies"),
                  tags$br("lines B2, B3  will show survival expectation curves over the horizon for the different treatment strategies"),
                  tags$br("lines will actually be ribbons to show uncertainties in predictions")
                            )
      )
      )
      ),
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
                     tableOutput("sheets")),
            tabPanel("Conditions",
                     tableOutput("conditions")),
            tabPanel("Survival data",
                     tableOutput("survivalData")),
            tabPanel("Viewpoints",
                     tableOutput("Viewpoints"))),
 
    
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
