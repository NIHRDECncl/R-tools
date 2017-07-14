#################### ui for ShinyApp to explore the meaning of survival statistics ###############################

ui <- function(request) { 
  navbarPage("",
    navbarMenu("Information",
               
        tabPanel("Do you want to know your prognosis?",
          h6("under construction: Do you want to know your prognosis?")),
        
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
          selectInput("prognosisPlot", label = "choose prognosis graph", choices = c("prognosis plot 1", "prognosis plot 2")),
          selectInput("conditionalSurvivalPlot", label = "choose conditional survival graph", choices = c("conditional survival 1", "conditional survival 2", "conditional survival 3")),
          checkboxGroupInput("showUncertainties", label= "Show uncertainties", choices = c("in the average prognosis", "best and worst cases for individuals"), selected = NULL,
                             inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL)
        ),
        bookmarkButton(), " ...... ",
        actionButton("goPrint", "Print")
      ),
      
      mainPanel(
        fluidRow(
          column(6, h5(renderText(output$LegendPrognosisPlot))),
          column(6, h5(renderText(output$LegendConditionalPlot))),
                    
        fluidRow(
          column(6,
                  tags$br(),
                  tags$img(src = "Figure 1. Ten-year survival ovarian cancer.png",
                      width = "300px", height = "300px", align = "left")),
          column(6,
        tags$img(src = "Figure 2. Five-year conditional survival ovarian cancer.png", 
                 width = "300px", height = "300px", align = "left")))
      )))),
    
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
                     dataTableOutput("plotsMetadata")),
            
            tabPanel("Plots data",
                     h4("plotsData worksheet"),
                     dataTableOutput("plotsData")),

    
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
