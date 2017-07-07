#################### ui for ShinyApp to explore the meaning of survival statistics ###############################

ui <- function(request) { 

  fluidPage(
    
    titlePanel(h4("Should survival statistics inspire fear? Or hope? Or realistic expectations?")),
    
    sidebarLayout(
      
      sidebarPanel(
        h5("under construction: choices for picklists will be read from a spreadsheet; when the choice is made, the input data will be dynakically populated"),
        selectInput("condition", label = "Condition", choices = c("generic", "ovarian cancer: high grade, stage IIIc")),
        selectInput("outcome", label = "Outcome", choices = c("Survival", "Disease-free survical")),
        selectInput("baseline for outcome", label = "Baseline for survival", choices = c("new diagnosis", "responded to treatment", "response to treatment poor", "relapse")),
        wellPanel(
          h6("Enter management strategies"),
          textInput("RxStrategy1", "", "Treatment strategy 1"),
          textInput("RxStrategy2", "", "Treatment strategy 2")
        ),
        wellPanel(
          selectInput("units", label = "Units for survival statistics", choices = c("years", "months", "days")),
          h5("under construction: ui will be constructed in server to dynamically update the labels for the following:"),
          h6("Survival estimates are a range of numbers, not a single number that would imply unwarrented certainty"),
          sliderInput("surviveHorizon", "Survival horizon", min = 0, max = 10, value = 5, step = 1),
          sliderInput("surviveNoRx", "% survival with no treatment", min = 0, max = 100, value = c(1, 5), step = 1),
          sliderInput("surviveRx1", "% survival with strategy 1", min = 0, max = 100, value = c(10, 20), step = 1),
          sliderInput("surviveRx2", "% survival with strategy 2", min = 0, max = 100, value = c(15, 25), step = 1)
        )
        ),
      
      mainPanel(
        navbarPage("",
          tabPanel("Graphs",
                   wellPanel(
                     h6("under construction"),
                    actionButton("goPrint", "Save file for printing"),
                    bookmarkButton()
                   ),
            wellPanel(
            h6("Plots under construction: "),
            
            tags$br("lines A1, A2, A3  will show survival curves over the horizon for the different treatment strategies"),
            tags$br("lines B2, B3  will show survival expectation curves over the horizon for the different treatment strategies"),
            tags$br("lines will actually be ribbons to show uncertainties in predictions")
          )),
          tabPanel("How to use this tool", 
                   h6("under construction"),
                   h6("An explantion for users : ")), 
          navbarMenu("Contacts",
                     tabPanel("Maintainers",
                              h6("under construction")
                     ),
                     tabPanel("Developers",
                              h6("under construction")
                     )
                     ),
          navbarMenu("More",
                     tabPanel("Sources of data",
                              h6("For the condition chosen: the sources, links to sources, contributor contact details")
                              ),
                     tabPanel("Contribute addotional data on survival",
                              h5("under construction"),
                              h6("Contributors will submit data on a spreadsheet with"),
                              h6("* everything in the data input section"),
                              h6("* a bibliograpy with references to sources and links to them"),
                              h6("* their contact details which will be published")
                     ),
                     tabPanel("Linking, licencing, and fair use",
                              h6("under construction")
                              ))
        )
      )),
  

###################################
#
#     credits as a running footer
#
      hr(),
      tags$br(),
      tags$b("Cite as:"),
      tags$p("Michael Power, Joy Allen."),
      tags$em("A ShinyApp tool to show how survival statistics could be interpreted by patients")
      
)}
