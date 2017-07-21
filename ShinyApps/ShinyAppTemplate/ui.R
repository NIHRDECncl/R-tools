#################### ui for ShinyApp to 
ui <- function(request) { 
  navbarPage("",
             tabPanel("Tab 0.1",
                      h6("under construction 0.1")),
      tabPanel("Inputs | Outputs",
      sidebarLayout(
       sidebarPanel(
         wellPanel(
           h6("under construction"),
           actionButton("go", "do something"),
           bookmarkButton()
         ),
         
        h5("under construction: "),
          sliderInput("slider", "label", min = 0, max = 100, value = c(15, 25), step = 1)
        ),
      mainPanel(
        wellPanel(
                  h6("under construction: "))
        )
      )),
navbarMenu("Menu1", 
            tabPanel("Tab 1.1",
                h6("under construction 1.1")),
            tabPanel("Tab 1.2",
                h6("under construction 1.2"))),
navbarMenu("Menu2",
            tabPanel("Tab 2.1",
                 h6("under construction 2.1")
            ),
            tabPanel("Tab 2.2",
                h6("under construction 2.2"))),
 
    
    
    ###################################
    #
    #     credits as a running footer
    #
    hr(),
    tags$br(),
    tags$b("Cite as:"),
    tags$p("Michael Power, Joy Allen."),
    tags$em("A ShinyApp tool to ..."),
    hr()
  )}
