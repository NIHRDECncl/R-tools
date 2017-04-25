library("shiny")
library("shinydashboard")
library("ggplot2")
library(tidyverse)
setwd("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/ImperfectRefApp")

enableBookmarking(store = "url")
shinyApp(ui, server, enableBookmarking = "url")


######
## to publish to shinyApp.io
## 1) comment out the runApp()
## 2) uncomment the next 2 lines
# library(rsconnect)
# rsconnect::deployApp("C:/Users/Michael Power/OneDrive/DEC/R tools/Shiny/ImpRefApp")
