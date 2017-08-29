library("shiny")
library("shinydashboard")
library("ggplot2")
library(tidyverse)
setwd("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/ImperfectRefApp")

enableBookmarking(store = "url")
shinyApp(ui, server, enableBookmarking = "url")