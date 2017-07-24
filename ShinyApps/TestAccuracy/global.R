###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
###  
# 
#if( !exists("LoadPackages", mode="function"))
source("FunctionsUsedByTestAccuracyApp.R")

LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)
options(shiny.reactlog = TRUE) # True for debugging

