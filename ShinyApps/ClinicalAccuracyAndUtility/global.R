###  a ShinyApp to visually explore the effects of sensitivity, specificity, and prevalence
###  on post-test probablities (clinical accuracy), and their relation to thresholds for rule-in and rule-out decision thresholds.
# 
if( !exists("LoadPackages", mode="function")) 
  source("FunctionsUsedByClinAccApp.R")

LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)
options(shiny.reactlog = TRUE) # True for debugging

