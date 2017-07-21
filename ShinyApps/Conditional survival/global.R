###  a ShinyApp to visually explore conditional survival probabilities
# 
if( !exists("LoadPackages", mode="function")) source("FunctionsUsedByConditionalSurvivalApp.R")

LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)

# load data as global objects

                                        

# tidy formatting with: 
# library(formatR)
# tidy_source(source = "/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/ClinicalAccuracyAndUtility",
# comment = getOption("formatR.comment"),
#                     indent = getOption("formatR.indent", 2), 
#                     output = TRUE, 
#                     text = NULL, 
#                     width.cutoff = 80)
                    
