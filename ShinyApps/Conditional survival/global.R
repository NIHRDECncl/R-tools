###  a ShinyApp to visually explore conditional survival probabilities
# 
if( !exists("LoadPackages", mode="function")) source("FunctionsUsedByConditionalSurvivalApp.R")

LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)

# load data as global unreactive objects

path <- paste0(getwd(), "/data/ConditionalSurvival.xlsx")
# path <- paste0("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/Conditional survival", "/data/ConditionalSurvival.xlsx")
sheets <- data.frame(excel_sheets(path), stringsAsFactors = FALSE)
names(sheets) <- "sheets"

metadata4Plots <- read_excel(path, sheets$sheets[1])
data4Plots <- read_excel(path, sheets$sheets[2])

datasetChoices <- subset(metadata4Plots, plotNameAndDataset == prognosisPlotChoice)$dataset %>% 
  sort() %>% 
  unique()
                                        
conditionChoices <- metadata4Plots$condition %>% 
  sort() %>% 
  unique()

prognosisPlotChoices <- 
  subset(metadata4Plots, condition == conditionChoice & view == "Prognosis")$plotNameAndDataset %>% 
  sort()


                    
