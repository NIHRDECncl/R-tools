## global.R ##
enableBookmarking(store = "url")

# initialise text variables for the "about" tabs
#
urlAbout <- "ImpRefTabAbout1.html"
AboutHtml <- "html place holder until ''content(GET(urlAbout)''... works"
# AboutHtml <- content(GET(urlAbout), "text", encoding = "ISO-8859-1") # ERROR: Couldn't resolve host name
# 
# urlTab2 <- "https://onedrive.live.com/download?cid=B2035DBFA124EFE7&resid=B2035DBFA124EFE7%213332&authkey=AEung92_Q6bRkaY"
# tab2Html <- content(GET(urlTab2), "text", encoding = "ISO-8859-1")
# 
# urlTab4 <- "https://onedrive.live.com/download?cid=B2035DBFA124EFE7&resid=B2035DBFA124EFE7%213333&authkey=AJcIpWL8ThA4eIg"
# tab4Html <- content(GET(urlTab4), "text", encoding = "ISO-8859-1")

urlNIHRlogo <- "nihr_colour.jpg" # in ~/www/

urlSpinner <- "spinner.gif" # in ~/www/

# setwd("/Users/michaelpower/Google Drive/GIT-project/GitHub/R-tools/ShinyApps/ImpRefV2.0")
isolate({ source("FunctionsUsedByImpRefV2.R", local = TRUE)})
LoadPackages()
enableBookmarking("url")
options(shiny.error = browser)

# initialise Dx accuracy list for index test (measured), reference test, index test (true)
#
