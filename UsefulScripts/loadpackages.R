# R script to load packages
loadpackages <- function()
  
#install the packages you require
if (!require(plyr)){
    install.packages("plyr", repos="http://cran.rstudio.com/") 
    library("plyr")
}
#install the packages you require

if (!require(scales)){
  install.packages("scales", repos="http://cran.rstudio.com/") 
  library("scales")
}

if (!require(dplyr)){
  install.packages("dplyr", repos="http://cran.rstudio.com/") 
  library("dplyr")
}

if (!require(sqldf)){
  install.packages("sqldf", repos="http://cran.rstudio.com/") 
  library("sqldf")
}

if (!require(flexdashboard)){
  install.packages("flexdashboard", repos="http://cran.rstudio.com/") 
  library("flexdashboard")
}


if (!require("gridExtra")) {
  install.packages("gridExtra", repos="http://cran.rstudio.com/") 
  library("gridExtra")
}

if (!require("DT")) {
  install.packages("DT", repos="http://cran.rstudio.com/") 
  library("DT")
}

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}

if (!require("PropCIs")) {
  install.packages("PropCIs", repos="http://cran.rstudio.com/") 
  library("PropCIs")
}



if (!require("ggplot2")) {
  install.packages("ggplot2", repos="http://cran.rstudio.com/") 
  library("ggplot2")
}

if (!require("VennDiagram")) {
  install.packages("VennDiagram", repos="http://cran.rstudio.com/") 
  library("VennDiagram")
}

if (!require("rmeta")) {
  install.packages("rmeta", repos="http://cran.rstudio.com/") 
  library("rmeta")
}

if (!require("stringr")) {
  install.packages("stringr", repos="http://cran.rstudio.com/") 
  library("stringr")
}

if (!require("xtable")) {
  install.packages("xtable", repos="http://cran.rstudio.com/") 
  library("xtable")
}

if (!require("data.table")) {
  install.packages("data.table", repos="http://cran.rstudio.com/") 
  library("data.table")
}

if (!require("eeptools")) {
  install.packages("eeptools", repos = "http://cran.rstudio.com/")
  library("eeptools") 
}

if(!require("foreign")) {
  install.packages("foreign", repos="http://cran.rstudio.com/")
  library("foreign")
}
if(!require("Hmisc")) {
  install.packages("Hmisc", repos="http://cran.rstudio.com/")
  library("Hmisc")
}

if (!require(gamlss)){
  install.packages("gamlss", repos="http://cran.rstudio.com/") 
  library("gamlss")
}

if (!require("AUC")){
  install.packages("AUC", repos="http://cran.rstudio.com/")
  library("AUC")
}

if (!require("pracma")){
  install.packages("pracma", repos="http://cran.rstudio.com/")
  library("pracma")
}

if (!require("zoo")){
  install.packages("zoo", repos="http://cran.rstudio.com/")
  library("zoo")
}

if (!require("fitdistrplus")){
  install.packages("fitdistrplus", repos="http://cran.rstudio.com/")
  library("fitdistrplus")
}

if (!require(MESS)){
  install.packages("MESS", repos="http://cran.rstudio.com/") 
  library("MESS")
}


if (!require("Rcpp")){
  install.packages("Rcpp", repos="http://cran.rstudio.com/")
  library("Rcpp")
}

if (!require("e1071")){
  install.packages("e1071", repos="http://cran.rstudio.com/")
  library("e1071")
}

if (!require("readr")){
  install.packages("readr", repos="http://cran.rstudio.com/")
  library("readr")
}


if (!require("magrittr")){
  install.packages("magrittr", repos="http://cran.rstudio.com/")
  library("magrittr")
}

if (!require("shinyjs")){
install.packages("shinyjs", repos="http://cran.rstudio.com/")
  library("shinyjs")
}