library(PropCIs)
install.packages("binGroup", repos="http://cran.rstudio.com/") 
library("binGroup")

# function for calculating one sided confidence interval for sinple proportion
OneSidedBinCI <-  function(x, n, OneSidedConfLevel) {
  # x  observed counts
  # n <- 242 # total counts
  
  ll_binW <- binWilson(n, x, OneSidedConfLevel, alternative="less")
  ul_binW <- binWilson(n, x, OneSidedConfLevel, alternative="greater")
  
  OneSidedConfInt <- cbind(ll_binW[2], ul_binW[1], OneSidedConfLevel, x/n)
  OneSidedConfInt <- as.data.frame(OneSidedConfInt)
  colnames(OneSidedConfInt) <- c("CILL", "CIUL", "Level", "ObservedProp")
  
  return (list(lowerCI = round(OneSidedConfInt$CIUL*100,2), upperCI = round(OneSidedConfInt$CILL*100,2), proportion = round((x/n)*100,2)))
}


# the below calls the script and calculates the CIs for tables 4a&b
#Table 4a
OneSidedConfLevel <- 0.95
overallagree <- OneSidedBinCI(241,242, OneSidedConfLevel)
paste("greater than or equal to ", overallagree$lowerCI, "% for observed proportion ", overallagree$proportion, "%")
paste("less than or equal to ", overallagree$upperCI, "% for observed proportion ", overallagree$proportion, "%")

posagree <- OneSidedBinCI(36,40, OneSidedConfLevel)
paste("greater than or equal to ", posagree$lowerCI, "% for observed proportion ", posagree$proportion, "%")
paste("less than or equal to ", posagree$upperCI, "% for observed proportion ", posagree$proportion, "%")

negagree <- OneSidedBinCI(202,202, OneSidedConfLevel)
paste("greater than or equal to ", negagree$lowerCI, "% for observed proportion ", negagree$proportion, "%")
paste("less than or equal to ", negagree$upperCI, "% for observed proportion ", negagree$proportion, "%")


#Table 4b
overallagree <- OneSidedBinCI(242,242, OneSidedConfLevel)
paste("greater than or equal to ", overallagree$lowerCI, "% for observed proportion ", overallagree$proportion, "%")
paste("less than or equal to ", overallagree$upperCI, "% for observed proportion ", overallagree$proportion, "%")

posagree <- OneSidedBinCI(52,52, OneSidedConfLevel)
paste("greater than or equal to ", posagree$lowerCI, "% for observed proportion ", posagree$proportion, "%")
paste("less than or equal to ", posagree$upperCI, "% for observed proportion ", posagree$proportion, "%")

negagree <- OneSidedBinCI(190,190, OneSidedConfLevel)
paste("greater than or equal to ", negagree$lowerCI, "% for observed proportion ", negagree$proportion, "%")
paste("less than or equal to ", negagree$upperCI, "% for observed proportion ", negagree$proportion, "%")

