############################
#
#  OneCIprop function to calculate upper and lower bounds for 1-sided confidence intervals for a proportion
#
############################

OneCIprop <- function(x, n, CIlevel = 0.95) {
  # x = count (numerator)
  # n = total (denominator)
  # CIlevel = level of confidence interval, default of 95%
  
  # set inputs for testing
  # x <- 20
  # n <- 240
  # CIlevel <- 0.95
  
  
TwoSidedConfLevel <- 1 - (1 - CIlevel)*2 #  2-sided confidence interval for 1-sided CI calculation
 xCI <- PropCIs::scoreci(x, n, TwoSidedConfLevel)  # Wilson’s confidence interval for a single proportion

CI <- as.data.frame(xCI[1], stringsAsFactors = FALSE)

CIlevelPct <- round(CIlevel*100, digits = 0)

prop <- round(x/n, digits = 4)
lb <- round(CI$conf.int[1], digits = 4)
ub <- round(CI$conf.int[2], digits = 4)

OneCIub <- paste0(CIlevelPct, "% 1-sided CI upper bound: less than or equal to ", ub, " for observed proportion ", prop)
OneCIlb <- paste0(CIlevelPct, "% 1-sided CI lower bound: greater than or equal to ", lb, " for observed proportion ", prop)

OneSidedConfInt <- data.frame(CILL = CI$conf.int[1], CIUL = CI$conf.int[2], level = CIlevel, ObservedProp = x/n, CIlbtxt = OneCIlb, CIubtxt = OneCIub, stringsAsFactors = FALSE)

return(OneSidedConfInt)

}

############################

library(PropCIs)

oneCI <- OneCIprop(x = 36, n= 40, CIlevel = 0.95)
print(oneCI$CIlbtxt)
print(oneCI$CIubtxt)

