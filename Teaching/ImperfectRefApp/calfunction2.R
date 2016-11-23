# install shiny

# R script for the 2 x 2 table and estimation of the effect of an imperfect reference
# standard on the estimates of sens and spec of index test

# naming conventions for the suffix in 2x2 diagnostic accuracy statistics
##   2 => 2x2 table
##   A => actual (true) gold standard test
##   R => rusty reference test (with assumed 100% sensitivity and 100% specificty)
##   I => index (new) test
##   IR => index test compared to rusty reference test
##   IA => index test compared to actual (true) gold standard
##   RA => reference test compared to actual (true) gold standard
##   NRpos => Net reclassification of true positives: (Actual - Measured) true positives
##   NRneg => Net reclassification of true negatives: (Actual - Measured) true negatives


estdxacc2 <- function(SnRA, SpRA, prevA, SenSpecFlag, SenSpecIRAssumed, VarFlag, NRpos = 0, NRneg = 0){
  
  # assume population = 1000
  n = 1000

# 2x2 Dx stats for rusty reference standard compared with actual (true) gold standard
# The truth
  
  DposA = prevA*n  # Disease present
  DnegA = n - DposA   # Disease absent

  TpRA = DposA * SnRA # True positives for reference test compared to actual gold standard
  FpRA = DposA * (1 - SpRA)  # False positives for reference test compared to actual gold standard

  TnRA = DnegA * SpRA # True negatives for reference test compared to actual gold standard
  FnRA = DnegA * (1 - SnRA)  # False negatives for reference test compared to actual gold standard

  DposR = (TpRA + FpRA)
  DnegR = n - DposR
  prevR = DposR/n


########################################################################################
# 2x2 Dx stats for index test compared with rusty reference standard

# initialize independent (X) variables for graphs
  SnIRrange = c(seq(0,1,0.01))
  SnIR = SnIRrange
  SpIR = SnIR

# initialize dependent (Y) variables for graphs
  SnIA = SnIR
  SpIA = SpIR


########################################################################################
# 2x2 Dx stats for index test compared with actual (true gold) standard
  ### working with vectors here so we can work out the errors

  TpIA = SnIA * DposA  # True positives for index test compared with actual (true gold) standard
  FpIA = SnIA*(1 - DposA) # False positives for index test compared with actual (true gold) standard

  TnIA = SpIA * DnegA  # True negatives for index test compared with actual (true gold) standard
  FpIA = SpIA*(1 - DnegA) # False negatives for index test compared with actual (true gold) standard

  ErrorSpec = SpIA - SpIR #initialising these variables
  ErrorSen = SnIA - SnIR #initialising these variables
  
  VarGraphChoice = 0.15 # initial 15% variation in graphing choice
##################################################################################
  # 2x2 Dx stats for index test compared with rusty reference standard
  ### working with vectors here so we can work out the errors!


  if (SenSpecFlag == 1) {
    ## given Sensitivity of index test compared to actual (true gold) reference
    SpIR = rep(SenSpecIRAssumed,length(SnIRrange))
    
    TnIR <- DnegR*SpIR
    FpIR <- DnegR - TnIR
  
    TpIR <- SnIR * DposR  # True positives for index test compared with rusty reference standard

    SpIA <- SpIR
    SnIA <- (TpIR - (1-SpIA)*FpRA)/TpRA
 

######################################################################################
# errors in estimation of specificity of index test compared with rusty reference test
  ErrorSpec <- SpIA - SpIR
  ErrorSen <- SnIA - SnIR # will be no error in spec as we need to know it 
  
  #   net reclassification (Actual - Measured)
  NRpos <- TpIA - TpIR
  NRneg <- TnIA - TnIR
  
  } 
  else {
    ## given specificity of index test compared to actual (tru gold) reference
    SnIR = rep(SenSpecIRAssumed,length(SnIRrange))
    
    
    TpIR <- SnIR * DposR 
    # varying SpIR therefore need to read in something with SpIR
    TnIR <- SpIR*DnegR
    
    SnIA <- SnIR
    SpIR <- (TnIR - (1-SnIA)*FnRA)/TnRA  
    
   
######################################################################################
#
# errors in estimation of sensitivity of index test compared with rusty reference test
    ErrorSen <- SnIA - SnIR
    ErrorSpec <- SpIA - SpIR
    
    
    #   net reclassification (Actual - Measured)
    NRpos <- TpIA - TpIR
    NRneg <- TnIA - TnIR
  }

return(list(SnIRrange = SnIRrange, SnIR = SnIR, SpIR = SpIR, SnIA = SnIA, SpIA = SpIA, ErrorSpec = ErrorSpec, ErrorSen = ErrorSen, NRpos = NRpos, NRneg = NRneg))

 }                        
                         