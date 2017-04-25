# script to calculate sample size

samplesize <- function(prev, SnI, CI, alpha, beta){

  z_a = qnorm(1-alpha/2, mean = 0, sd = 1, log = FALSE) # currently set up for two tail
  z_b = qnorm(1-beta, mean = 0, sd = 1, log = FALSE)
  
  
  n_obs = (z_a*sqrt(SnI*(1-SnI)) + z_b*sqrt((SnI-CI)*(1-(SnI-CI))))^2/CI^2
  n = n_obs/prev  
  
  s_size = 
    data.frame(
      Name = c("Number of events required", 
               "Sample size required"),
      Value = c(n_obs, 
                n))
  return(s_size)
}
