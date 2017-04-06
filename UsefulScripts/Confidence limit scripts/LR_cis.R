#Confidence limits for LR function
#from http://stats.stackexchange.com/questions/61349/how-to-calculate-the-confidence-intervals-for-likelihood-ratios-from-a-2x2-table
#Confidence Intervals for the Ratio of Two Binomial Proportions
#Author(s): P. A. R. Koopman
#Source: Biometrics, Vol. 40, No. 2 (Jun., 1984), pp. 513-517

# m is dataframe containing the 2x2 table

lr.ci <- function( m, sig.level=0.95 ) {
  
  alpha <- 1 - sig.level
  
  a <- m[1, 1]
  b <- m[1, 2]
  c <- m[2, 1]
  d <- m[2, 2]
  
  spec <- d/(b+d)
  sens <- a/(a+c)
  
  lr.pos <- sens/(1 - spec)  
  
  if ( a != 0 & b != 0 ) {
    
    sigma2 <- (1/a) - (1/(a+c)) + (1/b) - (1/(b+d))
    
    lower.pos <- lr.pos * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    
    upper.pos <- lr.pos * exp(qnorm(1-(alpha/2))*sqrt(sigma2)) 
    
    
  } else if ( a == 0 & b == 0 ) {
    
    lower.pos <- 0
    upper.pos <- Inf
    
  } else if ( a == 0 & b != 0 ) {
    
    a.temp <- (1/2)
    
    spec.temp <- d/(b+d)
    sens.temp <- a.temp/(a+c)
    
    lr.pos.temp <- sens.temp/(1 - spec.temp)  
    
    lower.pos <- 0
    
    sigma2 <- (1/a.temp) - (1/(a.temp+c)) + (1/b) - (1/(b+d))
    
    upper.pos <- lr.pos.temp * exp(qnorm(1-(alpha/2))*sqrt(sigma2))
    
  } else if ( a != 0 & b == 0 ) {
    
    b.temp <- (1/2)
    
    spec.temp <- d/(b.temp+d)
    sens.temp <- a/(a+c)
    
    lr.pos.temp <- sens.temp/(1 - spec.temp) 
    
    sigma2 <- (1/a) - (1/(a+c)) + (1/b.temp) - (1/(b.temp+d))
    
    lower.pos <- lr.pos.temp * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    
    upper.pos <- Inf  
    
  } else if ( (a == (a+c)) & (b == (b+d)) ) {
    
    a.temp <- a - (1/2)
    b.temp <- b - (1/2)
    
    spec.temp <- d/(b.temp+d)
    sens.temp <- a.temp/(a+c)
    
    lr.pos.temp <- sens.temp/(1 - spec.temp) 
    
    sigma2 <- (1/a.temp) - (1/(a.temp+c)) + (1/b.temp) - (1/(b.temp+d))
    
    lower.pos <- lr.pos.temp * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    
    upper.pos <- lr.pos.temp * exp(qnorm(1-(alpha/2))*sqrt(sigma2)) 
    
  }
  
  
  lr.neg <- (1 - sens)/spec
  
  if ( c != 0 & d != 0 ) {
    
    sigma2 <- (1/c) - (1/(a+c)) + (1/d) - (1/(b+d))
    
    lower.neg <- lr.neg * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    
    upper.neg <- lr.neg * exp(qnorm(1-(alpha/2))*sqrt(sigma2)) 
    
  } else if ( c == 0 & d == 0 ) {
    
    lower.neg<- 0
    upper.neg <- Inf
    
  } else if ( c == 0 & d != 0 ) {
    
    c.temp <- (1/2)
    
    spec.temp <- d/(b+d)
    sens.temp <- a/(a+c.temp)
    
    lr.neg.temp <- (1 - sens.temp)/spec.temp    
    
    lower.neg <- 0
    
    sigma2 <- (1/c.temp) - (1/(a+c)) + (1/d) - (1/(b+d))
    
    upper.neg <- lr.neg.temp * exp(qnorm(1-(alpha/2))*sqrt(sigma2))
    
  } else if ( c != 0 & d == 0 ) {
    
    d.temp <- (1/2)
    
    spec.temp <- d.temp/(b+d)
    sens.temp <- a/(a+c)
    
    lr.neg.temp <- (1 - sens.temp)/spec.temp  
    
    sigma2 <- (1/c) - (1/(a+c)) + (1/d.temp) - (1/(b+d))
    
    lower.neg <- lr.neg.temp * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    
    upper.neg <- Inf  
    
  } else if ( (c == (a+c)) & (d == (b+d)) ) {
    
    c.temp <- c - (1/2)
    d.temp <- d - (1/2)
    
    spec.temp <- d.temp/(b+d)
    sens.temp <- a/(a+c.temp)
    
    lr.neg.temp <- (1 - sens.temp)/spec.temp   
    
    sigma2 <- (1/c.temp) - (1/(a+c)) + (1/d.temp) - (1/(b+d))
    
    lower.neg <- lr.neg.temp * exp(-qnorm(1-(alpha/2))*sqrt(sigma2))
    
    upper.neg <- lr.neg.temp * exp(qnorm(1-(alpha/2))*sqrt(sigma2)) 
    
  }
  
  list(
    lr.pos=lr.pos, lower.pos=lower.pos, upper.pos=upper.pos,
    lr.neg=lr.neg, lower.neg=lower.neg, upper.neg=upper.neg
  )
  
}