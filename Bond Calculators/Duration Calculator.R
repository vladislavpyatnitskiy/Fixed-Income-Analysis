# Duration and Modified Duration
Duration <- function(P, C, r, y, f = 1, s = 1, PV = NULL, payments = NULL){
  
  P. <- P * (1 + C / f) / (1 + r / f) ^ (y * f) # Principle Part
  
  for (n in 1:(y*f-1)){ PV <- cbind(PV, C * P/f/(1 + r/f)^(n * f)) # NPV
  
    p <- cbind(p, n * PV[n]) } # # Coupon PV
  
  D <- (sum(p[seq(y*f - 1)]) + P.*y*f)/(P. + sum(PV[seq(y*f - 1)])) # Duration
  
  # Table with Duration and Modified Duration
  bond.list <- cbind(round(D, 3), round(D / (1 + (r - s * .01) / f), 2))
  
  colnames(bond.list) <- c("Duration", "Modified Duration") # Column names 
  
  return(bond.list) # Display sentence
}
# Test
Duration(1000, 0.1, 0.05, 3, 1, 1)
