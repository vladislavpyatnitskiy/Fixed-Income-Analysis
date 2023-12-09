# Create Table with Bond Parameters
B.table <- function(P, C, r, y, f = 1, s = 1, VaR = 95, PV = NULL, p = NULL){
  
  B <- P * (C/r * (1 - (1 + r/f)^-(y * f)) + (1 + r/f)^-(y * f)) # Bond Price
  
  # Duration & Modified Duration #
  P. <- P * (1 + C / f) / (1 + r / f) ^ (y * f) # Principle Part
  
  for (n in 1:(y*f-1)){ PV <- cbind(PV, C * P / f / (1 + r / f)^(n * f)) # NPV
  
    p <- cbind(p, n * PV[n]) } # # Coupon PV
  
  D <- (sum(p[seq(y*f - 1)]) + P.*y*f)/(P. + sum(PV[seq(y*f - 1)])) # Duration
  
  MD <- D / (1 + (r - s / 100) / f) # Modified Duration
  
  # Convexity #
  
  v <- NULL # List for all price values
  
  # Calculate all 3 prices; Product of maturity and repayment number
  for (n in seq(-1, 1, 1)){ d <- (1 + (r + s * n / 100) / f) ^ -y * f
  
    # Calculate price of bond and add result to list
    v <- cbind(v, P * (C / (r + s * n / 100) * (1 - d) + d)) }
  
  Cy <- (v[1] + v[3] - 2 * v[2]) / v[2] / (s / 100) ^ 2 # Convexity
  
  # Table with all parameters #
  l <- cbind(round(D,3), round(MD,2), round(Cy,2), B*qnorm(1 - VaR/100)*MD*r/f)
  
  # Column names 
  colnames(l) <- c("Duration", "Modified Duration", "Convexity", "VaR")
  
  return(l) # Display table
}
B.table(P = 1000, C = 0.1, r = 0.05, y = 3, f = 1, s = 1) # Test
