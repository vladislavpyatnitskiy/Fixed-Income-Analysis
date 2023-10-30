# Create Table with Bond Parameters
B.table <- function(P, C, r, y, f = 1, s = 1, VaR = 95, PV = NULL, p = NULL){
  
  B <- P * (C/r * (1 - (1 + r/f)^-(y * f)) + (1 + r/f)^-(y * f)) #Bond Price
  
  # Duration & Modified Duration #
  P. <- P * (1 + C / f) / (1 + r / f) ^ (y * f) # Principle Part
  
  for (n in 1:(y*f-1)){ PV <- cbind(PV, C * P/f/(1 + r/f)^(n * f)) # NPV
  
  p <- cbind(p, n * PV[n]) } # # Coupon PV
  
  D <- (sum(p[seq(y*f - 1)]) + P.*y*f)/(P. + sum(PV[seq(y*f - 1)])) # Duration
  
  MD <- D / (1 + (r - s * .01) / f) # Modified Duration
  
  # Convexity #
  
  # Put all interest rate values into one vector
  r.Cy <- c(r / f, (r - s * 0.01) / f, (r + s * 0.01) / f)
  
  v.Cy <- NULL # Create an empty list to contain all price values
  
  # Calculate all 3 prices; Product of maturity and repayment number
  for (n in 1:3){ d <- (1 + r.Cy[n] / f) ^ (y * f)
  
  # Calculate price of bond and add result to list
  v.Cy <- cbind(v.Cy, P * (C / r.Cy[n] * (1 - 1 / d) + 1 / d)) }
  
  Cy <- (v.Cy[2] + v.Cy[3] - 2 * v.Cy[1]) / (v.Cy[1] * (s * .01)^2) # Convexity
  
  # Table with all parameters #
  l <- cbind(round(D,3), round(MD,2), round(Cy,2), B*qnorm(1-VaR*.01)*MD*r/f)
  
  # Set column names 
  colnames(l) <- c("Duration", "Modified Duration", "Convexity", "VaR")
  
  return(l) # Display table
}
# Test
B.table(P = 1000, C = 0.1, r = 0.05, y = 3, f = 1, s = 1)
