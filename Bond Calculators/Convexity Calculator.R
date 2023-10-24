# Convexity
Convexity <- function(P, C = 0, r = 0, ytm, f = 1, s = 1){
  
  r.Cy <- c(r, r - s * 0.01, r + s * 0.01) # Put all rates into one vector
  
  v.Cy <- NULL # Create an empty list to contain all price values
  
  # Calculate all 3 prices; Product of maturity and repayment number
  for (n in 1:3){ d <- (1 + 1 / (f / r.Cy[n])) ^ (ytm * f)
  
    # Calculate price of bond and add result to list
    v.Cy <- cbind(v.Cy, P * (C * (1 / r.Cy[n] * (1 - 1 / d)) + 1 / d)) }
  
  # Convexity
  return(v.Cy[2] + v.Cy[3] - 2 * v.Cy[1]) / (v.Cy[1] * (s * 0.01) ^ 2)
}
# Test
Convexity(P = 1000, C = 0.1, r = 0.05, ytm = 3, f = 1, s = 1)
