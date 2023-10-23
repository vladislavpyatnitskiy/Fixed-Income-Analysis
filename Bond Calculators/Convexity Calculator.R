# Convexity
Convexity <- function(P, C = 0, r = 0, ytm, f = 1, s = 0.01){
  
  # Put all interest rate values into one vector
  r.v <- c(r, r - s, r + s)
  
  # Create an empty list to contain all price values
  B.list <- NULL
  
  # Calculate all 3 prices; payment frequency
  for (n in 1:3){ f.r <- f / r.v[n]
  
    # Denominator
    d <- (1 + 1 / f.r) ^ (ytm * f)
    
    # Coupon part
    C.part <- (C * P) / f
    
    # Rate part
    r.part <- f.r - f / (r.v[n] * d)
    
    # Principle part
    P.part <- P / d
    
    # Price of bond
    B.price = C.part * r.part + P.part
    
    # Add result to list
    B.list <- cbind(B.list, B.price) }
  
  # Convexity
  return((B.list[2] + B.list[3] - 2 * B.list[1]) / (B.list[1] * s ^ 2))
}
# Test
Convexity(P = 1000, C = 0.05, r = 0.08, ytm = 10, f = 1, s = 0.01)
