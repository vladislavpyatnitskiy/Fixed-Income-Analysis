# Bond Calculator
B.calculator <- function(P, C, r, ytm, f = 1){
  
  # Coupon part
  C.part <- (C * P) / f
  
  # Ratio between frequency and bond rate
  f.r <- f / r
  
  # Denominator
  d <- (1 + (1 / f.r)) ^ (ytm * f)
  
  # Rates part
  r.part <- f.r - f / (r * d)
  
  # Principle part
  P.part <- P / d
  
  # Connect all parts and display value
  return(C.part * r.part + P.part)
}
# Test it
B.calculator(1000, 0.08, 0.06, 30, 2)
