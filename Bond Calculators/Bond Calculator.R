# Bond Calculator
B.calculator <- function(P, C, r, ytm, f = 1){ 
  
  # Denominator
  d <- 1 / (1 + (1 / (f / r))) ^ (ytm * f)
  
  # Bond Price
  P * (C / f * (f / r * (1 - d)) + d)
}
# Test it
B.calculator(1000, 0.08, 0.06, 30, 2)
