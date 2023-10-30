# Exponential Bond Calculator
B.calculator.exp <- function(P, C, r, y, f = 1){ # Bond Price
  
  P * exp(-r * y) + sum(C * P/f * exp(-r * (seq(y * f) / y)))
}
# Test
B.calculator.exp(100, 0.06, 0.0676, 2, 2)
