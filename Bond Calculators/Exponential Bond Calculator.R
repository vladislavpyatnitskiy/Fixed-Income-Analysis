# Exponential Bond Calculator
B.calculator.exp <- function(P, C, r, ytm, f = 1){
  
  # Bond Value
  P * exp(-r * ytm) + sum(((C * P) / f) * exp(-r * (seq(ytm * f) / (ytm * f))))
}
# Test
B.calculator.exp(100, 0.06, 0.0676, 2, 2)
