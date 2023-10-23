# Exponential Bond Calculator
B.calculator.exp <- function(P, C, r, ytm, f = 1){
  
  # Principal part
  P.exp <- P * exp(-r * ytm)
  
  # Number of coupon repayments
  repay <- ytm * f
  
  # Sum of coupons
  C.exp <- sum(((C * P) / f) * exp(-r * (seq(repay) / repay)))
  
  # Bond Value
  return(P.exp + C.exp)
}
# Test
B.calculator.exp(100, 0.06, 0.0676, 2, 2)
