# Bond Calculator
B.calculator <- function(P, C, r, ytm, f = 1, exp = F){ 
  
  # Exponential Valuation
  if (isTRUE(exp)){ P*exp(-r*ytm) + sum(((C*P)/f)*exp(-r*(seq(ytm*f)/(ytm*f))))
  # Standard Valuation
  } else { P * (C*(1/r*(1-1/(1+(1/(f/r)))^(ytm*f))) + 1/(1+(1/(f/r)))^(ytm*f))}
}
# Test it
B.calculator(1000, 0.08, 0.06, 30, 2, F)
