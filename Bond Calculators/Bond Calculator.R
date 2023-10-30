# Bond Calculator
B.calculator <- function(P, C, r, y, f = 1, exp = F){ # Exponential Valuation
  
  if (isTRUE(exp)){ P * exp(-r * y) + sum(C * P/f * exp(-r * (seq(y * f) / y)))
    # Standard Valuation
  } else { P * (C / r * (1 - (1 + r / f) ^ -(y * f)) + (1 + r / f) ^ -(y * f))}
}
# Test it
B.calculator(1000, 0.08, 0.06, 30, 2, F)
