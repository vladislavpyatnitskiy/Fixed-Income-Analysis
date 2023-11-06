Convexity <- function(P, C = 0, r = 0, y, f = 1, s = 1){ # Convexity
  
  l <- NULL # List for all price values
  
  # Calculate all 3 prices; Product of maturity and repayment number
  for (n in seq(-1, 1, 1)){ d <- (1 + 1 / f * (r + s * n / 100)) ^ (y * f)
  
    # Calculate price of bond and add result to list
    l <- cbind(l, P * (C / (r + s * n / 100) * (1 - 1 / d) + 1 / d)) }
  
  (l[1] + l[3] - 2 * l[2]) / l[2] / (s / 100) ^ 2 # Convexity
}
Convexity(P = 1000, C = 0.1, r = 0.05, y = 3, f = 1, s = 1) # Test
