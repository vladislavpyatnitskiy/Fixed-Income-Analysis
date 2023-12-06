# Relationship between rate and price
bond.y.plt <- function(P, C = 0, y, f = 1, l.bound = 0, h.bound = 100){
  
  l.B.prices <- NULL # Empty lists for prices
  
  # For each yield value add bond price value to list
  for (n in l.bound:h.bound){ d <- (1 + .01 * n / f) ^ -y * f
  
    l.B.prices <- c(l.B.prices, P * (C / .01 / n * (1 - d) + d)) }
  
  # Generate plot
  plot(x = as.vector(seq(l.bound, h.bound)),
       y = l.B.prices,
       type = "l",
       xlab = "Bond Yield (%)",
       ylab = "Bond Price",
       main = "Bond Pricing Dependency On Yield",
       col = "red",
       lwd = 3,
       las = 1)
  
  # Axes 
  axis(side = 1, at = seq(0, 100, 5))
  axis(side = 2, at = seq(100, 9000, 100), las = 1)
  
  # Dotted lines
  abline(v = seq(0, 100, 5), lty = 3, col = "grey") 
  abline(h = seq(0, 9000, 100), lty = 3, col = "grey")
}
bond.y.plt(P = 1000, C = .1, y = 10, f = 1, l.bound = 0, h.bound = 100) # Test
