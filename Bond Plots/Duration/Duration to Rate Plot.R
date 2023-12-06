# Plot showing relationship between Duration & Interest Rate
duration.plt.rate <- function(P, C, f = 1, y = 5, D = NULL){
  
  P. <- NULL
  
  for (r in seq(0, 1, .01)){ P. <- P * (1 + C/f)/(1 + r/f)^(y*f)
  
    PV <- NULL # Variable to store PV
    payments <- NULL # Variable to store payments
  
    for (n in 1:(y * f - 1)){ PV <- cbind(PV, C * P / f / (1 + r / f) ^ n * f) 
    
      payments <- cbind(payments, n * PV[n]) } # Coupon PV
  
    # Duration
    D <- rbind(D,(sum(payments[seq(y*f-1)])+P.*y*f)/(P.+sum(PV[seq(y*f-1)])))}
  
  plot(x = seq(0, 1, .01) * 100,
       y = D,
       type = "l",
       xlab = "Bond Interest Rate (%)",
       ylab = "Bond Duration",
       main = "Bond Duration Dependency to Interest Rate",
       sub = "Source: None",
       col = "red",
       lwd = 3,
       las = 1)
  
  axis(side = 1, at=seq(0, 100, 10)) # Axes
  
  abline(v = seq(0, 100, 10), lty = 3, col = "grey")
}
duration.plt.rate(1000, 0.01, 1, 2) # Test
