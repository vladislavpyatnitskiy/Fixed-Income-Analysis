# Plot showing Duration Dependency on Frequency
duration.plt.frequency <- function(P, C, r = 0.05, y = 5, D = NULL){
  
  P. <- NULL
  
  for (f in seq(1, 12, 1)){ P. <- P * (1 + C/f)/(1 + r/f)^(y*f)
  
    PV <- NULL # Variable to store PV
    payments <- NULL # Variable to store payments
    
    for (n in 1:(y * f - 1)){ PV <- cbind(PV, C * P / f / (1 + r / f) ^ n * f) 
    
      payments <- cbind(payments, n * PV[n]) } # Coupon PV
  
    # Duration
    D <- rbind(D,(sum(payments[seq(y*f-1)])+P.*y*f)/(P.+sum(PV[seq(y*f-1)])))}
  
  plot(x = seq(1, 12, 1),
       y = D,
       type = "l",
       xlab = "Frequency",
       ylab = "Bond Duration",
       main = "Bond Duration Dependency on Frequency",
       sub = "Source: None",
       col = "red",
       lwd = 3,
       las = 1)
  
  axis(side = 1, at=seq(1, 12, 1)) # Axes
  axis(side = 2, at=seq(0, 100, 1), las = 1)
  
  abline(v = seq(1, 12, 1), lty = 3, col = "grey") # lines 
  abline(h = seq(1, 100, 1), lty = 3, col = "grey")
}
duration.plt.frequency(1000, 0.01, 0.1, 3) # Test
