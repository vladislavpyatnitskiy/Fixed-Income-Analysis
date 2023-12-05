# Plot showing Duration dependency on Coupon Rate
duration.plt.coupon <- function(P, r = 0.05, y = 5, f = 1, D = NULL){
  
  P. <- NULL
  
  for (C in seq(0, 1, .01)){ P. <- P * (1 + C/f)/(1 + r/f)^(y*f)
  
    PV <- NULL # Variable to store PV
    payments <- NULL # Variable to store payments
    
    for (n in 1:(y * f - 1)){ PV <- cbind(PV, C * P / f / (1 + r / f) ^ n * f) 
    
      payments <- cbind(payments, n * PV[n]) } # Coupon PV
      
    # Duration
    D <- rbind(D,(sum(payments[seq(y*f-1)])+P.*y*f)/(P.+sum(PV[seq(y*f-1)])))}
  
  plot(x = seq(0, 1, .01) * 100,
       y = D,
       type = "l",
       xlab = "Coupon Rate (%)",
       ylab = "Bond Duration",
       main = "Bond Duration Dependency on Coupon Rate",
       sub = "Source: None",
       col = "red",
       lwd = 3,
       las = 1)
  
  axis(side = 1, at=seq(0, 100, 5)) # Axes
  
  abline(v = seq(0, 100, 5), lty = 3, col = "grey") # lines 
}
duration.plt.coupon(1000) # Test
