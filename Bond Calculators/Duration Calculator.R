# Duration and Modified Duration
Duration <- function(P, C, r, ytm, f = 1, s = 1){
  
  P.part <- (P * (1 + C / f)) / (1 + r) ^ ytm # Calculate part for principle
  
  # Duration sum and payments
  PV.sum <- NULL
  payments <- NULL
  
  # Calculate PV of coupons
  for (n in 1:(ytm-1)){ PV.sum <- cbind(PV.sum, ((C * P ) / f ) / (1 + r ) ^ n) 
  
  payments <- cbind(payments, n * PV.sum[n]) } # Coupon part for numerator
  
  # Duration
  D <- (sum(payments[seq(ytm-1)])+P.part*ytm)/(P.part+sum(PV.sum[seq(ytm-1)]))
  
  # Table with Duration and Modified Duration
  bond.list <- cbind(round(D, 3), round(D / (1 + (r - s * 0.01)), 2))
  
  # Set column names 
  colnames(bond.list) <- c("Duration", "Modified Duration")
  
  # Display sentence
  return(bond.list)
}
# Test
Duration(1000, 0.1, 0.05, 3, 1, 1)
