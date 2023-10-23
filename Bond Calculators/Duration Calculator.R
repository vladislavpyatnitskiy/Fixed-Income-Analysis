# Duration and Modified Duration
Duration <- function(P, C, r, ytm, s = 1){
  
  # Maturity for loops
  ytm.a <- ytm - 1
  
  # Rate for nominator
  rt <- 1 + r
  
  # Coupon calculation
  C.part <- C * P
  
  # Calculate part for principle
  P.part <- (P + C.part) / (rt ^ ytm)
  
  # Multiply adjusted principle to number of YtM
  PV.P <- P.part * ytm
  
  # Set up duration sum
  PV.sum <- NULL
  
  # Calculate PV of coupons
  for (n in 1:ytm.a){ PV.sum <- cbind(PV.sum, C.part / rt ^ n) }
  
  # Set up new list to contain
  payments <- NULL
  
  # Coupon part for numerator
  for (n in 1:ytm.a){ payments <- cbind(payments, n * PV.sum[n]) }
  
  # Duration
  D <- (sum(payments[seq(ytm.a)]) + PV.P) / (P.part + sum(PV.sum[seq(ytm.a)]))
  
  # Modified duration
  MD <- D / (1 + (r - s * 0.01))
  
  # Put values into list
  bond.list <- cbind(round(D, 3), round(MD, 2))
  
  # Set column names 
  colnames(bond.list) <- c("Duration", "Modified Duration")
  
  # Display sentence
  return(bond.list)
}
# Test
Duration(1000, 0.1, 0.05, 3)
