# Create Table with Bond Parameters values
B.table <- function(P, C, r, ytm, f = 1, s = 1){
  
  # Maturity for loops
  ytm.a <- ytm - 1

  ### Duration Part ###
  
  # Calculate part for principle
  P.part <- (P * (1 + C / f)) / (1 + r) ^ ytm
  
  # Set up duration sum
  PV.sum <- NULL
  
  # Calculate PV of coupons
  for (n in 1:ytm.a){ PV.sum <- cbind(PV.sum, ((C * P) / f) / (1 + r) ^ n) }
  
  # Set up new list to contain
  payments <- NULL
  
  # Coupon part for numerator
  for (n in 1:ytm.a) { payments <- cbind(payments, n * PV.sum[n]) }
  
  # Duration
  D <- (sum(payments[seq(ytm.a)])+P.part*ytm)/(P.part+sum(PV.sum[seq(ytm.a)]))
  
  ### Convexity Part ###
  
  # Put all interest rate values into one vector
  r.Cy <- c(r, r - s * 0.01, r + s * 0.01)
  
  # Create an empty list to contain all price values
  v.Cy <- NULL
  
  # Calculate all 3 prices; Product of maturity and repayment number
  for (n in 1:3){ mat_x_rep <- (1 + 1 / (f / r.Cy[n])) ^ (ytm * f)
    
    # Calculate interest rate part
    r.part <- f / r.Cy[n] * (1 - 1 / mat_x_rep)
    
    # Calculate price of bond and add result to list
    v.Cy <- cbind(v.Cy, ((C * P) / f) * r.part + P / mat_x_rep) }
  
  # Calculate Convexity
  Cy <- (v.Cy[2] + v.Cy[3] - 2 * v.Cy[1]) / (v.Cy[1] * (s * 0.01) ^ 2)
  
  # Table formation #
  
  # Put values into list
  l.ratios <- cbind(round(D, 3), round(D/(1 + r - s * 0.01), 2), round(Cy, 2))
  
  # Set column names 
  colnames(l.ratios) <- c("Duration", "Modified Duration", "Convexity")
  
  # Display sentence
  return(l.ratios)
}
# Test
B.table(P = 1000, C = 0.1, r = 0.05, ytm = 3, f = 1, s = 1)
