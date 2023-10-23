# Create Table with Bond Parameters values
B.table <- function(P, C, r, ytm, f = 1, s = 1){
  
  # Maturity for loops
  ytm.a <- ytm - 1
  
  # Rate for nominator
  rt <- 1 + r
  
  ### Duration Part ###
  
  # Coupon calculation
  C.part <- (C * P) / f
  
  # Calculate part for principle
  P.part <- (P + C.part) / rt ^ ytm
  
  # Multiply adjusted principle to number of YtM
  PV.P <- P.part * ytm
  
  # Set up duration sum
  PV.sum <- NULL
  
  # Calculate PV of coupons
  for (n in 1:ytm.a){ PV.sum <- cbind(PV.sum, C.part / rt ^ n) }
  
  # Set up new list to contain
  payments <- NULL
  
  # Coupon part for numerator
  for (n in 1:ytm.a) { payments <- cbind(payments, n * PV.sum[n]) }
  
  # Add adjusted coupons sum to principle part
  D <- (sum(payments[seq(ytm.a)]) + PV.P) / (P.part + sum(PV.sum[seq(ytm.a)]))
  
  ### Modified Duration Part ###
  
  # Bond Price when yield goes down by 1%
  p.change <- r - s * 0.01
  
  # Bond Price when yield goes up by 1%
  n.change <- r + s * 0.01
  
  # Round value for modified duration
  MD <- round(D / (1 + p.change), 2)
  
  ### Convexity Part ###
  
  # Put all interest rate values into one vector
  r.Cy <- c(r, p.change, n.change)
  
  # Create an empty list to contain all price values
  v.Cy <- NULL
  
  # Calculate all 3 prices; product of repayment number and rate
  for (n in 1:3){ repay <- f / r.Cy[n]
    
    # Product of maturity and repayment number
    mat_x_rep <- (1 + 1/repay) ^ (ytm * f)
    
    # calculate interest rate part
    r.part <- repay - f / (r.Cy[n] * mat_x_rep)
    
    # Calculate principle part
    P.part <- P / mat_x_rep
    
    # Calculate price of bond and add result to list
    v.Cy <- cbind(v.Cy, C.part * r.part + P.part)
  }
  # Calculate Convexity
  Cy <- (v.Cy[2] + v.Cy[3] - 2 * v.Cy[1]) / (v.Cy[1] * (s * 0.01) ^ 2)
  
  # End of Calculations #
  
  # Put values into list
  l.ratios <- cbind(round(D, 3), MD, round(Cy, 2))
  
  # Set column names 
  colnames(l.ratios) <- c("Duration", "Modified Duration", "Convexity")
  
  # Display sentence
  return(l.ratios)
}
# Test
B.table(P = 1000, C = 0.1, r = 0.05, ytm = 3, f = 1, s = 1)
