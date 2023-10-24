# Create Table with Bond Parameters
B.table <- function(P, C, r, ytm, f = 1, s = 1){
  
  # Duration #
  P.part <- (P * (1 + C / f)) / (1 + r) ^ ytm # Calculate part for principle
  
  # Duration sum and payments
  PV.sum <- NULL
  payments <- NULL
  
  # Calculate PV of coupons
  for (n in 1:(ytm-1)){ PV.sum <- cbind(PV.sum, ((C * P ) / f ) / (1 + r ) ^ n) 
    
    payments <- cbind(payments, n * PV.sum[n]) } # Coupon part for numerator
  
  # Duration
  D <- (sum(payments[seq(ytm-1)])+P.part*ytm)/(P.part+sum(PV.sum[seq(ytm-1)]))
  
  # Convexity #
  
  # Put all interest rate values into one vector
  r.Cy <- c(r, r - s * 0.01, r + s * 0.01)
  
  v.Cy <- NULL # Create an empty list to contain all price values
  
  # Calculate all 3 prices; Product of maturity and repayment number
  for (n in 1:3){ d <- (1 + 1 / (f / r.Cy[n])) ^ (ytm * f)
  
  # Calculate price of bond and add result to list
  v.Cy <- cbind(v.Cy, ((C * P) / f) * (f / r.Cy[n] * (1 - 1 / d)) + P / d) }
  
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
