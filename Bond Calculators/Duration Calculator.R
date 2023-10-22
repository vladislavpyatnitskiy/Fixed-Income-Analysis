# Function to calculate both Duration and modified duration of bond
bond_duration <- function(bond_principle,
                          bond_coupon_rate,
                          bond_interest_rate,
                          bond_year_to_mat,
                          bond_sensitivity = 1){
  # Maturity for loops
  ytm <- bond_year_to_mat - 1
  
  # Rate for nominator
  rt <- 1 + bond_interest_rate
  
  # Coupon calculation
  coupon_part <- bond_coupon_rate * bond_principle
  
  # Calculate part for principle
  principle_part <- (bond_principle + coupon_part) / (rt ^ bond_year_to_mat)
  
  # Multiply adjusted principle to number of YtM
  pv_principle <- principle_part * bond_year_to_mat
  
  # Set up duration sum
  pv_sum <- NULL
  
  # Calculate PV of coupons
  for (n in 1:ytm){ pv_sum <- cbind(pv_sum, coupon_part / rt ^ n) }
  
  # Add adjusted coupons sum to principle part to find out main denominator
  fin_dur_denom <- principle_part + sum(pv_sum[seq(ytm)])
  
  # Set up new list to contain
  pmnts_list <- NULL
  
  # Coupon part for numerator
  for (n in 1:ytm){ pmnts_list <- cbind(pmnts_list, n * pv_sum[n]) }
  
  # Sum cash flows to principle and round value for modified duration
  mod.dur <- round(((sum(pmnts_list[seq(ytm)]) + pv_principle)/fin_dur_denom)/
                     (1 +(bond_interest_rate - bond_sensitivity * 0.01)), 2)
  
  # Put values into list
  bond.list <- cbind(round(duration_value, 3), mod.dur)
  
  # Set column names 
  colnames(bond.list) <- c("Duration", "Modified Duration")
  
  # Display sentence
  return(bond.list)
}
# Test
bond_duration(1000, 0.1, 0.05, 3)
