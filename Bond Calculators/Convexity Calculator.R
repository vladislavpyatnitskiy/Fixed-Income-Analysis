# Function to calculate Bond Convexity
bond_convexity <- function(bond_principle,
                           bond_coupon_rate = 0,
                           bond_interest_rate = 0,
                           bond_year_to_mat,
                           n_an_b = 1,
                           bond_yd_chng = 0.01){
  
  # Bond Price when yield goes down by 1%
  positive_b_chng <- bond_interest_rate - bond_yd_chng
  
  # Bond Price when yield goes up by 1%
  negative_b_chng <- bond_interest_rate + bond_yd_chng
  
  # Put all interest rate values into one vector
  bond_i_r <- c(bond_interest_rate, positive_b_chng, negative_b_chng)
  
  # Create an empty list to contain all price values
  bp_list <- NULL
  
  # Calculate all 3 prices; product of repayment number and rate
  for (n in 1:3){ repay <- n_an_b / bond_i_r[n]
    
    # Product of maturity and repayment number
    mat_x_rep <- bond_year_to_mat * n_an_b
  
    # calculate coupon part
    coupon_part <- (bond_coupon_rate * bond_principle) / n_an_b
    
    # calculate interest rate part
    bond_rate_part <- repay - n_an_b/(bond_i_r[n] * (1 + 1/repay) ^ mat_x_rep)
    
    # Calculate principle part
    principle_part <- bond_principle / ((1 + 1 / repay) ^ mat_x_rep)
    
    # Calculate price of bond
    price_of_the_bond = coupon_part * bond_rate_part + principle_part
    
    # Add result to list
    bp_list <- cbind(bp_list, price_of_the_bond) }
  
  # Calculate Convexity
  convexity_result <- (bp_list[2] + bp_list[3] - 2 * bp_list[1]) /
    (bp_list[1] * bond_yd_chng ^ 2)
  
  # Display value
  return(convexity_result)
}
# Test
bond_convexity(bond_principle = 1000,
               bond_coupon_rate = 0.05,
               bond_interest_rate = 0.08,
               bond_year_to_mat = 10,
               n_an_b = 1,
               bond_yd_chng = 0.01)
