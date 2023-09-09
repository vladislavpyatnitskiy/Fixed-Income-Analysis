# Create Table with Bond Parameters values
bond_param_table <- function(bond_principle,
                           bond_coupon_rate,
                           bond_interest_rate,
                           bond_year_to_mat,
                           n_an_b = 1,
                           bond_sensitivity = 1){
  
  ### Duration Part ###
  
  # Coupon calculation
  coupon_part <- bond_coupon_rate * bond_principle
  
  # Calculate part for principle
  principle_part <- ((bond_principle + coupon_part) / 
                       ((1 + bond_interest_rate)^bond_year_to_mat))
  
  # Multiply adjusted principle to number of YtM
  pv_principle <- principle_part * bond_year_to_mat
  
  # Set up duration sum
  pv_sum <- NULL
  
  # Calculate PV of coupons
  for (n in 1:(bond_year_to_mat-1)){
    # for each flow of cash
    duration_pv <- coupon_part / ((1 + bond_interest_rate) ^ n)
    
    # Put each flow of cash in list
    pv_sum <- cbind(pv_sum, duration_pv)
  }
  
  # Set list for adjusted coupons values to be added in denominator
  values_for_denominator <- 0
  
  # Coupon part for denominator
  for (n in 1:(bond_year_to_mat - 1)){
    values_for_denominator <- values_for_denominator + pv_sum[n]
  }
  
  # Main denominator for duration
  final_dur_denominator <- principle_part + 
    values_for_denominator
  
  # Set up new list to contain
  list_of_payments <- NULL
  
  # Coupon part for numerator
  for (n in 1:(bond_year_to_mat - 1)) {
    payments_sum <- n * pv_sum[n]
    
    list_of_payments <- cbind(list_of_payments, payments_sum)
  }
  
  # Define variable to contain adjusted coupons
  sum_of_duration_payments <- 0
  
  # Sum adjusted coupons
  for (n in 1:(bond_year_to_mat - 1)){
    sum_of_duration_payments <- sum_of_duration_payments + 
      list_of_payments[n]
  }
  
  # Sum cash flows to principle
  duration_value <- (sum_of_duration_payments + pv_principle) /
    final_dur_denominator
  
  ### Modified Duration Part ###
  
  # Round value for modified duration
  modified_duration <- round(duration_value /
                               (1 +
                                  (bond_interest_rate -
                                     bond_sensitivity * 0.01)),
                             2)
  
  # Round value for duration
  duration_value <- round(duration_value, 3)
  
  ### Convexity Part ###
  
  # Bond Price when yield goes down by 1%
  positive_b_chng <- bond_interest_rate - bond_sensitivity * 0.01
  
  # Bond Price when yield goes up by 1%
  negative_b_chng <- bond_interest_rate + bond_sensitivity * 0.01
  
  # Put all interest rate values into one vector
  bond_i_r <- c(bond_interest_rate,
                positive_b_chng,
                negative_b_chng)
  
  # Create an empty list to contain all price values
  bond_price_list <- NULL
  
  # For each bond price
  for (n in 1:3){
    
    # calculate coupon part
    coupon_part <- (bond_coupon_rate * bond_principle) / n_an_b
    
    # calculate interest rate part
    bond_rate_part <- ((n_an_b/(bond_i_r[n])) -
                         n_an_b/((bond_i_r[n]) *
                                   (1 + (bond_i_r[n])/n_an_b) ^
                                   bond_year_to_mat * n_an_b))
    # Calculate principle part
    principle_part <- bond_principle / ((1 + (bond_i_r[n])/n_an_b) ^
                                          bond_year_to_mat * n_an_b)
    # Calculate price of bond
    price_of_the_bond = coupon_part * bond_rate_part + principle_part
    
    # Add result to list
    bond_price_list <- cbind(bond_price_list,
                             price_of_the_bond)
  }
  # Current price of bond
  bond_now <- bond_price_list[1]
  
  # Bond price when yield goes down
  bond_up <- bond_price_list[2]
  
  # Bond price when yield goes up
  bond_down <- bond_price_list[3]
  
  # Calculate Convexity
  convexity_result <- round((bond_up + bond_down - 2 * bond_now) / 
    (bond_now * ((bond_sensitivity * 0.01) ^ 2)), 2)
  
  ### End of Calculations ### 
  
  # Put values into list
  bond_list <- cbind(duration_value,
                     modified_duration,
                     convexity_result)
  
  # Set column names 
  colnames(bond_list) <- c("Duration",
                           "Modified Duration",
                           "Convexity")
  
  # Display sentence
  return(bond_list)
}
# Test
bond_param_table(bond_principle = 1000,
                 bond_coupon_rate = 0.1,
                 bond_interest_rate = 0.05,
                 bond_year_to_mat = 3,
                 n_an_b = 1,
                 bond_sensitivity = 1)
