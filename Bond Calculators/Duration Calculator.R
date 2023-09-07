# Function to calculate both Duration and modified duration of bond
bond_duration <- function(bond_principle,
                          bond_coupon_rate,
                          bond_interest_rate,
                          bond_year_to_mat,
                          bond_sensitivity = 1){
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
  
  # Round value for modified duration
  modified_duration <- round(duration_value /
    (1 + (bond_interest_rate - bond_sensitivity * 0.01)), 2)
  
  # Round value for duration
  duration_value <- round(duration_value, 3)
  
  # Put values into list
  bond_list <- cbind(duration_value, modified_duration)
  
  # Set column names 
  colnames(bond_list) <- c("Duration", "Modified Duration")
  
  # Display sentence
  return(bond_list)
}
# Test
bond_duration(1000, 0.1, 0.05, 3)
