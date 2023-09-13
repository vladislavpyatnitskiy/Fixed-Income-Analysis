# Cashflows
cash_flow_for_dpr <- c(100, 100, 100, 100)

# Function to calculate using discounted payback rule
discounted_payback_rule <- function(x,
                                    y = 0.1,
                                    cf_0 = 200){
  # Define sum of paybacks
  discounted_payback_sum <- 0
  
  # Define a value to store period for paybacks
  pb_num <- 0
  
  # Until sum of payback is less than initial values
  while (discounted_payback_sum <= cf_0){
  
    # for each payback
    for (n in 1:(length(x))){
      
      # Calculate payback
      discounted_cash_flow <- (x[n]) / ((1 + y) ^ n)
      
      # Sum paybacks
      discounted_payback_sum <- discounted_payback_sum +
        discounted_cash_flow
      
      # When sum of payback is more than initial value
      if (discounted_payback_sum > cf_0){
        
        # Subtract a whole number
        payback_years <- n - 1
            
        # Get a decimal value
        pb_num <- ((cf_0 - (discounted_payback_sum - discounted_cash_flow)) /
                     (discounted_cash_flow)) +
          payback_years
            
        # Round value
        pb_num <- round(pb_num, 2)
        
        # Stop as we calculated value we need
        break
      }
    }
  }
  
  # Put number into sentence
  pb_num_disp <- sprintf("The payback period is %s years", pb_num)
  
  # Display value
  return(pb_num_disp)
}
# Test
discounted_payback_rule(x = cash_flow_for_dpr,
                        y = 0.1,
                        cf_0 = 200)
