# Cash flows
cash_flow_for_dpr <- c(100, 100, 100, 100)

# Function to calculate using discounted payback rule
discounted_payback_rule <- function(x, y = 0.1, cf_0 = 200){
  
  # Define sum of paybacks and value to store period for paybacks
  discounted_payback_sum <- 0
  pb_num <- 0
  
  # Until sum of payback is less than initial values
  while (discounted_payback_sum <= cf_0){
    
    # For each payback calculate payback
    for (n in 1:length(x)){ dis_cash_flow <- x[n] / (1 + y) ^ n
      
      # Sum paybacks
      discounted_payback_sum <- discounted_payback_sum + dis_cash_flow
      
      # When sum of payback is more than initial value
      if (discounted_payback_sum > cf_0){
        
        # Get a decimal value and round value
        pb_num <- round(((cf_0 - (discounted_payback_sum - dis_cash_flow)) /
                           dis_cash_flow) + n - 1, 2)
        
        # Stop as we calculated value we need
        break } } }
  
  # Put number into sentence and display value
  return(sprintf("The payback period is %s years", pb_num))
}
# Test
discounted_payback_rule(x = cash_flow_for_dpr, y = 0.1, cf_0 = 200)
