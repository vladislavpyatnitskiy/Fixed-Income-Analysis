# First project
cash_flow_for_mirr1 <- c(200, 400, 350, 300) 

# Second Project
cash_flow_for_mirr2 <- c(300, 300, 300, 300) # y = 0.12 cf_0 = 800

# function for MIRR
mirr_cash_flow <- function(x,
                           y = 0.1,
                           cf_0 = 1000){
  # Create variable for cash flow
  cash_flow_sum <- 0
  
  # For cash flow for each year
  for (n in 1:length(x)){
    
    # Calculate cash flow for period n
    cf_n <- (x[n]) * ((1 + y) ^ (length(x)-n))
    
    # Sum
    cash_flow_sum <- cash_flow_sum + cf_n
  }
  # Calculate MIRR
  mirr_value <- (cash_flow_sum / cf_0) ^ (1/length(x)) - 1
  
  # Round a value to display with a text
  mirr_value_for_text <- round(mirr_value, 4) * 100
  
  # When project is worth starting
  rec_to_start <- sprintf("MIRR is %s%%, what is above cost of capital. Start the project.",
                          mirr_value_for_text)
  
  # When project is not worth starting
  rec_not_to_start <- sprintf("MIRR is %s%%, what is below cost of capital. Do not start the project.",
                              mirr_value_for_text)
  
  # Recommendation whether to start a project or not
  if (mirr_value > y) { print(rec_to_start) } else { print(rec_not_to_start) }
}
# Test
mirr_cash_flow(x = cash_flow_for_mirr2,
               y = 0.12,
               cf_0 = 800)
