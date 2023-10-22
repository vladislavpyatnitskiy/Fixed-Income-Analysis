# First project
cash_flow_for_mirr1 <- c(200, 400, 350, 300) 

# Second Project
cash_flow_for_mirr2 <- c(300, 300, 300, 300) # y = 0.12 cf_0 = 800

# function for MIRR
mirr_cash_flow <- function(x, y = 0.1, cf_0 = 1000){
  
  # Calculate MIRR
  mirr <- (sum(x*((1 + y)^(length(x) - seq(x))))/cf_0)^(1/length(x)) - 1
  
  # Recommendation whether to start a project or not
  if (mirr > y){ print(sprintf("MIRR is %s%% > cost of capital. Start project",
                                      round(mirr, 4) * 100)) } else {
    print(sprintf("MIRR is %s%% < cost of capital. Don't start",
                  round(mirr, 4) * 100)) }
}
# Test
mirr_cash_flow(x = cash_flow_for_mirr2, y = 0.12, cf_0 = 800)
