# First project
cash_flow_for_mirr1 <- c(200, 400, 350, 300) 

# Second Project
cash_flow_for_mirr2 <- c(300, 300, 300, 300) # y = 0.12 cf_0 = 800

# MIRR
MIRR <- function(C, r = 0.1, I = 1000){
  
  # MIRR
  mirr <- (sum(C * ((1 + r) ^ (length(C) - seq(C)))) / I) ^ (1 / length(C)) - 1
  
  # Recommendation
  if (mirr > r){ print(sprintf("MIRR is %s%% > cost of capital. Start project",
                               round(mirr, 4) * 100)) 
  } else { print(sprintf("MIRR is %s%% < cost of capital. Don't start",
                           round(mirr, 4) * 100)) }
}
# Test
MIRR(C = cash_flow_for_mirr2, r = 0.12, I = 800)
