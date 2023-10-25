# Discounted Payback
DPR <- function(C, r, I){
  
  DP <- 0 # Define Payback Sum
  period <- 0 # and Payback Period
  
  # When Payback Sum < Initial Value add Cash Flow for each year
  while (DP <= I){ for (n in 1:length(C)){ DCF <- C[n] / (1 + r) ^ n
  
    DP <- DP + DCF # Payback Sum
  
  # When Payback Sum > Initial Value, calculate Payback Period and stop
  if (DP > I){ period <- (I - DP) / DCF + n
  
    break } } }
    
  # Put number into sentence and display value
  sprintf("The payback period is %s years", round(period, 2))
}
# Test
DPR(C = c(100, 100, 100, 100), r = 0.1, I = 200)
