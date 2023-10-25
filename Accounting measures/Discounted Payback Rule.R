# Discounted Payback
DPR <- function(C, r, I, DP = 0, period = 0){
  
  # When Payback Sum < Initial Value add Cash Flow for each year
  while (DP <= I){ for (n in 1:length(C)){ DP <- DP + C[n] / (1 + r) ^ n
  
  # When Payback Sum > Initial Value, calculate Payback Period
  if (DP > I){ period <- (I - DP) / (C[n] / (1 + r) ^ n) + n
  
  break } } } # stop
  
  sprintf("Payback period is %s years", round(period, 2)) # Display value
}
# Test
DPR(C = c(100, 100, 100, 100), r = 0.1, I = 200)
