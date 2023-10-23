# Cash flows
cash_flow_for_dpr <- c(100, 100, 100, 100)

# Discounted Payback
DPR <- function(C, r = 0.1, I = 200){
  
  # Define sum of paybacks and value to store period for paybacks
  DP <- 0
  period <- 0
  
  # Until sum of payback is less than initial value
  while (DP <= I){
    
    # For each payback calculate payback
    for (n in 1:length(C)){ DCF <- C[n] / (1 + r) ^ n
    
    # Sum paybacks
    DP <- DP + DCF
    
    # When sum of payback is more than initial value
    if (DP > I){
      
      # Get a decimal value and round value
      period <- ((I - (DP - DCF)) / DCF) + n - 1
      
      # Stop as we calculated value we need
      break } } }
  
  # Put number into sentence and display value
  return(sprintf("The payback period is %s years", round(period, 2)))
}
# Test
DPR(C = cash_flow_for_dpr, r = 0.1, I = 200)
