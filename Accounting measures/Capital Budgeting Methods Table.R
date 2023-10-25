# Table with capital budgeting methods
cash.flow.rates <- function(C, r, I){
  
  npv <- sum(C / (1 + r) ^ (seq(along = C) - 1)) # NPV

  irr <- function(C){ uniroot(NPV0, c(0, 1), C = C)$root } # IRR
  
  DP <- 0 # Sum of paybacks
  period <- 0 # Payback Period
  C.new <- C[2:length(C)] # Cash flow vector for payback and mirr
  
  # Until sum of payback < initial value
  while (DP <= I){ for (n in 1:length(C.new)){ DCF <- C.new[n] / (1 + r) ^ n
    
    DP <- DP + DCF # Sum paybacks
    
    if (DP>I){ period <- ((I-(DP-DCF))/DCF)+n-1 # when payback > initial value
      
      break } } } # End
  
  # MIRR
  mirr <- (sum(C.new*((1+r)^(length(C.new)-seq(C.new))))/I)^(1/length(C.new))-1
  
  a.ratios <- cbind(npv, irr, period, mirr) # Put values into list
  
  colnames(a.ratios) <- c("NPV", "IRR", "Payback", "MIRR") # Set column names 
  
  return(a.ratios) # Display sentence
}
# Test
cash.flow.rates(C = c(-800, 300, 300, 300, 300), r = 0.12, I = 800)