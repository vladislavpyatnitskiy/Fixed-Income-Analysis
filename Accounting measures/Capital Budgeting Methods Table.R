# Table with capital budgeting methods
cash.flow.rates <- function(C, r){ npv <- sum(C/(1 + r)^(seq(C) - 1)) # NPV

  irr <- uniroot(function(C, r) (sum(C/(1+r)^(seq(C)-1))), c(0, 1), C = C)$root
  
  DP <- 0 # Sum of paybacks
  period <- 0 # Payback Period
  I <- -C[1] # Initial Investment
  C1 <- C[2:length(C)] # Cash flow vector for payback and mirr
  
  # Until sum of payback < initial value (IV)
  while (DP <= I){ for (n in 1:length(C1)){ DP <- DP + C1[n]/(1 + r) ^ n
  
  if (DP > I){ period <- (I - DP) / (C1[n] / (1 + r) ^ n) + n # Payback > IV
  
  break } } } # End
  
  mirr <- (sum(C1*((1 + r)^(length(C1) - seq(C1))))/I)^(1/length(C1))-1 # MIRR
  
  a.ratios <- cbind(npv, irr, period, mirr) # Put values into list
  
  colnames(a.ratios) <- c("NPV", "IRR", "Payback", "MIRR") # Set column names 
  
  return(a.ratios) # Display sentence
}
cash.flow.rates(C = c(-800, 300, 300, 300, 300), r = 0.12) # Test
