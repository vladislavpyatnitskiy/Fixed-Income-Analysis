# Table with capital budgeting methods
cash.flow.rates <- function(CF, rt, project = NULL){ df.ratios <- NULL
  
  for (m in 1:ncol(CF)){ C <- CF[,m] #
    
    r <- rt[m] #
  
    npv <- sum(C/(1 + r)^(seq(C) - 1)) # NPV
  
    irr <- uniroot(function(C,r) (sum(C/(1+r)^(seq(C)-1))), c(0,1), C=C)$root
    
    DP <- 0 # Sum of paybacks
    period <- 0 # Payback Period
    I <- -C[1] # Initial Investment
    C1 <- C[2:length(C)] # Cash flow vector for payback and mirr
    
    # Until sum of payback < initial value (IV)
    while (DP <= I){ for (n in 1:length(C1)){ DP <- DP + C1[n]/(1 + r) ^ n
    
    if (DP > I){ period <- (I - DP) / (C1[n] / (1 + r) ^ n) + n # Payback > IV
    
    break } } } # End
    
    mirr <- (sum(C1*((1+r)^(length(C1)-seq(C1))))/I)^(1/length(C1))-1 # MIRR
    
    df.ratios <- rbind(df.ratios, cbind(npv, irr, period, mirr)) } 
    
  colnames(df.ratios) <- c("NPV", "IRR", "Payback", "MIRR") # Set column names 
  
  if (is.null(project)){ rownames(df.ratios) <- seq(ncol(CF)) } else {
    
    rownames(df.ratios) <- project } # Project names when typed
  
  return(df.ratios) # Display sentence
}
cash.flow.rates(CF = cbind(c(-800, 300, 300, 300, 300),
                            c(-1000, 250, 300, 360, 432),
                            c(-200, 100, 100, 100, 100),
                            c(-800, 300, 300, 300, 150)),
                 rt = c(0.12, 0.05, 0.1, 0.1),
                 project = c("Blue", "Red", "Green", "Yellow")) # Test
