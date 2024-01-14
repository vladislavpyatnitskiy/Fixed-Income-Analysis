# Plot relationship between IRR and NPV
npv.irr.plt <- function(x, xlim = NULL, h = 0, col, lwd = 3, main = NULL){ 
  
  if (is.array(x)) { l <- NULL # Scenario for Multiple Cash Flows
  
    for (m in 1:ncol(x)){ s <- x[,m]
    
      v <- NULL # List for NPV values
      
      for (n in seq(0, 1, .01)){ v <- rbind(v, sum(s / (1 + n)^(seq(s) - 1))) }
      
      plt <-  plot(x = data.frame(as.matrix(seq(0, 1, .01)), v), xlab = "IRR",
                   ylab = "NPV", type = "l", xlim = xlim, main = main, las = 1,
                   col = col, lwd = lwd) # Plot 
      
      abline(h = h, col = "black", lwd = 1) # Break Even horizontal line
      
      l <- list(l, plt) } } else { v <- NULL # Scenario for one cash flow
    
      for (n in seq(0, 1, .01)){ v <- rbind(v, sum(x / (1 + n)^(seq(x) - 1))) }
    
      plt <-  plot(x = data.frame(as.matrix(seq(0, 1, .01)),v), xlab = "IRR",
                   ylab = "NPV", type = "l", xlim = xlim, main = main, las = 1,
                   col = col, lwd = lwd) # Plot 
      
      abline(h = h, col = "black", lwd = 1) }
}
npv.irr.plt(x=cbind(c(-1000, 250, 300, 360, 432), c(-800, 300, 300, 300, 150)),
            xlim = c(0, 1), h = 0, col = "red", lwd = 3,
            main="Relationship between NPV and IRR") # Test
