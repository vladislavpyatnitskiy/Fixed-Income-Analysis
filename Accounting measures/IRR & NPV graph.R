# Plot relationship between IRR and NPV
npv.irr.plt <- function(x, xlim = NULL, h = 0, col, lwd = 3, main = NULL){ 
  
  v <- NULL # List for NPV values
  
  for (n in seq(0, 1, .01)){ v <- rbind(v, sum(x / (1 + n) ^ (seq(x) - 1))) }
  
  plot(x = data.frame(as.matrix(seq(0, 1, .01)),v), xlab = "IRR", ylab = "NPV",
       type="l", xlim=xlim, main = main, las = 1, col = col, lwd = lwd) # Plot 
  
  abline(h = h, col = "black", lwd = 1) # Make break even line more visible
}
npv.irr.plt(c(-800, 300, 300, 300, 150), xlim = c(0,1), h = 0, col = "red",
            lwd = 3, main = "Relationship between NPV and IRR") # Test
