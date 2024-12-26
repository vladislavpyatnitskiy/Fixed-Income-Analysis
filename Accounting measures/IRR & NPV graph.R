npv.irr.plot <- function(C, max = 0.2){ # IRR & NPV graph
  
  NPV <- function(CF, R){ sapply(R, function(r) sum(CF/(1 + r)^(seq(CF)-1))) }
  
  R <- seq(0, max, 0.01) # Define discount rates
  
  if (is.matrix(C)) { # Multiple Cash Flow Scenario
    
    matplot(R, apply(C, 2, NPV, R = R), type = "l", xlab = "IRR", ylab = "NPV",
            col = 1:ncol(C), lwd = 3, lty = 1,
            main = "Relationship between NPV and IRR", las = 1
    )
    legend("topright", legend = paste("Scenario",1:ncol(C)), col=1:ncol(C),
           lty = 1, lwd = 3, bty = "n")
    
  } else { # Single cash flow scenario
    
    plot(R, NPV(C, R), type = "l", xlab = "IRR", ylab = "NPV", col = "red",
         lwd=3, main = "Relationship between NPV and IRR", las=1)
  }
  abline(h = 0, col = "black", lwd = 1) # Add horizontal line at NPV = 0
  
  axis(side = 4, las = 2) # Right y-axis
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
    
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd=1) # grid lines
}
# Test the function
npv.irr.plot(C = cbind(c(-1000, 250, 300, 360, 432),
                       c(-800, 300, 300, 300, 150)), max = 0.2)
