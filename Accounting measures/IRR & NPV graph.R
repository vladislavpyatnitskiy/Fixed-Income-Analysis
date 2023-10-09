# Project Cash Flow
cash_flows2 <- c(-800, 300, 300, 300, 150)

# Plot relationship between IRR and NPV
npv_irr_graph <- function(x, xlim = NULL, h = 0, col, lwd = 3){
  
  # Make a vector with irr's and make it matrix
  irr_matrix <- as.matrix(seq(0, 1, by  = 0.01))
  
  # Create 
  values_for_npv_graph <- NULL
  
  # For each irr values
  for (v in 1:nrow(irr_matrix)){
    
    # Put value in vector
    values_for_npv_graph <- rbind(values_for_npv_graph,
                                  sum(x / (1 + irr_matrix[v])
                                      ^ (seq(along = x) - 1))) }
  
  # Join NPV and IRR into one data frame
  final_values_for_npv_graph <- data.frame(irr_matrix, values_for_npv_graph)
  
  # Create column names 
  colnames(final_values_for_npv_graph) <- c("IRR", "NPV")
  
  # Plot 
  plot(x = final_values_for_npv_graph, type = "l", xlim = xlim)
  
  # Make break even line more visible
  abline(h = h, col = col, lwd = lwd)
}
# Test
npv_irr_graph(cash_flows2, xlim = c(0, 0.2), h = 0, col = "red", lwd = 3)
