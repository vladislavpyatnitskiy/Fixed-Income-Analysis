# Cash Flow
cash_flows2 <- c(-800, 300, 300, 300, 150)

# Function to plot graph
npv_irr_graph <- function(x){
  
  # Make a vector with irr's and make it matrix
  irr_matrix <- as.matrix(seq(0, 1, by  = 0.01))
  
  # Create a variable to store NPV values
  values_for_npv_graph <- NULL
  
  # For each irr values
  for (v in 1:nrow(irr_matrix)){
    
    # Calculate NPV value
    graph_value <- sum(x / (1 + irr_matrix[v]) ^ (seq(along = x) - 1))
    
    # Put value in vector
    values_for_npv_graph <- c(values_for_npv_graph, graph_value)
  }
  # Make vector with NPV values a matrix
  values_for_npv_graph <- as.matrix(values_for_npv_graph)
  
  # Join NPV and IRR into one data frame
  final_values_for_npv_graph <- data.frame(irr_matrix,
                                           values_for_npv_graph)
  # Create column names 
  colnames(final_values_for_npv_graph) <- c("IRR", "NPV")
  
  # Plot 
  plot(x = final_values_for_npv_graph,
       type = "l",
       xlim = c(0.00, 0.2))
  
  # Make break even line more visible
  abline(h = 0,
         col = "red",
         lwd = 2)
}
# Test
npv_irr_graph(cash_flows2)
