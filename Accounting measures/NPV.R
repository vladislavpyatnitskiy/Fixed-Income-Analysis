# Cash Flow
cash_flow <- c(-1000, 250, 300, 360, 432)

# Rate of Return
rate_of_return <- 0.05

# NPV function
net_present_value <- function(x, y){
  
  # Profit in the beginning
  sum_profit <- 0

  
  for (n in 1:length(x)){

    # Put formulae
    profit <- x[n] / ((1 + y)^(n-1))

    # Sum all
    sum_profit <- sum_profit + profit
  }
  # Display Profit or Loss
  return(sum_profit)
}
net_present_value(cash_flow, rate_of_return)
