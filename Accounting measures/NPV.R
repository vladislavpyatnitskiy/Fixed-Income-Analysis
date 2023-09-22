# Cash Flow
cash_flow <- c(-1000, 250, 300, 360, 432)

# Rate of Return
rate_of_return <- 0.05

# NPV function
NPV <- function(C, r) {

  # Sum of cash flow adjusted by rate of return
  sum(C / (1 + r) ^ (seq(along = C) - 1))
}

# Test
NPV(cash_flow, rate_of_return)
