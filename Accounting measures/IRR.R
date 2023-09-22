# Define Cash Flow
irr_cash_flows1 <- c(-800, 300, 300, 300, 150)

# Function for IRR
IRR <- function(C, r) {
  uniroot(function(C, r) (sum(C / (1 + r) ^ (seq(along = C) - 1))),
          c(0, 1), C = C)$root
}

# Test
IRR(C = irr_cash_flows1, r  = 0.1)
