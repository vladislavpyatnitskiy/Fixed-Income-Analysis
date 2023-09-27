# Bond Cash Flow with initial buy
bond_payments <- c(-98.39, 3, 3, 3, 103)

# Yield Calculation
bond_r_cltn <- function(C, r) {
  uniroot(function(C, r) sum(C / (1 + r) ^ ((seq(along = C) - 1))/2)
          , c(0, 1), C = C)$root * 2
}

# Test
bond_r_cltn(C = bond_payments, r = 0.05)
