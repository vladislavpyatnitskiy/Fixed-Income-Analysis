# Function for IRR
IRR <- function(C, r) {
  uniroot(function(C, r) (sum(C/(1+r)^(seq(along = C)-1))), c(0,1), C=C)$root }

# Test
IRR(C = c(-800, 300, 300, 300, 150), r = 0.1)
