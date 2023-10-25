# Rate calculator
r.calculator <- function(C, r) {
  uniroot(function(C, r) sum(C/(1+r)^((seq(C)-1))/2), c(0, 1), C = C)$root*2 }

# Test
r.calculator(C = c(-98.39, 3, 3, 3, 103), r = 0.05)
