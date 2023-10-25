# NPV function
NPV <- function(C, r) { sum(C / (1 + r) ^ (seq(along = C) - 1)) }

# Test
NPV(C = c(-1000, 250, 300, 360, 432), r = 0.05)
