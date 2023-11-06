# MIRR
MIRR <- function(C, r = 0.1, I = 1000){ # Calculation & Recommendation
  
  mirr <- round((sum(C*((1+r)^(length(C)-seq(C))))/I)^(1/length(C))-1, 4) * 100
  
  if (mirr > r){ sprintf("MIRR is %s%% > cost of capital. Start project", mirr)
  } else { sprintf("MIRR is %s%% < cost of capital. Don't start", mirr) }
}
MIRR(C = c(300, 300, 300, 300), r = 0.12, I = 800) # Test
