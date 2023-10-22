# Simple Bond Calculator
simple_bond_calculator <- function(bond_principle,
                                   bond_coupon_rate,
                                   bond_interest_rate,
                                   bond_year_to_mat,
                                   n_an_b = 1){
  # Calculate coupon part
  coupon_part <- (bond_coupon_rate * bond_principle) / n_an_b
  
  # Variable is ratio between repayment number and bond rate
  rep_x_rate <- n_an_b / bond_interest_rate
  
  # Variable is ratio between maturity and repayment number
  mat_x_rep <- bond_year_to_mat * n_an_b
  
  # Calculate rates part
  bond_rate_part <- rep_x_rate - n_an_b / (bond_interest_rate *
                                             (1 + (1 / rep_x_rate))^mat_x_rep)
  # Calculate principle part
  principle_part <- bond_principle / ((1 + (1 / rep_x_rate)) ^ mat_x_rep)
  
  # Connect all parts and display value
  return(coupon_part * bond_rate_part + principle_part)
}
# Test it
simple_bond_calculator(1000, 0.08, 0.06, 30, 2)
