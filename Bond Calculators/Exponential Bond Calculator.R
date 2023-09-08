# Exponential Bond Calculator
exp_bond_calculator <- function(bond_principle,
                                bond_coupon_rate,
                                bond_interest_rate,
                                bond_year_to_mat,
                                n_an_b = 1){
  
  # Calculate Principal part
  exp_principal <- bond_principle * exp(-bond_interest_rate * bond_year_to_mat)
  
  # Set up variable name for sum of coupons
  exp_coupon <- 0
  
  # Sum Cash Flows
  for (n in 1:(bond_year_to_mat * n_an_b)){
    # Calculate Sum of coupons
    exp_coupon <- exp_coupon +
      ((bond_coupon_rate * bond_principle) / n_an_b) *
      exp(-bond_interest_rate *
            (n / (bond_year_to_mat * n_an_b)))
  }
  # Calculate Bond Value
  exp_bond_value <- exp_principal + exp_coupon
  
  # Display Value
  return(exp_bond_value)
}
# Test
exp_bond_calculator(100, 0.06, 0.0676, 2, 2)
