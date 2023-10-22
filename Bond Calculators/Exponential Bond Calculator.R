# Exponential Bond Calculator
exp_bond_calculator <- function(bond_principle,
                                bond_coupon_rate,
                                bond_interest_rate,
                                bond_year_to_mat,
                                n_an_b = 1){
  
  # Calculate Principal part
  exp_principal <- bond_principle * exp(-bond_interest_rate * bond_year_to_mat)
  
  # Define new variable for number of coupon repayments
  repay <- bond_year_to_mat * n_an_b
  
  # Set up variable name for sum of coupons
  exp_coupon <- sum(((bond_coupon_rate * bond_principle) / n_an_b) *
                      exp(-bond_interest_rate * (seq(repay) / repay)))
  
  # Calculate Bond Value and display it
  return(exp_principal + exp_coupon)
}
# Test
exp_bond_calculator(100, 0.06, 0.0676, 2, 2)
