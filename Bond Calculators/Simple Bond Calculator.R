# Simple Bond Calculator
simple_bond_calculator <- function(bond_principle,
                            bond_coupon_rate,
                            bond_interest_rate,
                            bond_year_to_mat,
                            n_an_b = 1
                            ){
  # Calculate coupon part
  coupon_part <- bond_coupon_rate / n_an_b

  # Calculate rates part
  bond_rate_part <- ((n_an_b/bond_interest_rate) -
                       n_an_b/(bond_interest_rate *
                            (1 + bond_interest_rate/n_an_b) ^
                              bond_year_to_mat * n_an_b))

  # Calculate principle part
  principle_part <- bond_principle / ((1 + bond_interest_rate/n_an_b) ^
                                         bond_year_to_mat * n_an_b)
  # Connect all parts
  price_of_the_bond = coupon_part * bond_rate_part + principle_part

  # Display value
  return(price_of_the_bond)
}
# Test it
simple_bond_calculator(1000, 80, 0.06, 30, 2)
