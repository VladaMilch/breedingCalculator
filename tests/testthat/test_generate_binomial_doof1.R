require(testthat)

test_that("Fails gracefully",{
  # all parameters checked before
})


test_that("Correct calculation",{
  expect_equal(
      distr::d(generate_binomial_doof1(
          litter_mean = 5, 
          binomial_p = 0.25,
          effective_fertility_p = 1-dbinom(x = 0, size = 20, prob = 1/4)
          )
        )(10),
      dbinom(x = 10, size = 20, prob = 1/4))
  
    expect_equal(
        distr::d(generate_binomial_doof1(
            effective_fertility_p = 0.7, 
            binomial_p = 0.9,
            litter_mean = 10))(0),
      0.3)
    
    expect_equal(
        distr::d(generate_binomial_doof1(
            effective_fertility_p = 0.7*(1-dbinom(x = 0, 
                                                  size = 16, 
                                                  prob = 0.5)), 
            binomial_p = 0.5,
            litter_mean = 8))(7),
      dbinom(x = 7, size = 16, prob = 0.5)*0.7)
})
