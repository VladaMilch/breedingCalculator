require(testthat)
require(distr)
#require(MASS)

test_that("Fails gracefully",{
    # all parameters checked before
})


test_that("Correct calculation",{
  expect_equal(
      distr::d(generate_poisson_doof1(
          litter_mean = 5, 
          fertility_p = 1-dpois(0,5))
      )(10),
      dpois(lambda = 5,x = 10))
              
  expect_equal(
      distr::d(generate_poisson_doof1(
          fertility_p = 0.7, 
          litter_mean = 10))(0),
      0.3)
  
  expect_equal(
      distr::d(generate_poisson_doof1(
          fertility_p = 0.7*(1-dpois(x=0,lambda = 10)), 
          litter_mean = 10))(7),
      dpois(x = 7,lambda = 10)*0.7)
})
