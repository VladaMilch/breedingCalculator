require(testthat)

require(testthat)

############# 1 ###############
####  Falling glacefully  #####

test_that(desc = "Falling glacefully: Confidence",{
    expect_error(
        calculate_needed_breedings(
            confidence_p = 1, 
            fertility_p = 1, 
            n_needed = 10, 
            litter_mean = 7, 
            method = "festing"))
  
    expect_error(
        calculate_needed_breedings(
            confidence_p = 0, 
            fertility_p = 1, 
            n_needed = 10, 
            litter_mean = 7, 
            method = "festing"))
    
})

test_that(desc = "Falling glacefully: Effective fertility",{
  expect_error(
    calculate_needed_breedings(
      confidence_p = 0.5, 
      fertility_p = 0, 
      n_needed = 10, 
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    calculate_needed_breedings(
      confidence_p = 0.5, 
      fertility_p = 1.1, 
      n_needed = 10, 
      litter_mean = 7, 
      method = "festing"))
  
})

test_that(desc = "Falling glacefully: Offstrings, litter",{
    expect_error(
        calculate_needed_breedings(
            confidence_p = 0.5, 
            fertility_p = 0.5, 
            n_needed = 0, 
            litter_mean = 7, 
            method = "festing"))
  
    expect_error(
        calculate_needed_breedings(
            confidence_p = 0.5, 
            fertility_p = 0.5, 
            n_needed = 10, 
            litter_mean = 0, 
            method = "festing"))
  
})

test_that(desc = "Falling glacefully: Methods general",{
  expect_error(
    calculate_needed_breedings(
      confidence_p = 0.5, 
      fertility_p = 0.5, 
      n_needed = 10, 
      litter_mean = 7, 
      method = "tet"))
})


##################### 2 ####################
###  Calculation for each method runs  ####

test_that(desc = "Calculation runs: Festing method",{
  
    confidence_p_random <-  runif(n = 1, min = 0.1, max = 1)
    fertility_p_random <- runif(n = 1, min = 0.1, max = 1)
    #n_needed_rand <- sample(size = 1, x = seq(1, 70, 1))
    n_needed_rand <- sample(size = 1, x = seq(2, 70, 1))
    litter_mean_rand <- sample(size=1, x=seq(2,30,1))
    
    expect_length(
        calculate_needed_breedings(
            confidence_p = confidence_p_random, 
            fertility_p = fertility_p_random, 
            n_needed = n_needed_rand, 
            litter_mean = litter_mean_rand, 
            method = "festing"), 
    1)
})


# Binomial model will not be used 
# test_that(desc = "Calculation runs: Binomial method",{
#   
#   confidence_p_random <-  runif(n = 1, min = 0.1, max = 1)
#   fertility_p_random <- runif(n = 1, min = 0.1, max = 1)
#   #n_needed_rand <- sample(size = 1, x = seq(1, 70, 1))
#   n_needed_rand <- sample(size = 1, x = seq(2, 70, 1))
#   litter_mean_rand <- sample(size=1, x=seq(2,30,1))
#   
#     expect_length(
#         calculate_needed_breedings(
#             confidence_p = confidence_p_random,
#             fertility_p = fertility_p_random,
#             n_needed = n_needed_rand,
#             litter_mean = litter_mean_rand,
#             method = "binomial"),
#       1)
# })

test_that(desc = "Calculation runs: Poisson method",{
  
  confidence_p_random <-  runif(n = 1, min = 0.1, max = 1)
  fertility_p_random <- runif(n = 1, min = 0.1, max = 1)
  #n_needed_rand <- sample(size = 1, x = seq(1, 70, 1))
  n_needed_rand <- sample(size = 1, x = seq(2, 70, 1))
  litter_mean_rand <- sample(size=1, x=seq(2,30,1))
  
    expect_length(
        calculate_needed_breedings(
            confidence_p = confidence_p_random, 
            fertility_p = fertility_p_random, 
            n_needed = n_needed_rand, 
            litter_mean = litter_mean_rand, 
            method = "poisson"), 
        1)
})

test_that(desc = "Calculation runs: Empirical method",{
  
  confidence_p_random <-  runif(n = 1, min = 0.1, max = 1)
  fertility_p_random <- runif(n = 1, min = 0.1, max = 1)
  #n_needed_rand <- sample(size = 1, x = seq(1, 70, 1))
  n_needed_rand <- sample(size = 1, x = seq(2, 70, 1))
  litter_mean_rand <- sample(size=1, x=seq(2,30,1))
  
    expect_length(
        calculate_needed_breedings(
            confidence_p = confidence_p_random, 
            fertility_p = fertility_p_random, 
            n_needed = n_needed_rand, 
            offsprings_n_sample = c(rep(1,10), rep(3,20)),
            method = "empirical"), 
        1)
    
})
