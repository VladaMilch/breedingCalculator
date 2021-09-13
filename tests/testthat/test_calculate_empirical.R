require(testthat)

test_that(desc = "Empirical calculation is correct",{
    # empirical and poisson return the same when 
    # the observations come from poisson (large sample)
    expect_equal(
        calculate_needed_breedings_empirical(
            confidence_p = 0.9, 
            effective_fertility_p = 1, 
            n_needed = 8, 
            offsprings_n_sample = rpois(n=1000, lambda = 7)),
        calculate_needed_breedings_poisson(
            confidence_p = 0.9, 
            effective_fertility_p = 1, 
            n_needed = 8, 
            litter_mean = 7)
    )
    # degenerate distribution
    expect_equal(
        calculate_needed_breedings_empirical(
            confidence_p = 0.5, 
            effective_fertility_p = 1, 
            n_needed = 11, 
            offsprings_n_sample = rep(2,100)),
        6
    )
    expect_equal(
        calculate_needed_breedings_empirical(
            confidence_p = 0.999, 
            effective_fertility_p = 1, 
            n_needed = 11, 
            offsprings_n_sample = rep(2,100)),
        6
    )
    # uniform with small support
    set.seed(1)
    expect_equal(
        calculate_needed_breedings_empirical(
            confidence_p = 0.99, 
            effective_fertility_p = 1, 
            n_needed = 8, 
            offsprings_n_sample = sample(
                c(2,3,4,5), 
                size = 100, 
                prob = c(0.25, 0.25, 0.25, 0.25), replace = T)),
        4
    )
    expect_equal(
        calculate_needed_breedings_empirical(
            confidence_p = 0.9, 
            effective_fertility_p = 1, 
            n_needed = 8, 
            offsprings_n_sample = sample(
                c(2,3,4,5), 
                size = 100, 
                prob = c(0.25, 0.25, 0.25, 0.25), replace = T)),
        3
      )
  })

  calculate_needed_breedings_empirical(
    confidence_p = 0.95, 
    effective_fertility_p = 1, 
    n_needed = 8, 
    offsprings_n_sample = sample(c(2,3,4,5), 
                                 size = 100, 
                                 prob = c(0.25, 0.25, 0.25, 0.25), replace = T))

  
  doof1 <- distr::DiscreteDistribution(supp = c(2,3,4,5), 
                                       prob = c(0.25, 0.25, 0.25, 0.25))
  doofN_quantile <- distr::convpow(doof1, N=3)@q(1-0.95)
  doofN_quantile
  
  doof1 <- distr::DiscreteDistribution(supp = c(2), 
                                       prob = c(1))
  doofN_quantile <- distr::convpow(doof1, N=3)@q(1-0.8)
  doofN_quantile
  
  calculate_needed_breedings_empirical(
    confidence_p = 0.5, 
    effective_fertility_p = 1, 
    n_needed = 11, 
    offsprings_n_sample = rep(2,100))
  