require(testthat)

test_that("Binomial calculation correct: p=0.5",{
    n_needed <- 2
    confidence_p <- 0.9
    litmean=3
    #litmean <- sample(x=c(2:7), size=1)  # max offs single mouse
    N <- litmean*2
    single_mouse_p = 0.5
    fertility = 1
    success_p <- fertility-dbinom(0, 
                     size = 2*litmean, 
                     prob = single_mouse_p)
    
    success_p*(1-sum(dbinom(x = c(0,1), size = 6, prob = 0.5)))

    expect_equal(
        2,
        calculate_needed_breedings_binomial(
            0.88, 
            effective_fertility_p = fertility, 
            n_needed, 
            litter_mean = litmean)
    )
    
    expect_equal(
        1,
        calculate_needed_breedings_binomial(
            0.87, 
            effective_fertility_p = fertility, 
            n_needed, 
            litter_mean = litmean)
    )
    
})




test_that("Binomial calculation correct: p!=0.5",{
  
    n_needed <- 20
    confidence_p <- 0.9
    litmean=12
    single_mouse_p = 0.2
    fertility = 1
    success_p <- fertility-dbinom(0, 
                                  size = litmean/single_mouse_p, 
                                  prob = single_mouse_p)
  
    # for 2 mice
    pp2 <- success_p*(1-sum(dbinom(x = c(0:19), 
                                 size = litmean/single_mouse_p*2, 
                                 prob = single_mouse_p)))
  
    expect_equal(
      2,
      calculate_needed_breedings_binomial(
        confidence_p = 0.8,
        effective_fertility_p = fertility, 
        n_needed, 
        binomial_p = 0.2,
        litter_mean = litmean)
    )
    
    expect_equal(
      3,
      calculate_needed_breedings_binomial(
        confidence_p = 0.85,
        effective_fertility_p = fertility, 
        n_needed, 
        binomial_p = 0.2,
        litter_mean = litmean)
    )

  
})
  #   # m1offs <- distr::Binom(prob=0.5,size=2*litmean)
  #   # found <- 0
  #   # K <- 0
  #   # while(found==0){
  #   #   K <- K+1
  #   #     CPK <- distr::CompoundDistribution(
  #   #         NumbOfSummandsDistr = distr::Binom(prob=success_p,size=K), 
  #   #         SummandsDistr = distr::Binom(prob=0.5,size=2*litmean))
  #   #     found <- ifelse(distr::p(CPK)(n_needed-1) < 1-confidence_p, 1, 0)
  #   # }
  #   # expect_equal(K,
  #       calculate_needed_breedings_binomial(
  #           confidence_p, 
  #           effective_fertility_p = fertility, 
  #           n_needed, 
  #           litter_mean = litmean)
  #   
  #   
  #   CP1 <- distr::CompoundDistribution(
  #     NumbOfSummandsDistr = distr::Binom(prob=success_p,size=1), 
  #     SummandsDistr = distr::Binom(prob=0.5,size=2*litmean))
  # 
  #   p(CP1)(1)
  #   pbinom(q = 0, size=6,prob = 0.5 )*success_p + 1-success_p
  #   pbinom(q = 0, size=6,prob = 0.5, lower.tail = F)*success_p 
  #   1-p(CP1)(0)
  #   
  #   CP1 <- distr::CompoundDistribution(
  #     NumbOfSummandsDistr = distr::Binom(prob=1,size=1), 
  #     SummandsDistr = distr::Binom(prob=0.5,size=2*litmean))
  #   p(CP1)(1)
  #   pbinom(q = 1, size = 6, prob = 0.5)
  #   pbinom(q = 0, size = 6, prob = 0.5)
  #   
  #   CP2 <- distr::CompoundDistribution(
  #     NumbOfSummandsDistr = distr::Binom(prob=1,size=2), 
  #     SummandsDistr = distr::Binom(prob=0.5,size=2*litmean))
  #   p(CP2)(1)
  #   
  #   pbinom(q = 1, size = 12, prob = 0.5)
  #   pbinom(q = 0, size = 12, prob = 0.5)
  #   
  #   DD <- convpow(D1 =  distr::Binom(prob=0.5,size=2*litmean), N=2)
  #   p(DD)(1)
  #   
  #   CP3 <- CompoundDistribution(NumbOfSummandsDistr = distr::Binom(prob=1,size=1),
  #                               distr::Binom(prob=0.5,size=2*litmean))
  #   
  #   p(CP3)(1)
  #   p(distr::Binom(prob=0.5,size=2*litmean))(1)
  #   pbinom(q = 1, size = 6, prob = 0.5)
  #   
  #   
  # })
