require(testthat)

test_that("poisson model calculation correct", {
    expect_equal(
        calculate_needed_breedings_poisson(confidence_p = 0.9, 
                                   effective_fertility_p = 1, 
                                   n_needed = 35, litter_mean = 7),
        7)
  
    expect_equal(
        calculate_needed_breedings_poisson(confidence_p = 0.9, 
                                       effective_fertility_p = 1, 
                                       n_needed = 25, litter_mean = 7),
        5)
    set.seed(1)
    litmean=4
    k <- calculate_needed_breedings_poisson(confidence_p = 0.95, 
                                       effective_fertility_p = 0.7, 
                                       n_needed = 8, litter_mean = litmean)
    
    xx1 <- rpois(n = k*10^3, lambda = litmean)
    zeroes <- sample(x = c(1,0), replace = T, prob=c(0.7,0.3), size = k*10^3) 
    offs1 <- ifelse(zeroes==0,0,xx1)
    offs <- matrix(offs1, nrow=10^3, ncol=k)
    expect_true(
        table(rowSums(offs)>=8)["TRUE"] >= 0.95*10^3
    )
    expect_true(
        table(rowSums(offs)-offs[,1]>=8)["TRUE"] <= 0.95*10^3
    )
})

