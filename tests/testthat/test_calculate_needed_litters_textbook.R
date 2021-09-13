require(testthat)

test_that("Border effect problem resolved", {
    #barplot(pnorm(q = c(-10:15), mean = 3, sd = 2.5))
    expect_equal(
        calculate_needed_litters_textbook(confidence_p = 0.9, 
                                      litter_mean = 30, n_needed = 2),
        1)
})

test_that("Calculation is correct", {
    needed_n_random <- sample(x = seq(1, 100, 1), size = 1)
    nbre <- calculate_needed_litters_textbook(confidence_p = 0.9, 
                                        litter_mean = 3, n_needed = needed_n_random)
      
    expect_gt(
        qnorm(p = 0.1, mean = 3*nbre, sd = sqrt(nbre)*2.5), 
        needed_n_random)
    
})
