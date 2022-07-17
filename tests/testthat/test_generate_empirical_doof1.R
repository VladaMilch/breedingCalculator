require(testthat)

test_that("Fails gracefully",{
    # empirical sample is not too small
    expect_warning(
        generate_empirical_doof1(
            fertility_p = 1, 
            offsprings_n_sample = seq(1,10,1) 
        )
    )
    # whole numbers in the empirical sample
    expect_error(
        generate_empirical_doof1(
            fertility_p = 1, 
            offsprings_n_sample = seq(1,10,0.5) 
      )
    )
    # non-negative values in the empirical sample
    expect_error(
      generate_empirical_doof1(
        fertility_p = 1, 
        offsprings_n_sample = seq(-1,10,1) 
      )
    )
})


test_that("Correct calculation",{
    expect_equal(
        distr::d(generate_empirical_doof1(
            fertility_p = 1, 
            offsprings_n_sample = seq(1,5,1)))(1),
        0.2)
    expect_equal(
        distr::d(generate_empirical_doof1(
            fertility_p = 1, 
            offsprings_n_sample = seq(1,5,1)))(3),
        0.2)
    expect_equal(
        distr::d(generate_empirical_doof1(
            fertility_p = 5/6, 
            offsprings_n_sample = seq(1,5,1)))(4),
    1/6)
})
