require(testthat)

############# 1 ###############
####  Falling glacefully  #####

test_that(desc = "Used support numerically equale 1 in probability (and does not break).",{
    expect_equivalent(
        class(generate_poisson_doof1(fertility_p = 0.999, 
            litter_mean = 7)), 
        "DiscreteDistribution")
    
    expect_equivalent(
        class(generate_poisson_doof1(fertility_p = 0.001, 
                                     litter_mean = 7)), 
        "DiscreteDistribution")
    
})
