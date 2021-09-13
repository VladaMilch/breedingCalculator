require(testthat)
library(ggplot2)
library(ggpubr)


test_that("plotting just works ",{
    
    objMG <- multiGenotype(
            confidence_p = 0.90, 
            birth_days = 3, 
            genotypes_p = c(0,1), 
            genotypes_N = c(0,200), 
            sex_distribution = "balanced", 
            strain = "Festing")
    
    pp <- probabilitiesPlot(objMG)
    expect_is(pp, "ggplot")
    expect_is(pp, "gg")
})
    
