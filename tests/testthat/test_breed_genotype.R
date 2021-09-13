require(testthat)


############# 1 ###############
####  Falling glacefully  #####

test_that(desc = "Falling glacefully: Confidence",{
  expect_error(
    breed_genotype(
      confidence_p = 1, 
      effective_fertility_p = 1, 
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    breed_genotype(
      confidence_p = 0, 
      effective_fertility_p = 1, 
      litter_mean = 7, 
      method = "festing"))
  
})

test_that(desc = "Falling glacefully: Effective fertility",{
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0, 
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 1.1, 
      litter_mean = 7, 
      method = "festing"))
  
})

test_that(desc = "Falling glacefully: Methods general",{
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0.5, 
      litter_mean = 7, 
      method = "tet"))
})

# @TODO: chenage that fpr the genotype-specific errors
test_that(desc = "Falling glacefully: Offstrings, litter",{
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0.5, 
      genotypes_N = c(0), 
      genotypes_p = 1,
      litter_mean = 7, 
      method = "festing"))
  
  expect_error(
    breed_genotype(
      confidence_p = 0.5, 
      effective_fertility_p = 0.5, 
      litter_mean = 0, 
      method = "festing"))
  
})

############# 2 ######################################
####  Calculation agrees with non-genotype case  #####

test_that("Calculation with 1 genotype agrees with 
          the non-genotype-specific model: Poisson", {
    expect_equal(
        breed_genotype(
          confidence_p = 0.9, 
          effective_fertility_p = 0.6,
          genotypes_p = c(1,0), 
          genotypes_N = c(100,0),
          litter_mean = 7, 
          method = "poisson")$required_breedings,
        calculate_needed_breedings(
            confidence_p = 0.9, 
            effective_fertility_p = 0.6, 
            n_needed = 100, 
            litter_mean = 7, 
            method = "poisson")
        )
})




# test_that("Calculation with >1 genotype agrees with 
#           empirical sampling: Poisson model", {
#             
#     n_mice = sample(x = c(6:15), size = 1)
#     empitmp <- t(sapply(seq(1:10^5), FUN = function(x){
#     rowSums(sapply(rpois(n = n_mice, # n mice
#                          lambda = 8), 
#                    FUN = function(xx)rmultinom(n = 1, 
#                                                size = xx, 
#                                                prob = c(0.4, 0.6))))
#     }))
#     
#     empi_confidence <- sum(empitmp[,1]>30 & empitmp[,2]>30)/10^5
#     expect_equal(breed_genotype(
#       confidence_p = empi_confidence-0.1^3, 
#       effective_fertility_p = 1,
#       genotypes_p = c(0.4,0.6), 
#       genotypes_N = c(31,31),
#       litter_mean = 8, 
#       method = "poisson"), n_mice)
#     
#     expect_equal(breed_genotype(
#       confidence_p = empi_confidence + 0.1^3, 
#       effective_fertility_p = 1,
#       genotypes_p = c(0.4,0.6), 
#       genotypes_N = c(31,31),
#       litter_mean = 8, 
#       method = "poisson"), n_mice+1)
# 
# })
