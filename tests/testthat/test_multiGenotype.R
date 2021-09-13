require(testthat)

test_that("Failing gracefully: generally wrong input ",{
  expect_error(
    multiGenotype(
      confidence_p = 0, # error
      birth_days = 3, 
      genotypes_p = c(0.6,0.4), 
      genotypes_N = c(5,5), 
      sex_distribution = "unimportant", 
      strain = "Festing"
      )
    )
  
  expect_error(
    multiGenotype(
      confidence_p = 0.9, 
      birth_days = 3, 
      genotypes_p = c(0.5,0.4), # error
      genotypes_N = c(5,5), 
      sex_distribution = "unimportant", 
      litter_average = 6
    )
  )
  
  expect_error(
    multiGenotype(
      confidence_p = 0.9, 
      birth_days = 3, 
      genotypes_p = c(0.6,0.4), 
      genotypes_N = c(0.5, 1), # error
      sex_distribution = "unimportant", 
      litter_average = 6
    )
  )
  
  expect_error(
    multiGenotype(
      confidence_p = 0.9, 
      birth_days = 3, 
      genotypes_p = c(0.6,0.4), 
      genotypes_N = c(110, 1), 
      sex_distribution = "-", # error
      litter_average = 6
    )
  )
  
  expect_error(
    multiGenotype(
      confidence_p = 0.9, 
      birth_days = 3, 
      genotypes_p = c(0.6,0.4), 
      genotypes_N = c(0, 10), 
      sex_distribution = "unimportant", 
      strain = "manual", 
      litter_average = 0
    )
  )

})

test_that("Failing gracefully: balanced gender scenario requires even numbers",{
  expect_error(
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0.6,0.4), 
      genotypes_N = c(5,10), 
      sex_distribution = "balanced", 
      litter_average = 6
    )
  )
})

test_that("Correct calculation: balanced",{
  
  expect_equal(
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0,1), 
      genotypes_N = c(0,100), 
      sex_distribution = "balanced", 
      litter_average = 6
    )$required_breedings,
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0.5,0.5), 
      genotypes_N = c(50,50), 
      sex_distribution = "unimportant", 
      litter_average = 6
    )$required_breedings
  )
  
  expect_equal(
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0.1, 0.2, 0.3, 0.4), 
      genotypes_N = c(10,16,14,12), 
      sex_distribution = "balanced", 
      litter_average = 6
    )$required_breedings,
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0.05,0.05,0.1,0.1,0.15,0.15,0.2,0.2),
      genotypes_N = c(5,5,8,8,7,7,6,6), 
      sex_distribution = "unimportant", 
      litter_average = 6
    )$required_breedings
  )
  
})

test_that("Correct calculation: all one sex",{
  
  expect_equal(
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0,1), 
      genotypes_N = c(0,20), 
      sex_distribution = "all one sex", 
      litter_average = 6
    )$required_breedings,
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0, 0, 0.5, 0.5), 
      genotypes_N = c(0, 0, 0, 20), 
      sex_distribution = "unimportant", 
      litter_average = 6
    )$required_breedings
  )
  
  expect_equal(
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0.4, 0.6), 
      genotypes_N = c(10, 16), 
      sex_distribution = "all one sex", 
      litter_average = 6
    )$required_breedings,
    multiGenotype(
      confidence_p = 0.8, 
      birth_days = 3, 
      genotypes_p = c(0.2, 0.2, 0.3, 0.3),
      genotypes_N = c(0, 10, 0, 16), 
      sex_distribution = "unimportant", 
      litter_average = 6
    )$required_breedings
  )
  
})

test_that("Correct class object returned",{
  
  # only the required breedings printed
  expect_identical(
    capture_output_lines(
      multiGenotype(
        confidence_p = 0.8, 
        birth_days = 3, 
        genotypes_p = c(0,1), 
        genotypes_N = c(0,20), 
        sex_distribution = "all one sex", 
        litter_average = 6
        ), 
      print=TRUE), 
    "[1] 14")
  
  mm <- multiGenotype(
    confidence_p = 0.8, 
    birth_days = 3, 
    genotypes_p = c(0,1), 
    genotypes_N = c(0,20), 
    sex_distribution = "all one sex", 
    litter_average = 6)
  
  expect_equal(mm$confidence_p  , 0.8)
  expect_equal(mm$required_breedings, 14)
  expect_equal(mm$if_balanced_sex, FALSE)
  expect_equal(mm$if_onesex, TRUE)
})

test_that("All strains work correctly",{
  strains <- strain_data$strain_name
  for (current_strain in strains){
    mm <- multiGenotype(
      confidence_p = 0.8, 
      birth_days = 4, 
      genotypes_p = c(0,1), 
      genotypes_N = c(0,20), 
      strain = current_strain,
      sex_distribution = "all one sex", 
      litter_average = 6)
  }

})


