test_that("Correct is.wholenumber",{
    expect_true(is.wholenumber(1))
    expect_true(is.wholenumber(-1))
    expect_false(is.wholenumber(-1.1))
})