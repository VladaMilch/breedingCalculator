test_that("Two roots correct",{
    expect_equivalent(
        sort(solve_quadratic(a = 1, b=2, c=-3)),
        sort(c(-3,1)))
  
    expect_equivalent(
        sort(solve_quadratic(a = -1, b=-2, c=3)),
        unique(c(-3,1)))
})


test_that("One root correct",{
    expect_equivalent(
        solve_quadratic(a = 1, b=-2, c=1),
        1)
})

test_that("No roots correct",{
  expect_null(
    solve_quadratic(a = 1, b=2, c=3),
    1)
})
