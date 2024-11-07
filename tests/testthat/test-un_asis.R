test_that("un_asis does not alter objects without AsIs class", {
  x <- rnorm(10L)
  results <- un_asis(x)
  expect_identical(results, x)
})

test_that("un_asis removes AsIs class", {
  x <- rnorm(10L)
  x_asis <- I(x)
  results <- un_asis(x_asis)
  expect_identical(results, x)
})
