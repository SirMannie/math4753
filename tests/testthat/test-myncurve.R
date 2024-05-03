library(testthat)
library(package1) # Replace package1 with the actual name of your package

test_that("myncurve returns expected values", {
  result <- myncurve(mu = 0, sigma = 1)
  expect_type(result, "list")
  expect_true("mu" %in% names(result))
  expect_true("sigma" %in% names(result))
  # Additional expectations...
})

