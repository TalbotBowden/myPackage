library(testthat)
library(TBowden2025Package)  # Load package

test_that("myncurve returns expected structure", {
  result <- myncurve(5, 10, 3)

  expect_type(result, "list")  # Check if result is a list
  expect_named(result, c("mu", "sigma", "probability"))  # Check if names are correct

  expect_equal(result$mu, 10)  # Ensure mean is stored correctly
  expect_equal(result$sigma, 3)  # Ensure sigma is correct
})

test_that("myncurve probability is computed correctly", {
  result <- myncurve(5, 10, 3)

  # Check if probability matches expected output (use known expected values)
  expected_prob <- pnorm(5, mean = 10, sd = 3)
  expect_equal(result$probability, round(expected_prob, 4))
})
