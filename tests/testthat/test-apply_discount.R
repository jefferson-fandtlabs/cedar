test_that("apply_discount calculates correct present value with discounting", {
  # Test case: $5000/year for 3 years at 3% discount rate
  result <- apply_discount(5000, 3, 0.03)

  # Manual calculation:
  # Year 1: 5000 / 1.03^1 = 4854.369
  # Year 2: 5000 / 1.03^2 = 4713.950
  # Year 3: 5000 / 1.03^3 = 4575.195
  # Total: 14143.514
  expected <- 5000 / 1.03 + 5000 / 1.03^2 + 5000 / 1.03^3

  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount handles zero discount rate correctly", {
  # With 0% discount, should just be cost * time_periods
  result <- apply_discount(5000, 3, 0)

  expect_equal(result, 15000)
})


test_that("apply_discount handles single time period", {
  # Single period should be cost / (1 + discount_rate)^1
  result <- apply_discount(5000, 1, 0.03)

  expected <- 5000 / 1.03
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount handles zero time periods", {
  # Zero time periods should return 0
  result <- apply_discount(5000, 0, 0.03)

  expect_equal(result, 0)
})


test_that("apply_discount handles zero cost", {
  # Zero cost should return 0 regardless of time periods
  result <- apply_discount(0, 5, 0.03)

  expect_equal(result, 0)
})


test_that("apply_discount validates input types", {
  # Non-numeric cost
  expect_error(
    apply_discount("5000", 3, 0.03),
    "cost must be a single numeric value"
  )

  # Non-numeric time_periods
  expect_error(
    apply_discount(5000, "3", 0.03),
    "time_periods must be a single numeric value"
  )

  # Non-numeric discount_rate
  expect_error(
    apply_discount(5000, 3, "0.03"),
    "discount_rate must be a single numeric value"
  )
})


test_that("apply_discount validates input lengths", {
  # Multiple cost values
  expect_error(
    apply_discount(c(5000, 6000), 3, 0.03),
    "cost must be a single numeric value"
  )

  # Multiple time_periods
  expect_error(
    apply_discount(5000, c(3, 4), 0.03),
    "time_periods must be a single numeric value"
  )

  # Multiple discount_rates
  expect_error(
    apply_discount(5000, 3, c(0.03, 0.05)),
    "discount_rate must be a single numeric value"
  )
})


test_that("apply_discount validates non-negative values", {
  # Negative time_periods
  expect_error(
    apply_discount(5000, -3, 0.03),
    "time_periods must be non-negative"
  )

  # Negative discount_rate
  expect_error(
    apply_discount(5000, 3, -0.03),
    "discount_rate must be non-negative"
  )
})


test_that("apply_discount handles realistic CEA scenarios", {
  # Scenario 1: Yearly medical cost over 4 years at 3% discount
  c_success_year <- 1000
  time_periods <- 4
  discount_rate <- 0.03

  result <- apply_discount(c_success_year, time_periods, discount_rate)

  # Manual calculation
  expected <- sum(c_success_year / (1 + discount_rate)^(1:time_periods))

  expect_equal(result, expected, tolerance = 0.001)

  # Scenario 2: Higher cost over 1 year
  c_fail_year <- 5000
  result2 <- apply_discount(c_fail_year, 1, discount_rate)

  expected2 <- c_fail_year / (1 + discount_rate)

  expect_equal(result2, expected2, tolerance = 0.001)
})


test_that("apply_discount matches example from Part2_Model_McMillanWilhoit_CP.R", {
  # From the example file, reconstruct one of the discounted costs
  c_success_year <- 1000
  discount_rate <- 0.03

  # c_success_years_two_five should be years 2-5 (4 years starting at year 2)
  # Using the original formula: sum(c_success_year / (1 + discount_rate)^(2:5))
  # Our function calculates for years 1-n, so for years 2-5 we need:
  # Years 2-5 is 4 time periods, but discounted starting from year 2

  # This is different - our function assumes years 1-n
  # For years 1-4:
  result_years_1_4 <- apply_discount(c_success_year, 4, discount_rate)

  # Manual calculation for years 1-4
  expected <- sum(c_success_year / (1 + discount_rate)^(1:4))

  expect_equal(result_years_1_4, expected, tolerance = 0.001)
})


test_that("apply_discount handles high discount rates", {
  # Test with 10% discount rate
  result <- apply_discount(10000, 5, 0.10)

  expected <- sum(10000 / (1.10)^(1:5))

  expect_equal(result, expected, tolerance = 0.01)
})


test_that("apply_discount handles many time periods", {
  # Test with 20 years
  result <- apply_discount(1000, 20, 0.03)

  expected <- sum(1000 / (1.03)^(1:20))

  expect_equal(result, expected, tolerance = 0.01)
})


test_that("apply_discount handles decimal time periods", {
  # Test with fractional time periods (e.g., 2.5 years)
  result <- apply_discount(5000, 2.5, 0.03)

  # seq_len will floor to 2
  expected <- sum(5000 / (1.03)^(1:2))

  expect_equal(result, expected, tolerance = 0.001)
})