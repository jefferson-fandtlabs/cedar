test_that("apply_discount calculates correct present value with discounting", {
  # Test case: $5000/year for 3 years at 3% discount rate, starting at offset=1
  # (equivalent to old behavior where first year was always discounted)
  result <- apply_discount(5000, 3, 0.03, 1)

  # Manual calculation:
  # Year 1: 5000 / 1.03^1 = 4854.369
  # Year 2: 5000 / 1.03^2 = 4713.950
  # Year 3: 5000 / 1.03^3 = 4575.195
  # Total: 14143.514
  expected <- 5000 / 1.03 + 5000 / 1.03^2 + 5000 / 1.03^3

  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount handles zero discount rate correctly", {
  # With 0% discount, should just be cost * time_periods (offset doesn't matter)
  result <- apply_discount(5000, 3, 0, 0)

  expect_equal(result, 15000)
})


test_that("apply_discount handles single time period", {
  # Single period with offset=1 should be cost / (1 + discount_rate)^1
  result <- apply_discount(5000, 1, 0.03, 1)

  expected <- 5000 / 1.03
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount handles zero time periods", {
  # Zero time periods should return 0 (offset doesn't matter)
  result <- apply_discount(5000, 0, 0.03, 0)

  expect_equal(result, 0)
})


test_that("apply_discount handles zero cost", {
  # Zero cost should return 0 regardless of time periods or offset
  result <- apply_discount(0, 5, 0.03, 0)

  expect_equal(result, 0)
})


test_that("apply_discount validates input types", {
  # Non-numeric cost
  expect_error(
    apply_discount("5000", 3, 0.03, 0),
    "cost must be a single numeric value"
  )

  # Non-numeric time_periods
  expect_error(
    apply_discount(5000, "3", 0.03, 0),
    "time_periods must be a single numeric value"
  )

  # Non-numeric discount_rate
  expect_error(
    apply_discount(5000, 3, "0.03", 0),
    "discount_rate must be a single numeric value"
  )

  # Non-numeric time_offset
  expect_error(
    apply_discount(5000, 3, 0.03, "0"),
    "time_offset must be a single numeric value"
  )
})


test_that("apply_discount validates input lengths", {
  # Multiple cost values
  expect_error(
    apply_discount(c(5000, 6000), 3, 0.03, 0),
    "cost must be a single numeric value"
  )

  # Multiple time_periods
  expect_error(
    apply_discount(5000, c(3, 4), 0.03, 0),
    "time_periods must be a single numeric value"
  )

  # Multiple discount_rates
  expect_error(
    apply_discount(5000, 3, c(0.03, 0.05), 0),
    "discount_rate must be a single numeric value"
  )

  # Multiple time_offsets
  expect_error(
    apply_discount(5000, 3, 0.03, c(0, 1)),
    "time_offset must be a single numeric value"
  )
})


test_that("apply_discount validates non-negative values", {
  # Negative time_periods
  expect_error(
    apply_discount(5000, -3, 0.03, 0),
    "time_periods must be non-negative"
  )

  # Negative discount_rate
  expect_error(
    apply_discount(5000, 3, -0.03, 0),
    "discount_rate must be non-negative"
  )

  # Negative time_offset
  expect_error(
    apply_discount(5000, 3, 0.03, -1),
    "time_offset must be non-negative"
  )
})


test_that("apply_discount handles realistic CEA scenarios", {
  # Scenario 1: Yearly medical cost over 4 years at 3% discount, starting at offset=1
  c_success_year <- 1000
  time_periods <- 4
  discount_rate <- 0.03

  result <- apply_discount(c_success_year, time_periods, discount_rate, 1)

  # Manual calculation
  expected <- sum(c_success_year / (1 + discount_rate)^(1:time_periods))

  expect_equal(result, expected, tolerance = 0.001)

  # Scenario 2: Higher cost over 1 year
  c_fail_year <- 5000
  result2 <- apply_discount(c_fail_year, 1, discount_rate, 1)

  expected2 <- c_fail_year / (1 + discount_rate)

  expect_equal(result2, expected2, tolerance = 0.001)
})


test_that("apply_discount matches example from Part2_Model_McMillanWilhoit_CP.R", {
  # From the example file, reconstruct one of the discounted costs
  c_success_year <- 1000
  discount_rate <- 0.03

  # NOW WE CAN PROPERLY HANDLE years 2-5!
  # c_success_years_two_five should be years 2-5 (4 years starting at year 2)
  # Using the NEW formula with offset=2:
  result_years_2_5 <- apply_discount(c_success_year, 4, discount_rate, 2)

  # Manual calculation for years 2-5
  expected <- sum(c_success_year / (1 + discount_rate)^(2:5))

  expect_equal(result_years_2_5, expected, tolerance = 0.001)

  # Also test years 1-4 with offset=1 (old behavior)
  result_years_1_4 <- apply_discount(c_success_year, 4, discount_rate, 1)
  expected_1_4 <- sum(c_success_year / (1 + discount_rate)^(1:4))
  expect_equal(result_years_1_4, expected_1_4, tolerance = 0.001)
})


test_that("apply_discount handles high discount rates", {
  # Test with 10% discount rate, offset=1
  result <- apply_discount(10000, 5, 0.10, 1)

  expected <- sum(10000 / (1.10)^(1:5))

  expect_equal(result, expected, tolerance = 0.01)
})


test_that("apply_discount handles many time periods", {
  # Test with 20 years, offset=1
  result <- apply_discount(1000, 20, 0.03, 1)

  expected <- sum(1000 / (1.03)^(1:20))

  expect_equal(result, expected, tolerance = 0.01)
})


test_that("apply_discount handles decimal time periods", {
  # Test with fractional time periods (e.g., 2.5 years), offset=1
  result <- apply_discount(5000, 2.5, 0.03, 1)

  # seq_len will floor to 2
  expected <- sum(5000 / (1.03)^(1:2))

  expect_equal(result, expected, tolerance = 0.001)
})


# NEW TESTS FOR TIME_OFFSET FUNCTIONALITY

test_that("apply_discount with offset=0 gives undiscounted first year", {
  # First year should be undiscounted with offset=0
  result <- apply_discount(1000, 1, 0.03, 0)

  # With offset=0, discount factor is (1.03)^0 = 1.0
  expected <- 1000 / (1.03)^0
  expect_equal(expected, 1000)  # Verify it's truly undiscounted
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount correctly handles sequential time offsets", {
  # Simulate the user's example: 4 edges with 1 year each
  # Edge 4->7: offset=0, duration=1 (year 0-1, undiscounted)
  # Edge 7->11: offset=1, duration=1 (year 1-2, discounted once)
  # Edge 11->13: offset=2, duration=1 (year 2-3, discounted twice)
  # Edge 13->21: offset=3, duration=1 (year 3-4, discounted three times)

  cost <- 1000
  discount_rate <- 0.03

  edge1 <- apply_discount(cost, 1, discount_rate, 0)
  edge2 <- apply_discount(cost, 1, discount_rate, 1)
  edge3 <- apply_discount(cost, 1, discount_rate, 2)
  edge4 <- apply_discount(cost, 1, discount_rate, 3)

  # Manual calculations
  expect_equal(edge1, 1000 / (1.03)^0, tolerance = 0.001)  # 1000.000
  expect_equal(edge2, 1000 / (1.03)^1, tolerance = 0.001)  # 970.874
  expect_equal(edge3, 1000 / (1.03)^2, tolerance = 0.001)  # 942.596
  expect_equal(edge4, 1000 / (1.03)^3, tolerance = 0.001)  # 915.142

  # Total across all edges
  total <- edge1 + edge2 + edge3 + edge4
  expected_total <- sum(1000 / (1.03)^(0:3))
  expect_equal(total, expected_total, tolerance = 0.001)
})


test_that("apply_discount with offset=0 and multiple periods", {
  # 3 years starting at t=0: years 0-1, 1-2, 2-3
  result <- apply_discount(1000, 3, 0.03, 0)

  expected <- sum(1000 / (1.03)^(0:2))
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount with offset=2 and multiple periods", {
  # 3 years starting at t=2: years 2-3, 3-4, 4-5
  result <- apply_discount(1000, 3, 0.03, 2)

  expected <- sum(1000 / (1.03)^(2:4))
  expect_equal(result, expected, tolerance = 0.001)
})


test_that("apply_discount: offset matters for non-zero discount rates", {
  # Same cost and periods, different offsets should give different results
  result_offset0 <- apply_discount(1000, 2, 0.03, 0)
  result_offset1 <- apply_discount(1000, 2, 0.03, 1)
  result_offset2 <- apply_discount(1000, 2, 0.03, 2)

  # Higher offset means more discounting, so lower present value
  expect_true(result_offset0 > result_offset1)
  expect_true(result_offset1 > result_offset2)
})


test_that("apply_discount: offset doesn't matter for zero discount rate", {
  # With 0% discount, offset shouldn't matter
  result_offset0 <- apply_discount(1000, 3, 0, 0)
  result_offset1 <- apply_discount(1000, 3, 0, 1)
  result_offset10 <- apply_discount(1000, 3, 0, 10)

  # All should equal cost * periods
  expect_equal(result_offset0, 3000)
  expect_equal(result_offset1, 3000)
  expect_equal(result_offset10, 3000)
})