test_that("convert_duration_to_years converts hours correctly", {
  # 1 year = 8766 hours (365.25 * 24)
  expect_equal(convert_duration_to_years(8766, "hours"), 1)
  expect_equal(convert_duration_to_years(4383, "hours"), 0.5)
  expect_equal(convert_duration_to_years(24, "hours"), 24 / 8766)
})

test_that("convert_duration_to_years converts days correctly", {
  # 1 year = 365.25 days
  expect_equal(convert_duration_to_years(365.25, "days"), 1)
  expect_equal(convert_duration_to_years(182.625, "days"), 0.5)
  expect_equal(convert_duration_to_years(30, "days"), 30 / 365.25)
})

test_that("convert_duration_to_years converts weeks correctly", {
  # 1 year = 52.17857 weeks (365.25 / 7)
  weeks_per_year <- 365.25 / 7
  expect_equal(convert_duration_to_years(weeks_per_year, "weeks"), 1)
  expect_equal(convert_duration_to_years(weeks_per_year / 2, "weeks"), 0.5)
  expect_equal(convert_duration_to_years(4, "weeks"), 4 / weeks_per_year)
})

test_that("convert_duration_to_years converts months correctly", {
  # 1 year = 12 months
  expect_equal(convert_duration_to_years(12, "months"), 1)
  expect_equal(convert_duration_to_years(6, "months"), 0.5)
  expect_equal(convert_duration_to_years(3, "months"), 0.25)
})

test_that("convert_duration_to_years handles years correctly", {
  # years should pass through unchanged
  expect_equal(convert_duration_to_years(1, "years"), 1)
  expect_equal(convert_duration_to_years(5, "years"), 5)
  expect_equal(convert_duration_to_years(0.5, "years"), 0.5)
})

test_that("convert_duration_to_years handles zero duration", {
  expect_equal(convert_duration_to_years(0, "hours"), 0)
  expect_equal(convert_duration_to_years(0, "days"), 0)
  expect_equal(convert_duration_to_years(0, "weeks"), 0)
  expect_equal(convert_duration_to_years(0, "months"), 0)
  expect_equal(convert_duration_to_years(0, "years"), 0)
})

test_that("convert_duration_to_years handles decimal durations", {
  expect_equal(convert_duration_to_years(1.5, "years"), 1.5)
  expect_equal(convert_duration_to_years(2.5, "months"), 2.5 / 12)
  expect_equal(convert_duration_to_years(10.5, "days"), 10.5 / 365.25)
})

test_that("convert_duration_to_years validates duration input", {
  # Non-numeric duration
  expect_error(
    convert_duration_to_years("5", "years"),
    "duration must be a single numeric value"
  )

  # Vector duration
  expect_error(
    convert_duration_to_years(c(1, 2), "years"),
    "duration must be a single numeric value"
  )

  # NULL duration
  expect_error(
    convert_duration_to_years(NULL, "years"),
    "duration must be a single numeric value"
  )

  # NA duration
  expect_error(
    convert_duration_to_years(NA, "years"),
    "duration must be a single numeric value"
  )
})

test_that("convert_duration_to_years validates duration_unit input", {
  # Non-character unit
  expect_error(
    convert_duration_to_years(5, 5),
    "duration_unit must be a single character value"
  )

  # Vector unit
  expect_error(
    convert_duration_to_years(5, c("years", "months")),
    "duration_unit must be a single character value"
  )

  # NULL unit
  expect_error(
    convert_duration_to_years(5, NULL),
    "duration_unit must be a single character value"
  )
})

test_that("convert_duration_to_years rejects invalid duration units", {
  expect_error(
    convert_duration_to_years(5, "decades"),
    "Invalid duration_unit: 'decades'"
  )

  expect_error(
    convert_duration_to_years(5, "year"),  # singular, should be "years"
    "Invalid duration_unit: 'year'"
  )

  expect_error(
    convert_duration_to_years(5, "YEARS"),  # wrong case
    "Invalid duration_unit: 'YEARS'"
  )

  # Error message should list valid units
  expect_error(
    convert_duration_to_years(5, "invalid"),
    "Valid units are: hours, days, weeks, months, years"
  )
})

test_that("convert_duration_to_years rejects negative durations", {
  expect_error(
    convert_duration_to_years(-5, "years"),
    "duration must be non-negative"
  )

  expect_error(
    convert_duration_to_years(-0.1, "months"),
    "duration must be non-negative"
  )
})

test_that("convert_duration_to_years produces expected results for realistic scenarios", {
  # 6 months treatment = 0.5 years
  expect_equal(convert_duration_to_years(6, "months"), 0.5)

  # 90 days = roughly 0.246 years
  expect_equal(convert_duration_to_years(90, "days"), 90 / 365.25)

  # 2 weeks = roughly 0.0383 years
  expect_equal(convert_duration_to_years(2, "weeks"), 2 / (365.25 / 7))

  # 5 years of treatment
  expect_equal(convert_duration_to_years(5, "years"), 5)

  # 48 hours = 0.00547 years
  expect_equal(convert_duration_to_years(48, "hours"), 48 / (365.25 * 24))
})

test_that("convert_duration_to_years uses consistent conversion factors", {
  # Verify the standard 365.25 days per year
  days_per_year <- 365.25

  # Test that conversions are consistent across units
  one_year_from_days <- convert_duration_to_years(days_per_year, "days")
  one_year_from_months <- convert_duration_to_years(12, "months")
  one_year_from_weeks <- convert_duration_to_years(days_per_year / 7, "weeks")
  one_year_from_hours <- convert_duration_to_years(days_per_year * 24, "hours")

  expect_equal(one_year_from_days, 1)
  expect_equal(one_year_from_months, 1)
  expect_equal(one_year_from_weeks, 1)
  expect_equal(one_year_from_hours, 1)
})

test_that("convert_duration_to_years maintains precision", {
  # Test that we maintain reasonable precision in calculations
  result <- convert_duration_to_years(100, "days")
  expected <- 100 / 365.25

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that("convert_duration_to_years works with very small durations", {
  # 1 hour
  result <- convert_duration_to_years(1, "hours")
  expect_gt(result, 0)
  expect_lt(result, 0.001)

  # 1 day
  result <- convert_duration_to_years(1, "days")
  expect_gt(result, 0.002)
  expect_lt(result, 0.003)
})

test_that("convert_duration_to_years works with large durations", {
  # 100 years
  expect_equal(convert_duration_to_years(100, "years"), 100)

  # 1200 months = 100 years
  expect_equal(convert_duration_to_years(1200, "months"), 100)

  # 36525 days ≈ 100 years
  expect_equal(convert_duration_to_years(36525, "days"), 100)
})

test_that("convert_duration_to_years example calculations match documentation", {
  # From documentation examples

  # 6 months to years
  expect_equal(convert_duration_to_years(6, "months"), 0.5)

  # 90 days to years (documentation says 0.2464066)
  result <- convert_duration_to_years(90, "days")
  expect_equal(result, 90 / 365.25, tolerance = 1e-7)

  # 2 weeks to years (documentation says 0.03833)
  result <- convert_duration_to_years(2, "weeks")
  expect_equal(result, 2 / (365.25 / 7), tolerance = 1e-5)

  # 5 years unchanged
  expect_equal(convert_duration_to_years(5, "years"), 5)
})