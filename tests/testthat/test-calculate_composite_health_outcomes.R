test_that("calculate_composite_health_outcomes calculates utility_to_qaly correctly", {
  # Simple utility to QALY conversion
  base_outcomes <- list(utility_success = 0.85)

  outcome_components <- data.frame(
    outcome_label = "qaly_success",
    base_outcome_name = "utility_success",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 2,
    edge_to = 4,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # 0.85 utility * 5 years = 4.25 QALYs
  expect_equal(result$qaly_success, 4.25)
})

test_that("calculate_composite_health_outcomes handles different duration units", {
  base_outcomes <- list(utility1 = 0.8)

  # Test with months
  components_months <- data.frame(
    outcome_label = "qaly_months",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "months",
    edge_from = 1,
    edge_to = 2,
    duration_value = 6
  ) |>
    specify_health_outcome_components()

  result_months <- calculate_composite_health_outcomes(
    components_months,
    base_outcomes
  )

  # 0.8 utility * 0.5 years = 0.4 QALYs
  expect_equal(result_months$qaly_months, 0.4)

  # Test with days
  components_days <- data.frame(
    outcome_label = "qaly_days",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "days",
    edge_from = 1,
    edge_to = 2,
    duration_value = 365.25
  ) |>
    specify_health_outcome_components()

  result_days <- calculate_composite_health_outcomes(
    components_days,
    base_outcomes
  )

  # 0.8 utility * 1 year = 0.8 QALYs
  expect_equal(result_days$qaly_days, 0.8)
})

test_that("calculate_composite_health_outcomes calculates life_years correctly", {
  base_outcomes <- list(ly_multiplier = 1.0)

  outcome_components <- data.frame(
    outcome_label = "life_years",
    base_outcome_name = "ly_multiplier",
    outcome_type = "life_years",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 10
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # 1.0 * 10 years = 10 life years
  expect_equal(result$life_years, 10)
})

test_that("calculate_composite_health_outcomes handles qaly_direct correctly", {
  base_outcomes <- list(precalc_qaly = 2.5)

  outcome_components <- data.frame(
    outcome_label = "direct_qaly",
    base_outcome_name = "precalc_qaly",
    outcome_type = "qaly_direct",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5  # Not used for qaly_direct
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # Should use base outcome directly, ignoring duration
  expect_equal(result$direct_qaly, 2.5)
})

test_that("calculate_composite_health_outcomes sums multiple components", {
  base_outcomes <- list(
    utility1 = 0.8,
    utility2 = 0.6,
    precalc = 1.5
  )

  outcome_components <- data.frame(
    outcome_label = c("total_qaly", "total_qaly", "total_qaly"),
    base_outcome_name = c("utility1", "utility2", "precalc"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly", "qaly_direct"),
    duration_unit = c("years", "months", "years"),
    edge_from = c(1, 2, 3),
    edge_to = c(2, 3, 4),
    duration_value = c(3, 6, 1)  # qaly_direct doesn't use duration, but we provide a value
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # (0.8 * 3) + (0.6 * 0.5) + 1.5 = 2.4 + 0.3 + 1.5 = 4.2
  expect_equal(result$total_qaly, 4.2)
})

test_that("calculate_composite_health_outcomes uses edge_properties for duration lookup", {
  base_outcomes <- list(utility1 = 0.85)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 2,
    edge_to = 4
    # duration_value omitted (will be NA)
  ) |>
    specify_health_outcome_components()

  edge_properties <- data.frame(
    from_node = c(2, 3),
    to_node = c(4, 5),
    years = c(5, 3)
  )

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes,
    edge_properties
  )

  # 0.85 * 5 = 4.25
  expect_equal(result$qaly1, 4.25)
})

test_that("calculate_composite_health_outcomes handles multiple duration unit columns in edge_properties", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = c("qaly1", "qaly1"),
    base_outcome_name = c("utility1", "utility1"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly"),
    duration_unit = c("years", "months"),
    edge_from = c(1, 2),
    edge_to = c(2, 3)
  ) |>
    specify_health_outcome_components()

  edge_properties <- data.frame(
    from_node = c(1, 2),
    to_node = c(2, 3),
    years = c(2, NA),
    months = c(NA, 6)
  )

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes,
    edge_properties
  )

  # (0.8 * 2) + (0.8 * 0.5) = 1.6 + 0.4 = 2.0
  expect_equal(result$qaly1, 2.0)
})

test_that("calculate_composite_health_outcomes validates outcome_components class", {
  base_outcomes <- list(utility1 = 0.8)

  # Not a health_outcome_components object
  invalid_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  )

  expect_error(
    calculate_composite_health_outcomes(invalid_components, base_outcomes),
    "outcome_components must be created using specify_health_outcome_components"
  )
})

test_that("calculate_composite_health_outcomes validates base_outcomes is a list", {
  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, c(utility1 = 0.8)),
    "base_outcomes must be a named list"
  )
})

test_that("calculate_composite_health_outcomes validates base_outcomes is named", {
  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, list(0.8)),
    "All elements in base_outcomes must be named"
  )
})

test_that("calculate_composite_health_outcomes validates base_outcomes is not empty", {
  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, list()),
    "base_outcomes cannot be empty"
  )
})

test_that("calculate_composite_health_outcomes detects missing base outcomes", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility_missing",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes),
    "Referenced base outcomes not found in base_outcomes: utility_missing"
  )
})

test_that("calculate_composite_health_outcomes requires edge_properties when duration_value is NA", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
    # duration_value will be NA
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes),
    "edge_properties must be provided when outcome_components contains NA duration_value"
  )
})

test_that("calculate_composite_health_outcomes validates edge_properties columns", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  ) |>
    specify_health_outcome_components()

  # Missing from_node column
  edge_properties <- data.frame(
    to_node = c(2),
    years = c(5)
  )

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes, edge_properties),
    "edge_properties must contain 'from_node' and 'to_node' columns"
  )
})

test_that("calculate_composite_health_outcomes detects missing duration columns in edge_properties", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "months",  # Needs "months" column
    edge_from = 1,
    edge_to = 2
  ) |>
    specify_health_outcome_components()

  edge_properties <- data.frame(
    from_node = c(1),
    to_node = c(2),
    years = c(5)  # Has "years" but not "months"
  )

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes, edge_properties),
    "edge_properties is missing columns needed for duration lookup: months"
  )
})

test_that("calculate_composite_health_outcomes detects missing edges in edge_properties", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  ) |>
    specify_health_outcome_components()

  edge_properties <- data.frame(
    from_node = c(2),
    to_node = c(3),
    years = c(5)
  )

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes, edge_properties),
    "No matching edge found in edge_properties for 1 -> 2"
  )
})

test_that("calculate_composite_health_outcomes detects duplicate edges in edge_properties", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  ) |>
    specify_health_outcome_components()

  edge_properties <- data.frame(
    from_node = c(1, 1),
    to_node = c(2, 2),
    years = c(5, 6)
  )

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes, edge_properties),
    "Multiple matching edges found in edge_properties for 1 -> 2"
  )
})

test_that("calculate_composite_health_outcomes detects NA duration values in edge_properties", {
  base_outcomes <- list(utility1 = 0.8)

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  ) |>
    specify_health_outcome_components()

  edge_properties <- data.frame(
    from_node = c(1),
    to_node = c(2),
    years = c(NA)
  )

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes, edge_properties),
    "Duration value is NA in edge_properties for edge 1 -> 2, column 'years'"
  )
})

test_that("calculate_composite_health_outcomes validates base outcome values are numeric", {
  base_outcomes <- list(utility1 = "not_numeric")

  outcome_components <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes),
    "Base outcome 'utility1' must be a single numeric value"
  )
})

test_that("calculate_composite_health_outcomes handles multiple outcome labels", {
  base_outcomes <- list(
    utility_success = 0.85,
    utility_failure = 0.65
  )

  outcome_components <- data.frame(
    outcome_label = c("qaly_success", "qaly_failure"),
    base_outcome_name = c("utility_success", "utility_failure"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly"),
    duration_unit = c("years", "years"),
    edge_from = c(2, 2),
    edge_to = c(4, 5),
    duration_value = c(5, 2)
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  expect_equal(result$qaly_success, 4.25)
  expect_equal(result$qaly_failure, 1.3)
  expect_equal(length(result), 2)
})

test_that("calculate_composite_health_outcomes realistic scenario with mixed types", {
  # Realistic scenario: Treatment pathway with utility gains, life years, and pre-calculated QALYs
  base_outcomes <- list(
    utility_treatment = 0.80,
    utility_maintenance = 0.85,
    ly_multiplier = 1.0,
    literature_qaly = 2.5
  )

  outcome_components <- data.frame(
    outcome_label = c("total_benefit", "total_benefit", "total_benefit", "total_benefit"),
    base_outcome_name = c("utility_treatment", "utility_maintenance", "ly_multiplier", "literature_qaly"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly", "life_years", "qaly_direct"),
    duration_unit = c("months", "years", "years", "years"),
    edge_from = c(1, 2, 3, 4),
    edge_to = c(2, 3, 4, 5),
    duration_value = c(6, 4, 5, 1)  # qaly_direct doesn't use duration, but we provide a value
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # (0.80 * 0.5) + (0.85 * 4) + (1.0 * 5) + 2.5
  # = 0.4 + 3.4 + 5.0 + 2.5
  # = 11.3
  expect_equal(result$total_benefit, 11.3)
})

test_that("calculate_composite_health_outcomes handles weeks and days correctly", {
  base_outcomes <- list(utility1 = 0.9)

  outcome_components <- data.frame(
    outcome_label = c("qaly1", "qaly1"),
    base_outcome_name = c("utility1", "utility1"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly"),
    duration_unit = c("weeks", "days"),
    edge_from = c(1, 2),
    edge_to = c(2, 3),
    duration_value = c(52, 365.25)  # Both approximately 1 year
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # 52 weeks is not exactly 1 year (52.17857 weeks = 1 year), so there's a small difference
  # 52 weeks = 0.9966 years, so 0.9 * 0.9966 + 0.9 * 1 = 0.897 + 0.9 = 1.797
  expect_equal(result$qaly1, 1.797, tolerance = 1e-2)
})

test_that("calculate_composite_health_outcomes handles hours correctly", {
  base_outcomes <- list(utility1 = 0.95)

  outcome_components <- data.frame(
    outcome_label = "qaly_hours",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "hours",
    edge_from = 1,
    edge_to = 2,
    duration_value = 8766  # Approximately 1 year
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(
    outcome_components,
    base_outcomes
  )

  # Should be approximately 0.95 QALYs
  expect_equal(result$qaly_hours, 0.95, tolerance = 1e-6)
})

# Tests for life_years auto-value functionality

test_that("life_years works without base_outcome_name column", {
  outcome_components <- data.frame(
    outcome_label = "life_years",
    outcome_type = "life_years",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 10
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(outcome_components, base_outcomes = NULL)

  # 1.0 * 10 years = 10 life years (auto-value)
  expect_equal(result$life_years, 10)
})

test_that("life_years works with NULL base_outcomes", {
  outcome_components <- data.frame(
    outcome_label = c("ly_total", "ly_total"),
    outcome_type = c("life_years", "life_years"),
    duration_unit = c("years", "months"),
    edge_from = c(1, 2),
    edge_to = c(2, 3),
    duration_value = c(5, 24)
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(outcome_components)

  # 1.0 * 5 years + 1.0 * 2 years = 7 life years
  expect_equal(result$ly_total, 7)
})

test_that("life_years ignores provided base_outcome_name value", {
  outcome_components <- data.frame(
    outcome_label = "life_years",
    base_outcome_name = "ignored_value",
    outcome_type = "life_years",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 10
  ) |>
    specify_health_outcome_components()

  base_outcomes <- list(ignored_value = 999.0)  # Should not be used
  result <- calculate_composite_health_outcomes(outcome_components, base_outcomes)

  # Uses 1.0, not 999.0
  expect_equal(result$life_years, 10)
})

test_that("NULL base_outcomes fails for utility_to_qaly", {
  outcome_components <- data.frame(
    outcome_label = "qaly",
    base_outcome_name = "util_val",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes = NULL),
    "base_outcomes required for non-life_years"
  )
})

test_that("NULL base_outcomes fails for qaly_direct", {
  outcome_components <- data.frame(
    outcome_label = "qaly",
    base_outcome_name = "precalc",
    outcome_type = "qaly_direct",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = NA
  ) |>
    specify_health_outcome_components()

  expect_error(
    calculate_composite_health_outcomes(outcome_components, base_outcomes = NULL),
    "base_outcomes required for non-life_years"
  )
})

test_that("mixed outcomes work with simplified life_years", {
  # Only need base_outcomes for non-life_years
  base_outcomes <- list(
    utility1 = 0.80,
    precalc_qaly = 2.5
  )

  outcome_components <- data.frame(
    outcome_label = c("total_qaly", "total_qaly", "life_years"),
    base_outcome_name = c("utility1", "precalc_qaly", NA),
    outcome_type = c("utility_to_qaly", "qaly_direct", "life_years"),
    duration_unit = c("years", "years", "years"),
    edge_from = c(1, 2, 1),
    edge_to = c(2, 3, 2),
    duration_value = c(3, 1, 10)  # qaly_direct doesn't use duration, but needs a value
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(outcome_components, base_outcomes)

  # total_qaly = (0.80 * 3) + 2.5 = 4.9
  expect_equal(result$total_qaly, 4.9)

  # life_years = 1.0 * 10 = 10
  expect_equal(result$life_years, 10)
})

test_that("life_years with different duration units all use 1.0", {
  outcome_components <- data.frame(
    outcome_label = rep("survival", 5),
    outcome_type = rep("life_years", 5),
    duration_unit = c("hours", "days", "weeks", "months", "years"),
    edge_from = 1:5,
    edge_to = 2:6,
    duration_value = c(8766, 365.25, 52.17857, 12, 1)  # All approximately 1 year
  ) |>
    specify_health_outcome_components()

  result <- calculate_composite_health_outcomes(outcome_components)

  # All should sum to approximately 5 life years (5 * 1 year)
  expect_equal(result$survival, 5, tolerance = 1e-3)
})

test_that("life_years calculation matches old behavior with ly_multiplier=1.0", {
  # Old way (still works but ignores the value)
  old_way <- data.frame(
    outcome_label = "life_years",
    base_outcome_name = "ly_mult",
    outcome_type = "life_years",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 7.5
  ) |>
    specify_health_outcome_components()

  old_result <- calculate_composite_health_outcomes(
    old_way,
    list(ly_mult = 1.0)
  )

  # New way (simplified)
  new_way <- data.frame(
    outcome_label = "life_years",
    outcome_type = "life_years",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 7.5
  ) |>
    specify_health_outcome_components()

  new_result <- calculate_composite_health_outcomes(new_way)

  # Both should give same result
  expect_equal(old_result$life_years, new_result$life_years)
  expect_equal(new_result$life_years, 7.5)
})