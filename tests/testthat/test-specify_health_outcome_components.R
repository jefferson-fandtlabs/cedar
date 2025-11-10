test_that("specify_health_outcome_components validates required columns", {
  # Valid minimal specification
  valid_spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  result <- specify_health_outcome_components(valid_spec)
  expect_s3_class(result, "health_outcome_components")
  expect_s3_class(result, "tbl_df")
})

test_that("specify_health_outcome_components adds duration_value column if missing", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  result <- specify_health_outcome_components(spec)
  expect_true("duration_value" %in% names(result))
  expect_true(is.na(result$duration_value[1]))
})

test_that("specify_health_outcome_components preserves existing duration_value column", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  )

  result <- specify_health_outcome_components(spec)
  expect_equal(result$duration_value[1], 5)
})

test_that("specify_health_outcome_components errors on missing outcome_label", {
  spec <- data.frame(
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Missing required columns.*outcome_label"
  )
})

test_that("specify_health_outcome_components errors on missing base_outcome_name for non-life_years", {
  spec <- data.frame(
    outcome_label = "qaly1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "base_outcome_name column required when using outcome types other than 'life_years'"
  )
})

test_that("specify_health_outcome_components errors on missing outcome_type", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Missing required columns.*outcome_type"
  )
})

test_that("specify_health_outcome_components errors on missing duration_unit", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    edge_from = 1,
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Missing required columns.*duration_unit"
  )
})

test_that("specify_health_outcome_components errors on missing edge_from", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Missing required columns.*edge_from"
  )
})

test_that("specify_health_outcome_components errors on missing edge_to", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Missing required columns.*edge_to"
  )
})

test_that("specify_health_outcome_components validates outcome_type values", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "invalid_type",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Invalid outcome_type values: invalid_type"
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Valid values are: utility_to_qaly, life_years, qaly_direct"
  )
})

test_that("specify_health_outcome_components accepts all valid outcome_types", {
  # utility_to_qaly
  spec1 <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )
  expect_s3_class(specify_health_outcome_components(spec1), "health_outcome_components")

  # life_years
  spec2 <- data.frame(
    outcome_label = "ly1",
    base_outcome_name = "ly_base",
    outcome_type = "life_years",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )
  expect_s3_class(specify_health_outcome_components(spec2), "health_outcome_components")

  # qaly_direct
  spec3 <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "precalc_qaly",
    outcome_type = "qaly_direct",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )
  expect_s3_class(specify_health_outcome_components(spec3), "health_outcome_components")
})

test_that("specify_health_outcome_components validates duration_unit values", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "decades",  # invalid
    edge_from = 1,
    edge_to = 2
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Invalid duration_unit values: decades"
  )

  expect_error(
    specify_health_outcome_components(spec),
    "Valid values are: hours, days, weeks, months, years"
  )
})

test_that("specify_health_outcome_components accepts all valid duration_units", {
  for (unit in c("hours", "days", "weeks", "months", "years")) {
    spec <- data.frame(
      outcome_label = "qaly1",
      base_outcome_name = "utility1",
      outcome_type = "utility_to_qaly",
      duration_unit = unit,
      edge_from = 1,
      edge_to = 2
    )
    result <- specify_health_outcome_components(spec)
    expect_s3_class(result, "health_outcome_components")
    expect_equal(result$duration_unit[1], unit)
  }
})

test_that("specify_health_outcome_components converts data.frame to tibble", {
  spec <- data.frame(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  result <- specify_health_outcome_components(spec)
  expect_s3_class(result, "tbl_df")
})

test_that("specify_health_outcome_components handles tibble input", {
  spec <- tibble::tibble(
    outcome_label = "qaly1",
    base_outcome_name = "utility1",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2
  )

  result <- specify_health_outcome_components(spec)
  expect_s3_class(result, "health_outcome_components")
  expect_s3_class(result, "tbl_df")
})

test_that("specify_health_outcome_components handles multiple components", {
  spec <- data.frame(
    outcome_label = c("qaly1", "qaly1", "qaly2"),
    base_outcome_name = c("utility1", "utility2", "utility3"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly", "life_years"),
    duration_unit = c("years", "months", "days"),
    edge_from = c(1, 2, 3),
    edge_to = c(2, 3, 4),
    duration_value = c(5, 6, 365.25)
  )

  result <- specify_health_outcome_components(spec)
  expect_equal(nrow(result), 3)
  expect_s3_class(result, "health_outcome_components")
})

test_that("specify_health_outcome_components print method works", {
  spec <- data.frame(
    outcome_label = c("qaly1", "qaly1", "qaly2"),
    base_outcome_name = c("utility1", "utility2", "utility3"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly", "life_years"),
    duration_unit = c("years", "months", "days"),
    edge_from = c(1, 2, 3),
    edge_to = c(2, 3, 4),
    duration_value = c(5, 6, 365.25)
  )

  result <- specify_health_outcome_components(spec)

  # Capture output
  output <- capture.output(print(result))

  # Check for expected content
  expect_true(any(grepl("Health Outcome Components Specification", output)))
  expect_true(any(grepl("Number of composite outcomes", output)))
  expect_true(any(grepl("Total components", output)))
  expect_true(any(grepl("Components by outcome type", output)))
  expect_true(any(grepl("Components by duration unit", output)))
})

test_that("specify_health_outcome_components print method shows duration value info", {
  spec <- data.frame(
    outcome_label = c("qaly1", "qaly2"),
    base_outcome_name = c("utility1", "utility2"),
    outcome_type = c("utility_to_qaly", "utility_to_qaly"),
    duration_unit = c("years", "months"),
    edge_from = c(1, 2),
    edge_to = c(2, 3),
    duration_value = c(5, NA)
  )

  result <- specify_health_outcome_components(spec)
  output <- capture.output(print(result))

  # Should show explicit vs lookup
  expect_true(any(grepl("Explicit values: 1", output)))
  expect_true(any(grepl("Edge property lookup: 1", output)))
})

test_that("specify_health_outcome_components handles mixed outcome types", {
  spec <- data.frame(
    outcome_label = c("outcome1", "outcome1", "outcome2"),
    base_outcome_name = c("utility1", "precalc_qaly1", "ly1"),
    outcome_type = c("utility_to_qaly", "qaly_direct", "life_years"),
    duration_unit = c("years", "months", "weeks"),
    edge_from = c(1, 2, 3),
    edge_to = c(2, 3, 4)
  )

  result <- specify_health_outcome_components(spec)
  expect_equal(nrow(result), 3)

  # Check that all outcome types are preserved
  expect_true("utility_to_qaly" %in% result$outcome_type)
  expect_true("qaly_direct" %in% result$outcome_type)
  expect_true("life_years" %in% result$outcome_type)
})

test_that("specify_health_outcome_components handles mixed duration units", {
  spec <- data.frame(
    outcome_label = c("qaly1", "qaly1", "qaly1", "qaly1", "qaly1"),
    base_outcome_name = rep("utility1", 5),
    outcome_type = rep("utility_to_qaly", 5),
    duration_unit = c("hours", "days", "weeks", "months", "years"),
    edge_from = 1:5,
    edge_to = 2:6
  )

  result <- specify_health_outcome_components(spec)
  expect_equal(nrow(result), 5)

  # Check that all duration units are preserved
  expect_setequal(result$duration_unit, c("hours", "days", "weeks", "months", "years"))
})

# Tests for optional base_outcome_name column (life_years functionality)

test_that("base_outcome_name column is optional for life_years only", {
  # Should work - all life_years, no base_outcome_name column
  spec_valid <- data.frame(
    outcome_label = c("ly_total", "ly_total"),
    outcome_type = c("life_years", "life_years"),
    duration_unit = c("years", "months"),
    edge_from = c(1, 2),
    edge_to = c(2, 3),
    duration_value = c(10, 24)
  )

  result <- specify_health_outcome_components(spec_valid)
  expect_s3_class(result, "health_outcome_components")
  expect_equal(nrow(result), 2)
  expect_false("base_outcome_name" %in% names(result))
})

test_that("base_outcome_name required for utility_to_qaly when column missing", {
  # Should fail - utility_to_qaly without base_outcome_name column
  spec_invalid <- data.frame(
    outcome_label = "qaly",
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  )

  expect_error(
    specify_health_outcome_components(spec_invalid),
    "base_outcome_name column required when using outcome types other than 'life_years'"
  )
})

test_that("base_outcome_name required for qaly_direct when column missing", {
  # Should fail - qaly_direct without base_outcome_name column
  spec_invalid <- data.frame(
    outcome_label = "qaly",
    outcome_type = "qaly_direct",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = NA
  )

  expect_error(
    specify_health_outcome_components(spec_invalid),
    "base_outcome_name column required when using outcome types other than 'life_years'"
  )
})

test_that("base_outcome_name required for mixed types when column missing", {
  # Should fail - mixed types without base_outcome_name column
  spec_invalid <- data.frame(
    outcome_label = c("outcome1", "outcome2"),
    outcome_type = c("life_years", "utility_to_qaly"),
    duration_unit = c("years", "years"),
    edge_from = c(1, 2),
    edge_to = c(2, 3),
    duration_value = c(10, 5)
  )

  expect_error(
    specify_health_outcome_components(spec_invalid),
    "base_outcome_name column required when using outcome types other than 'life_years'"
  )
})

test_that("base_outcome_name can be NA for life_years when column present", {
  # Should work - life_years with NA, others with values
  spec_mixed <- data.frame(
    outcome_label = c("ly", "qaly"),
    base_outcome_name = c(NA, "util_val"),
    outcome_type = c("life_years", "utility_to_qaly"),
    duration_unit = c("years", "years"),
    edge_from = c(1, 2),
    edge_to = c(2, 3),
    duration_value = c(10, 5)
  )

  result <- specify_health_outcome_components(spec_mixed)
  expect_s3_class(result, "health_outcome_components")
  expect_equal(nrow(result), 2)
  expect_true(is.na(result$base_outcome_name[1]))
  expect_equal(result$base_outcome_name[2], "util_val")
})

test_that("base_outcome_name cannot be NA for utility_to_qaly", {
  # Should fail - utility_to_qaly with NA base_outcome_name
  spec_invalid <- data.frame(
    outcome_label = "qaly",
    base_outcome_name = NA,
    outcome_type = "utility_to_qaly",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = 5
  )

  expect_error(
    specify_health_outcome_components(spec_invalid),
    "base_outcome_name cannot be NA for outcome types 'utility_to_qaly' or 'qaly_direct'"
  )
})

test_that("base_outcome_name cannot be NA for qaly_direct", {
  # Should fail - qaly_direct with NA base_outcome_name
  spec_invalid <- data.frame(
    outcome_label = "qaly",
    base_outcome_name = NA,
    outcome_type = "qaly_direct",
    duration_unit = "years",
    edge_from = 1,
    edge_to = 2,
    duration_value = NA
  )

  expect_error(
    specify_health_outcome_components(spec_invalid),
    "base_outcome_name cannot be NA for outcome types 'utility_to_qaly' or 'qaly_direct'"
  )
})

test_that("multiple life_years rows can all have NA base_outcome_name", {
  # Should work - all life_years with NA
  spec_valid <- data.frame(
    outcome_label = c("survival", "survival", "survival"),
    base_outcome_name = c(NA, NA, NA),
    outcome_type = c("life_years", "life_years", "life_years"),
    duration_unit = c("years", "months", "days"),
    edge_from = c(1, 2, 3),
    edge_to = c(2, 3, 4),
    duration_value = c(10, 12, 365)
  )

  result <- specify_health_outcome_components(spec_valid)
  expect_s3_class(result, "health_outcome_components")
  expect_equal(nrow(result), 3)
  expect_true(all(is.na(result$base_outcome_name)))
})