test_that("specify_cost_components validates required columns", {
  # Missing cost_label
  incomplete_df <- tibble::tribble(
    ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_base", "years", 5, NA, 1, 2
  )

  expect_error(
    specify_cost_components(incomplete_df),
    "Missing required columns.*cost_label"
  )

  # Missing base_cost_name
  incomplete_df2 <- tibble::tribble(
    ~cost_label, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "years", 5, NA, 1, 2
  )

  expect_error(
    specify_cost_components(incomplete_df2),
    "Missing required columns.*base_cost_name"
  )

  # Missing edge_from
  incomplete_df3 <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_to,
    "c_comp", "c_base", "years", 5, NA, 2
  )

  expect_error(
    specify_cost_components(incomplete_df3),
    "Missing required columns.*edge_from"
  )
})


test_that("specify_cost_components adds optional columns if missing", {
  # Minimal specification without optional columns
  # After adding optional columns, will have all NA values
  # With duration_unit=NA, this becomes quantity-based with edge lookup (valid)
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~edge_from, ~edge_to,
    "c_comp", "c_base", 1, 2
  )

  result <- specify_cost_components(components)

  # Should add the missing columns
  expect_true("duration_unit" %in% names(result))
  expect_true("duration_value" %in% names(result))
  expect_true("quantity_value" %in% names(result))

  # All optional columns should be NA (quantity-based with edge lookup)
  expect_true(is.na(result$duration_unit[1]))
  expect_true(is.na(result$duration_value[1]))
  expect_true(is.na(result$quantity_value[1]))

  # With duration_unit only (should add duration_value and quantity_value)
  components2 <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~edge_from, ~edge_to,
    "c_comp", "c_base", "years", 1, 2
  )

  result2 <- specify_cost_components(components2)

  expect_true("duration_value" %in% names(result2))
  expect_true("quantity_value" %in% names(result2))
  expect_true(is.na(result2$duration_value[1]))
  expect_true(is.na(result2$quantity_value[1]))
})


test_that("specify_cost_components validates duration_unit values", {
  # Invalid duration_unit
  bad_components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "invalid_unit", 5, NA, 1, 2
  )

  expect_error(
    specify_cost_components(bad_components),
    "Invalid duration_unit values.*invalid_unit"
  )

  # Valid duration_units should work
  valid_components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp1", "c_base", "hours", 24, NA, 1, 2,
    "c_comp2", "c_base", "days", 7, NA, 2, 3,
    "c_comp3", "c_base", "weeks", 4, NA, 3, 4,
    "c_comp4", "c_base", "months", 6, NA, 4, 5,
    "c_comp5", "c_base", "years", 2, NA, 5, 6
  )

  expect_silent(specify_cost_components(valid_components))
})


test_that("specify_cost_components distinguishes duration vs quantity by duration_unit", {
  # Duration-based: duration_unit set, quantity_value must be NA
  duration_with_quantity <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "years", 5, 2, 1, 2
  )

  expect_error(
    specify_cost_components(duration_with_quantity),
    "quantity_value must be NA when duration_unit is specified"
  )

  # Quantity-based: duration_unit NA, can have any quantity_value (including NA for lookup)
  quantity_based_explicit <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", NA, NA, 5, 1, 2
  )

  expect_silent(specify_cost_components(quantity_based_explicit))

  # Quantity-based with edge lookup (all NA is valid for quantity-based)
  quantity_based_lookup <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", NA, NA, NA, 1, 2
  )

  expect_silent(specify_cost_components(quantity_based_lookup))
})


test_that("specify_cost_components validates duration-based costs correctly", {
  # Valid duration-based cost with explicit duration_value
  duration_explicit <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_annual", "years", 5, NA, 1, 2
  )

  expect_silent(specify_cost_components(duration_explicit))

  # Valid duration-based cost with NA duration_value (edge lookup)
  duration_lookup <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_annual", "years", NA, NA, 1, 2
  )

  expect_silent(specify_cost_components(duration_lookup))

  # Invalid: duration_unit set but quantity_value is not NA
  duration_with_quantity <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_annual", "years", 5, 0, 1, 2
  )

  expect_error(
    specify_cost_components(duration_with_quantity),
    "quantity_value must be NA when duration_unit is specified"
  )
})


test_that("specify_cost_components validates quantity-based costs correctly", {
  # Valid quantity-based cost with explicit quantity_value
  quantity_explicit <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_injection", NA, NA, 2, 1, 2
  )

  expect_silent(specify_cost_components(quantity_explicit))

  # Valid quantity-based cost with NA quantity_value (edge lookup)
  quantity_lookup <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_injection", NA, NA, NA, 1, 2
  )

  expect_silent(specify_cost_components(quantity_lookup))

  # Valid one-time cost
  one_time_cost <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_surgery", "c_surgery_base", NA, NA, 1, 3, 4
  )

  expect_silent(specify_cost_components(one_time_cost))

  # Invalid: duration_unit set (duration-based) but quantity_value is not NA
  quantity_with_duration_unit <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_injection", "years", NA, 2, 1, 2
  )

  expect_error(
    specify_cost_components(quantity_with_duration_unit),
    "quantity_value must be NA when duration_unit is specified"
  )

  # Invalid: duration_unit is NA (quantity-based) but duration_value is not NA
  quantity_with_duration_value <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_injection", NA, 5, 2, 1, 2
  )

  expect_error(
    specify_cost_components(quantity_with_duration_value),
    "duration_value must be NA when duration_unit is NA"
  )
})


test_that("specify_cost_components validates numeric edge nodes", {
  # Non-numeric edge nodes
  bad_edges <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", NA, NA, 2, "a", "b"
  )

  expect_error(
    specify_cost_components(bad_edges),
    "edge_from and edge_to must be numeric"
  )
})


test_that("specify_cost_components validates character cost_label and base_cost_name", {
  # Numeric cost_label
  bad_label <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    123, "c_base", NA, NA, 2, 1, 2
  )

  expect_error(
    specify_cost_components(bad_label),
    "cost_label must be character"
  )

  # Numeric base_cost_name
  bad_base <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", 456, NA, NA, 2, 1, 2
  )

  expect_error(
    specify_cost_components(bad_base),
    "base_cost_name must be character"
  )
})


test_that("specify_cost_components sets correct class", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", NA, NA, 2, 1, 2
  )

  result <- specify_cost_components(components)

  expect_s3_class(result, "cost_components")
  expect_s3_class(result, "tbl_df")
})


test_that("specify_cost_components works with data.frame input", {
  # Create a regular data.frame (not tibble)
  components_df <- data.frame(
    cost_label = "c_comp",
    base_cost_name = "c_base",
    duration_unit = NA_character_,
    duration_value = NA_real_,
    quantity_value = 2,
    edge_from = 1,
    edge_to = 2,
    stringsAsFactors = FALSE
  )

  result <- specify_cost_components(components_df)

  expect_s3_class(result, "cost_components")
  expect_s3_class(result, "tbl_df")
})


test_that("specify_cost_components handles complex realistic example", {
  # Mixed duration and quantity components
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_biol_treatment", "c_injection", NA, NA, 2, 4, 7,
    "c_biol_treatment", "c_annual", "years", NA, NA, 7, 10,
    "c_biol_treatment", "c_monthly", "months", 48, NA, 10, 19,
    "c_biol_treatment", "c_surgery", NA, NA, 1, 4, 7
  )

  result <- specify_cost_components(components)

  expect_s3_class(result, "cost_components")
  expect_equal(nrow(result), 4)
  expect_equal(length(unique(result$cost_label)), 1)
  expect_equal(unique(result$cost_label), "c_biol_treatment")

  # Check that duration and quantity components are correctly identified
  duration_components <- sum(!is.na(result$duration_unit))
  quantity_components <- sum(!is.na(result$quantity_value))

  expect_equal(duration_components, 2)
  expect_equal(quantity_components, 2)
})


test_that("specify_cost_components handles multiple cost labels", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_treatment_a", "c_annual", "years", 5, NA, 1, 2,
    "c_treatment_a", "c_injection", NA, NA, 2, 2, 3,
    "c_treatment_b", "c_monthly", "months", 12, NA, 3, 4,
    "c_treatment_b", "c_surgery", NA, NA, 1, 4, 5
  )

  result <- specify_cost_components(components)

  expect_equal(length(unique(result$cost_label)), 2)
  expect_true("c_treatment_a" %in% result$cost_label)
  expect_true("c_treatment_b" %in% result$cost_label)
})


test_that("specify_cost_components validates all time units", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_1", "c_base", "hours", 168, NA, 1, 2,
    "c_2", "c_base", "days", 7, NA, 2, 3,
    "c_3", "c_base", "weeks", 4, NA, 3, 4,
    "c_4", "c_base", "months", 6, NA, 4, 5,
    "c_5", "c_base", "years", 2, NA, 5, 6
  )

  result <- specify_cost_components(components)

  expect_equal(nrow(result), 5)
  expect_true(all(c("hours", "days", "weeks", "months", "years") %in% result$duration_unit))
})


test_that("print.cost_components displays summary information", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp1", "c_annual", "years", 3, NA, 1, 2,
    "c_comp1", "c_monthly", "months", 6, NA, 2, 3,
    "c_comp1", "c_injection", NA, NA, 2, 2, 3,
    "c_comp2", "c_surgery", NA, NA, 1, 3, 4
  )

  result <- specify_cost_components(components)

  # Capture output
  output <- capture.output(print(result))

  expect_true(any(grepl("Cost Components Specification", output)))
  expect_true(any(grepl("Number of composite costs: 2", output)))
  expect_true(any(grepl("Total cost components: 4", output)))
  expect_true(any(grepl("Duration-based components: 2", output)))
  expect_true(any(grepl("Quantity-based components: 2", output)))
  expect_true(any(grepl("Duration units:", output)))
})


test_that("print.cost_components shows duration units for each cost label", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_treatment", "c_annual", "years", 3, NA, 1, 2,
    "c_treatment", "c_monthly", "months", 12, NA, 2, 3
  )

  result <- specify_cost_components(components)
  output <- capture.output(print(result))

  # Should show both years and months as duration units
  expect_true(any(grepl("years", output)))
  expect_true(any(grepl("months", output)))
})
