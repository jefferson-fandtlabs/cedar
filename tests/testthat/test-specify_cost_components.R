test_that("specify_cost_components validates required columns", {
  # Missing cost_label
  incomplete_df <- tibble::tribble(
    ~base_cost_name, ~multiplier_type, ~edge_from, ~edge_to,
    "c_base", "quantity", 1, 2
  )

  expect_error(
    specify_cost_components(incomplete_df),
    "Missing required columns.*cost_label"
  )

  # Missing multiplier_type
  incomplete_df2 <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~edge_from, ~edge_to,
    "c_comp", "c_base", 1, 2
  )

  expect_error(
    specify_cost_components(incomplete_df2),
    "Missing required columns.*multiplier_type"
  )
})


test_that("specify_cost_components adds multiplier_value column if missing", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~edge_from, ~edge_to,
    "c_comp", "c_base", "quantity", 1, 2
  )

  result <- specify_cost_components(components)

  expect_true("multiplier_value" %in% names(result))
  expect_true(is.na(result$multiplier_value[1]))
})


test_that("specify_cost_components validates multiplier_type values", {
  # Invalid multiplier_type
  bad_components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "invalid_type", 2, 1, 2
  )

  expect_error(
    specify_cost_components(bad_components),
    "Invalid multiplier_type values.*invalid_type"
  )

  # Valid multiplier_types should work
  valid_components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp1", "c_base", "duration", 3, 1, 2,
    "c_comp2", "c_base", "quantity", 2, 2, 3,
    "c_comp3", "c_base", "none", NA, 3, 4
  )

  expect_silent(specify_cost_components(valid_components))
})


test_that("specify_cost_components validates numeric edge nodes", {
  # Non-numeric edge nodes
  bad_edges <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "quantity", 2, "a", "b"
  )

  expect_error(
    specify_cost_components(bad_edges),
    "edge_from and edge_to must be numeric"
  )
})


test_that("specify_cost_components validates character cost_label and base_cost_name", {
  # Numeric cost_label
  bad_label <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    123, "c_base", "quantity", 2, 1, 2
  )

  expect_error(
    specify_cost_components(bad_label),
    "cost_label must be character"
  )

  # Numeric base_cost_name
  bad_base <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", 456, "quantity", 2, 1, 2
  )

  expect_error(
    specify_cost_components(bad_base),
    "base_cost_name must be character"
  )
})


test_that("specify_cost_components warns for none with non-1 multiplier_value", {
  components_with_value <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "none", 2, 1, 2
  )

  expect_warning(
    specify_cost_components(components_with_value),
    "multiplier_type = 'none' but multiplier_value is not 1 or NA"
  )

  # No warning for multiplier_value = 1
  components_with_one <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "none", 1, 1, 2
  )

  expect_silent(specify_cost_components(components_with_one))

  # No warning for multiplier_value = NA
  components_with_na <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "none", NA, 1, 2
  )

  expect_silent(specify_cost_components(components_with_na))
})


test_that("specify_cost_components sets correct class", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "quantity", 2, 1, 2
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
    multiplier_type = "quantity",
    multiplier_value = 2,
    edge_from = 1,
    edge_to = 2,
    stringsAsFactors = FALSE
  )

  result <- specify_cost_components(components_df)

  expect_s3_class(result, "cost_components")
  expect_s3_class(result, "tbl_df")
})


test_that("specify_cost_components handles complex realistic example", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_biol_2_success", "c_biologic", "quantity", 1, 4, 7,
    "c_biol_2_success", "c_biologic", "quantity", 1, 7, 10,
    "c_biol_2_success", "c_fail_year", "duration", NA, 7, 10,
    "c_biol_2_success", "c_success_year", "duration", NA, 10, 19
  )

  result <- specify_cost_components(components)

  expect_s3_class(result, "cost_components")
  expect_equal(nrow(result), 4)
  expect_equal(length(unique(result$cost_label)), 1)
  expect_equal(unique(result$cost_label), "c_biol_2_success")
})


test_that("print.cost_components displays summary information", {
  components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp1", "c_base1", "quantity", 2, 1, 2,
    "c_comp1", "c_base2", "duration", 3, 2, 3,
    "c_comp2", "c_base3", "none", NA, 3, 4
  )

  result <- specify_cost_components(components)

  # Capture output
  output <- capture.output(print(result))

  expect_true(any(grepl("Cost Components Specification", output)))
  expect_true(any(grepl("Number of composite costs: 2", output)))
  expect_true(any(grepl("Total cost components: 3", output)))
})