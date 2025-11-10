test_that("calculate_composite_costs validates cost_components class", {
  # Not a cost_components object
  bad_components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", "quantity", 2, 1, 2
  )

  base_costs <- list(c_base = 1000)

  expect_error(
    calculate_composite_costs(bad_components, base_costs),
    "cost_components must be created using specify_cost_components"
  )
})


test_that("calculate_composite_costs validates base_costs is named list", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "quantity", 2, 1, 2
    )
  )

  # Not a list
  expect_error(
    calculate_composite_costs(components, c(c_base = 1000)),
    "base_costs must be a named list"
  )

  # Unnamed list
  expect_error(
    calculate_composite_costs(components, list(1000)),
    "base_costs must be a named list"
  )
})


test_that("calculate_composite_costs validates discount_rate", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "quantity", 2, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Non-numeric discount_rate
  expect_error(
    calculate_composite_costs(components, base_costs, discount_rate = "0.03"),
    "discount_rate must be a single numeric value or NULL"
  )

  # Negative discount_rate
  expect_error(
    calculate_composite_costs(components, base_costs, discount_rate = -0.03),
    "discount_rate must be non-negative"
  )

  # NULL should work
  expect_silent(
    calculate_composite_costs(components, base_costs, discount_rate = NULL)
  )
})


test_that("calculate_composite_costs checks for missing base costs", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_missing", "quantity", 2, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  expect_error(
    calculate_composite_costs(components, base_costs),
    "Missing base costs: c_missing"
  )
})


test_that("calculate_composite_costs requires edge_properties when needed", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", NA, 1, 2  # NA requires edge lookup
    )
  )

  base_costs <- list(c_base = 1000)

  expect_error(
    calculate_composite_costs(components, base_costs, edge_properties = NULL),
    "edge_properties required: some cost components have NA multiplier_value"
  )
})


test_that("calculate_composite_costs validates edge_properties structure", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Missing from_node
  bad_edge_props <- tibble::tribble(
    ~to_node, ~duration,
    2, 3
  )

  expect_error(
    calculate_composite_costs(components, base_costs, bad_edge_props),
    "edge_properties must contain 'from_node' and 'to_node' columns"
  )

  # Missing multiplier_type column
  bad_edge_props2 <- tibble::tribble(
    ~from_node, ~to_node,
    1, 2
  )

  expect_error(
    calculate_composite_costs(components, base_costs, bad_edge_props2),
    "Missing columns in edge_properties.*duration"
  )
})


test_that("calculate_composite_costs calculates simple quantity multiplier", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "quantity", 2, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_equal(result$c_comp, 2000)
})


test_that("calculate_composite_costs handles multiplier_type = none", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "none", NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_equal(result$c_comp, 1000)
})


test_that("calculate_composite_costs looks up multiplier from edge_properties", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", NA, 7, 10
    )
  )

  base_costs <- list(c_base = 5000)

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~duration,
    7, 10, 1
  )

  result <- calculate_composite_costs(components, base_costs, edge_props)

  # Without discounting, should be 5000 * 1 = 5000
  expect_equal(result$c_comp, 5000)
})


test_that("calculate_composite_costs applies discounting for duration multipliers", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", 3, 1, 2
    )
  )

  base_costs <- list(c_base = 5000)

  result_with_discount <- calculate_composite_costs(
    components,
    base_costs,
    discount_rate = 0.03
  )

  # Should use apply_discount with offset=1 (old behavior - will be updated when offsets are computed)
  expected <- apply_discount(5000, 3, 0.03, 1)
  expect_equal(result_with_discount$c_comp, expected, tolerance = 0.001)

  # Without discount
  result_no_discount <- calculate_composite_costs(
    components,
    base_costs,
    discount_rate = NULL
  )

  expect_equal(result_no_discount$c_comp, 15000)
})


test_that("calculate_composite_costs does not discount quantity multipliers", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "quantity", 3, 1, 2
    )
  )

  base_costs <- list(c_base = 30000)

  # Even with discount_rate, quantity should not be discounted
  result <- calculate_composite_costs(
    components,
    base_costs,
    discount_rate = 0.03
  )

  expect_equal(result$c_comp, 90000)
})


test_that("calculate_composite_costs sums multiple components for same cost_label", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base1", "quantity", 2, 1, 2,
      "c_comp", "c_base2", "quantity", 3, 2, 3,
      "c_comp", "c_base3", "none", NA, 3, 4
    )
  )

  base_costs <- list(
    c_base1 = 1000,
    c_base2 = 500,
    c_base3 = 200
  )

  result <- calculate_composite_costs(components, base_costs)

  # 1000*2 + 500*3 + 200*1 = 2000 + 1500 + 200 = 3700
  expect_equal(result$c_comp, 3700)
})


test_that("calculate_composite_costs handles multiple cost_labels", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp1", "c_base", "quantity", 2, 1, 2,
      "c_comp2", "c_base", "quantity", 3, 2, 3
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_equal(length(result), 2)
  expect_equal(result$c_comp1, 2000)
  expect_equal(result$c_comp2, 3000)
})


test_that("calculate_composite_costs reproduces c_biol_2_success example", {
  # Reproduce the example from Part2_Model_McMillanWilhoit_CP.R
  base_costs <- list(
    c_biologic = 30000,
    c_fail_year = 5000,
    c_success_year = 1000
  )

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~duration,
    4, 7, 1,    # Placeholder (quantity is specified)
    7, 10, 1,   # 1 year between 7 and 10
    10, 19, 4   # 4 years between 10 and 19
  )

  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_biol_2_success", "c_biologic", "quantity", 1, 4, 7,
      "c_biol_2_success", "c_biologic", "quantity", 1, 7, 10,
      "c_biol_2_success", "c_fail_year", "duration", NA, 7, 10,
      "c_biol_2_success", "c_success_year", "duration", NA, 10, 19
    )
  )

  discount_rate <- 0.03

  result <- calculate_composite_costs(
    components,
    base_costs,
    edge_props,
    discount_rate
  )

  # Manual calculation with computed offsets:
  # Edge 4->7: orphaned, gets offset=0, duration=1 -> node 7 time = 0+1 = 1
  # Edge 7->10: offset=1 (time at node 7), duration=1 -> node 10 time = 1+1 = 2
  # Edge 10->19: offset=2 (time at node 10), duration=4
  #
  # - c_biologic * 1 (4->7, quantity, no discount) = 30000
  # - c_biologic * 1 (7->10, quantity, no discount) = 30000
  # - c_fail_year over 1 year (7->10, duration, offset=1) = apply_discount(5000, 1, 0.03, 1)
  # - c_success_year over 4 years (10->19, duration, offset=2) = apply_discount(1000, 4, 0.03, 2)

  expected <- 30000 + 30000 +
              apply_discount(5000, 1, 0.03, 1) +
              apply_discount(1000, 4, 0.03, 2)

  expect_equal(result$c_biol_2_success, expected, tolerance = 0.01)
})


test_that("calculate_composite_costs errors on missing edge property", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Edge properties for different edge
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~duration,
    2, 3, 1
  )

  expect_error(
    calculate_composite_costs(components, base_costs, edge_props),
    "No edge property found for edge 1 -> 2"
  )
})


test_that("calculate_composite_costs errors on duplicate edge property", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Duplicate edge properties
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~duration,
    1, 2, 1,
    1, 2, 2
  )

  expect_error(
    calculate_composite_costs(components, base_costs, edge_props),
    "Multiple edge properties found for edge 1 -> 2"
  )
})


test_that("calculate_composite_costs errors on NA in edge_properties lookup", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "duration", NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Edge property has NA duration
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~duration,
    1, 2, NA
  )

  expect_error(
    calculate_composite_costs(components, base_costs, edge_props),
    "NA value in edge_properties\\$duration for edge 1 -> 2"
  )
})


test_that("calculate_composite_costs handles mixed specified and lookup multipliers", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp", "c_base1", "quantity", 2, 1, 2,      # Specified
      "c_comp", "c_base2", "duration", NA, 7, 10     # Lookup
    )
  )

  base_costs <- list(
    c_base1 = 30000,
    c_base2 = 1000
  )

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~duration,
    7, 10, 4
  )

  result <- calculate_composite_costs(
    components,
    base_costs,
    edge_props,
    discount_rate = 0.03
  )

  # With computed offsets:
  # Edge 7->10: orphaned (node 7 not in edge list as to_node), gets offset=0
  # - c_base1 * 2 (quantity, no discount) = 60000
  # - c_base2 over 4 years (7->10, duration, offset=0) = apply_discount(1000, 4, 0.03, 0)
  expected <- 60000 + apply_discount(1000, 4, 0.03, 0)

  expect_equal(result$c_comp, expected, tolerance = 0.01)
})


test_that("calculate_composite_costs returns named list with correct structure", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
      "c_comp1", "c_base", "quantity", 2, 1, 2,
      "c_comp2", "c_base", "quantity", 3, 2, 3
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_type(result, "list")
  expect_named(result, c("c_comp1", "c_comp2"))
  expect_true(all(sapply(result, is.numeric)))
})