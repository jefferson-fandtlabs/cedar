test_that("calculate_composite_costs validates cost_components class", {
  # Not a cost_components object
  bad_components <- tibble::tribble(
    ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
    "c_comp", "c_base", NA, NA, 2, 1, 2
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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", NA, NA, 2, 1, 2
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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", NA, NA, 2, 1, 2
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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_missing", NA, NA, 2, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  expect_error(
    calculate_composite_costs(components, base_costs),
    "Missing base costs: c_missing"
  )
})


test_that("calculate_composite_costs requires edge_properties when duration_value is NA", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "years", NA, NA, 1, 2  # NA requires edge lookup
    )
  )

  base_costs <- list(c_base = 1000)

  expect_error(
    calculate_composite_costs(components, base_costs, edge_properties = NULL),
    "edge_properties required: some cost components have NA duration_value or quantity_value"
  )
})


test_that("calculate_composite_costs requires edge_properties when quantity_value is NA", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", NA, NA, NA, 1, 2  # NA requires edge lookup
    )
  )

  base_costs <- list(c_base = 1000)

  expect_error(
    calculate_composite_costs(components, base_costs, edge_properties = NULL),
    "edge_properties required: some cost components have NA duration_value or quantity_value"
  )
})


test_that("calculate_composite_costs validates edge_properties structure", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "years", NA, NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Missing from_node
  bad_edge_props <- tibble::tribble(
    ~to_node, ~years,
    2, 3
  )

  expect_error(
    calculate_composite_costs(components, base_costs, bad_edge_props),
    "edge_properties must contain 'from_node' and 'to_node' columns"
  )

  # Missing duration_unit column
  bad_edge_props2 <- tibble::tribble(
    ~from_node, ~to_node,
    1, 2
  )

  expect_error(
    calculate_composite_costs(components, base_costs, bad_edge_props2),
    "Missing columns in edge_properties for duration lookup: years"
  )
})


test_that("calculate_composite_costs requires quantity column when needed", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", NA, NA, NA, 1, 2  # NA quantity requires edge lookup
    )
  )

  base_costs <- list(c_base = 1000)

  # Missing quantity column
  bad_edge_props <- tibble::tribble(
    ~from_node, ~to_node,
    1, 2
  )

  expect_error(
    calculate_composite_costs(components, base_costs, bad_edge_props),
    "Missing 'quantity' column in edge_properties"
  )
})


test_that("calculate_composite_costs calculates simple quantity multiplier", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", NA, NA, 2, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_equal(result$c_comp, 2000)
})


test_that("calculate_composite_costs handles one-time costs (quantity = 1)", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_surgery", "c_surgery_base", NA, NA, 1, 1, 2
    )
  )

  base_costs <- list(c_surgery_base = 50000)

  result <- calculate_composite_costs(components, base_costs)

  expect_equal(result$c_surgery, 50000)
})


test_that("calculate_composite_costs converts years duration correctly", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "years", 5, NA, 1, 2
    )
  )

  base_costs <- list(c_annual = 1000)

  result <- calculate_composite_costs(components, base_costs, discount_rate = NULL)

  # 1000 * 5 years = 5000
  expect_equal(result$c_comp, 5000)
})


test_that("calculate_composite_costs converts months duration correctly", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "months", 24, NA, 1, 2
    )
  )

  base_costs <- list(c_annual = 1200)  # Annual cost

  result <- calculate_composite_costs(components, base_costs, discount_rate = NULL)

  # 24 months = 2 years, so 1200 * 2 = 2400
  expect_equal(result$c_comp, 2400)
})


test_that("calculate_composite_costs converts days duration correctly", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "days", 365.25, NA, 1, 2
    )
  )

  base_costs <- list(c_annual = 1000)

  result <- calculate_composite_costs(components, base_costs, discount_rate = NULL)

  # 365.25 days = 1 year, so 1000 * 1 = 1000
  expect_equal(result$c_comp, 1000, tolerance = 0.01)
})


test_that("calculate_composite_costs converts weeks duration correctly", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "weeks", 52.17857, NA, 1, 2
    )
  )

  base_costs <- list(c_annual = 1000)

  result <- calculate_composite_costs(components, base_costs, discount_rate = NULL)

  # 52.17857 weeks = 1 year, so 1000 * 1 = 1000
  expect_equal(result$c_comp, 1000, tolerance = 0.01)
})


test_that("calculate_composite_costs converts hours duration correctly", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "hours", 8766, NA, 1, 2
    )
  )

  base_costs <- list(c_annual = 1000)

  result <- calculate_composite_costs(components, base_costs, discount_rate = NULL)

  # 8766 hours = 1 year, so 1000 * 1 = 1000
  expect_equal(result$c_comp, 1000, tolerance = 0.01)
})


test_that("calculate_composite_costs looks up duration from edge_properties", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "years", NA, NA, 7, 10
    )
  )

  base_costs <- list(c_annual = 5000)

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years,
    7, 10, 3
  )

  result <- calculate_composite_costs(components, base_costs, edge_props)

  # 5000 * 3 years = 15000 (no discounting)
  expect_equal(result$c_comp, 15000)
})


test_that("calculate_composite_costs looks up different duration units from edge_properties", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_treatment", "c_annual", "years", NA, NA, 1, 2,
      "c_treatment", "c_annual", "months", NA, NA, 2, 3,
      "c_treatment", "c_annual", "days", NA, NA, 3, 4
    )
  )

  base_costs <- list(c_annual = 1000)

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years, ~months, ~days,
    1, 2, 2, NA, NA,
    2, 3, NA, 12, NA,
    3, 4, NA, NA, 365.25
  )

  result <- calculate_composite_costs(components, base_costs, edge_props, discount_rate = NULL)

  # 2 years + 12 months (1 year) + 365.25 days (1 year) = 4 years total
  # 1000 * 4 = 4000
  expect_equal(result$c_treatment, 4000, tolerance = 0.01)
})


test_that("calculate_composite_costs looks up quantity from edge_properties", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_injection", NA, NA, NA, 4, 7
    )
  )

  base_costs <- list(c_injection = 30000)

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~quantity,
    4, 7, 2
  )

  result <- calculate_composite_costs(components, base_costs, edge_props)

  # 30000 * 2 = 60000
  expect_equal(result$c_comp, 60000)
})


test_that("calculate_composite_costs applies discounting only to duration multipliers", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_annual", "years", 3, NA, 1, 2
    )
  )

  base_costs <- list(c_annual = 5000)

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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_injection", NA, NA, 3, 1, 2
    )
  )

  base_costs <- list(c_injection = 30000)

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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base1", NA, NA, 2, 1, 2,
      "c_comp", "c_base2", NA, NA, 3, 2, 3,
      "c_comp", "c_base3", NA, NA, 1, 3, 4
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


test_that("calculate_composite_costs handles mixed duration and quantity components", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_treatment", "c_injection", NA, NA, 2, 1, 2,
      "c_treatment", "c_annual", "years", 5, NA, 2, 3
    )
  )

  base_costs <- list(
    c_injection = 30000,
    c_annual = 1000
  )

  result <- calculate_composite_costs(components, base_costs, discount_rate = NULL)

  # 30000 * 2 + 1000 * 5 = 60000 + 5000 = 65000
  expect_equal(result$c_treatment, 65000)
})


test_that("calculate_composite_costs handles multiple cost_labels", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp1", "c_base", NA, NA, 2, 1, 2,
      "c_comp2", "c_base", NA, NA, 3, 2, 3
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_equal(length(result), 2)
  expect_equal(result$c_comp1, 2000)
  expect_equal(result$c_comp2, 3000)
})


test_that("calculate_composite_costs reproduces c_biol_2_success example with new structure", {
  # Reproduce the example with new duration_unit/quantity_value structure
  base_costs <- list(
    c_biologic = 30000,
    c_fail_year = 5000,
    c_success_year = 1000
  )

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years,
    4, 7, 1,    # Placeholder (quantity is specified explicitly)
    7, 10, 1,   # 1 year between 7 and 10
    10, 19, 4   # 4 years between 10 and 19
  )

  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_biol_2_success", "c_biologic", NA, NA, 1, 4, 7,
      "c_biol_2_success", "c_biologic", NA, NA, 1, 7, 10,
      "c_biol_2_success", "c_fail_year", "years", NA, NA, 7, 10,
      "c_biol_2_success", "c_success_year", "years", NA, NA, 10, 19
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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "years", NA, NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Edge properties for different edge
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years,
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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "years", NA, NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Duplicate edge properties
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years,
    1, 2, 1,
    1, 2, 2
  )

  expect_error(
    calculate_composite_costs(components, base_costs, edge_props),
    "Multiple edge properties found for edge 1 -> 2"
  )
})


test_that("calculate_composite_costs errors on NA in duration edge_properties lookup", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", "years", NA, NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Edge property has NA years
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years,
    1, 2, NA
  )

  expect_error(
    calculate_composite_costs(components, base_costs, edge_props),
    "NA value in edge_properties\\$years for edge 1 -> 2"
  )
})


test_that("calculate_composite_costs errors on NA in quantity edge_properties lookup", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base", NA, NA, NA, 1, 2
    )
  )

  base_costs <- list(c_base = 1000)

  # Edge property has NA quantity
  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~quantity,
    1, 2, NA
  )

  expect_error(
    calculate_composite_costs(components, base_costs, edge_props),
    "NA value in edge_properties\\$quantity for edge 1 -> 2"
  )
})


test_that("calculate_composite_costs handles mixed specified and lookup values", {
  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp", "c_base1", NA, NA, 2, 1, 2,              # Quantity: explicit
      "c_comp", "c_base2", "years", NA, NA, 7, 10        # Duration: lookup
    )
  )

  base_costs <- list(
    c_base1 = 30000,
    c_base2 = 1000
  )

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~years,
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
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_comp1", "c_base", NA, NA, 2, 1, 2,
      "c_comp2", "c_base", NA, NA, 3, 2, 3
    )
  )

  base_costs <- list(c_base = 1000)

  result <- calculate_composite_costs(components, base_costs)

  expect_type(result, "list")
  expect_named(result, c("c_comp1", "c_comp2"))
  expect_true(all(sapply(result, is.numeric)))
})


test_that("calculate_composite_costs handles complex realistic scenario with multiple time units", {
  # Scenario: Hospital stay with mixed time units
  base_costs <- list(
    c_icu_bed = 5000,      # per day
    c_regular_bed = 1000,  # per day
    c_medication = 100,    # per day
    c_procedure = 50000    # one-time
  )

  edge_props <- tibble::tribble(
    ~from_node, ~to_node, ~days, ~weeks, ~months,
    1, 2, 7, NA, NA,      # 7 days ICU
    2, 3, 14, NA, NA,     # 14 days regular
    3, 4, NA, 8, NA,      # 8 weeks recovery
    4, 5, NA, NA, 6       # 6 months follow-up
  )

  components <- specify_cost_components(
    tibble::tribble(
      ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
      "c_hospital", "c_icu_bed", "days", NA, NA, 1, 2,
      "c_hospital", "c_regular_bed", "days", NA, NA, 2, 3,
      "c_hospital", "c_medication", "weeks", NA, NA, 3, 4,
      "c_hospital", "c_medication", "months", NA, NA, 4, 5,
      "c_hospital", "c_procedure", NA, NA, 1, 1, 2
    )
  )

  result <- calculate_composite_costs(components, base_costs, edge_props, discount_rate = NULL)

  # Calculate expected:
  # ICU: 5000 * 7 days = 5000 * (7/365.25) years = 5000 * 0.01916... years
  # Regular: 1000 * 14 days
  # Recovery medication: 100 * 8 weeks
  # Follow-up medication: 100 * 6 months
  # Procedure: 50000 * 1

  icu_cost <- 5000 * convert_duration_to_years(7, "days")
  regular_cost <- 1000 * convert_duration_to_years(14, "days")
  recovery_cost <- 100 * convert_duration_to_years(8, "weeks")
  followup_cost <- 100 * convert_duration_to_years(6, "months")
  procedure_cost <- 50000

  expected <- icu_cost + regular_cost + recovery_cost + followup_cost + procedure_cost

  expect_equal(result$c_hospital, expected, tolerance = 0.01)
})
