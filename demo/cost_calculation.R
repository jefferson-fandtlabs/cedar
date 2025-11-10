# Cost Calculation Example: Composite Costs with Multiple Time Units
# This example demonstrates how to calculate composite costs for CEA decision trees
# using the cost components framework

# Load the cedar package
# If running from the package directory, use: devtools::load_all()
# If the package is installed, use: library(cedar)
library(cedar)
library(tibble)
library(dplyr)

# =============================================================================
# SCENARIO: Treatment with Biologic Therapy
# =============================================================================

cat("===== COST CALCULATION EXAMPLE =====\n\n")

cat("Scenario: Calculate total cost for biologic treatment pathway\n")
cat("The patient receives:\n")
cat("  - 2 biologic injections (at different time points)\n")
cat("  - 1 year of failed treatment medical care\n")
cat("  - 4 years of successful treatment medical care\n\n")


# =============================================================================
# Step 1: Define Primitive Base Costs
# =============================================================================

cat("Step 1: Define primitive base costs\n")
cat("--------------------------------------\n")

base_costs <- list(
  c_biologic = 30000,       # Cost per biologic injection
  c_fail_year = 5000,       # Annual medical cost for failed treatment
  c_success_year = 1000     # Annual medical cost for successful treatment
)

cat("Base costs:\n")
cat("  Biologic injection:   $30,000\n")
cat("  Failed treatment:     $5,000/year\n")
cat("  Successful treatment: $1,000/year\n\n")


# =============================================================================
# Step 2: Define Edge Properties
# =============================================================================

cat("Step 2: Define edge properties for the decision tree\n")
cat("------------------------------------------------------\n")

# Edge properties map to transitions in your decision tree
# They capture information like duration or quantity for each edge
# NOTE: Multiple time unit columns (years, months, etc.) can coexist

edge_properties <- tribble(
  ~from_node, ~to_node, ~years, ~quantity,
  4, 7, NA, NA,             # Will use explicit quantity in cost_components
  7, 10, 1, 1,              # 1 year + 1 injection
  10, 19, 4, NA             # 4 years of successful treatment (duration-based)
)

cat("Edge properties:\n")
print(edge_properties)
cat("\nExplanation:\n")
cat("  Edge 4->7:  First injection (quantity specified in cost_components)\n")
cat("  Edge 7->10: 1 year of failed treatment + second injection\n")
cat("  Edge 10->19: 4 years of successful treatment\n\n")


# =============================================================================
# Step 3: Specify Cost Components
# =============================================================================

cat("Step 3: Specify cost components\n")
cat("----------------------------------\n")

# Define how base costs combine to create the composite cost
# Each row represents one component of the total cost

cost_components <- tribble(
  ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
  "c_biol_2_success", "c_biologic", NA, NA, 1, 4, 7,             # 1st injection (explicit quantity)
  "c_biol_2_success", "c_biologic", NA, NA, NA, 7, 10,           # 2nd injection (lookup quantity from edge)
  "c_biol_2_success", "c_fail_year", "years", NA, NA, 7, 10,     # Failed treatment (lookup years)
  "c_biol_2_success", "c_success_year", "years", NA, NA, 10, 19  # Successful treatment (lookup years)
) %>%
  specify_cost_components()

cat("Cost components for 'c_biol_2_success':\n")
print(cost_components)
cat("\nKey points:\n")
cat("  - duration_unit: Time unit for duration-based costs (hours/days/weeks/months/years)\n")
cat("  - duration_value: Specific duration OR NA to lookup from edge_properties\n")
cat("  - quantity_value: Specific quantity OR NA to lookup from edge_properties$quantity\n")
cat("  - Each row must have EITHER duration OR quantity, never both\n")
cat("  - All components with same cost_label are summed together\n\n")


# =============================================================================
# Step 4: Calculate Composite Costs
# =============================================================================

cat("Step 4: Calculate composite costs\n")
cat("------------------------------------\n")

# Set discount rate (3% per year)
discount_rate <- 0.03

# Calculate the composite cost
# The function will:
#   1. Look up duration/quantity from edge_properties (when value is NA)
#   2. Convert durations to years using the duration_unit
#   3. Apply discounting to duration-based costs (if discount_rate provided)
#   4. Sum all components for each cost_label

costs_calculated <- calculate_composite_costs(
  cost_components = cost_components,
  base_costs = base_costs,
  edge_properties = edge_properties,
  discount_rate = discount_rate
)

cat("Calculated costs:\n")
cat(sprintf("  c_biol_2_success = $%.2f\n\n", costs_calculated$c_biol_2_success))


# =============================================================================
# COST BREAKDOWN
# =============================================================================

cat("===== DETAILED COST BREAKDOWN =====\n\n")

cat("Component 1: First biologic injection (edge 4->7)\n")
cat("  Base cost: $30,000\n")
cat("  Quantity: 1 (explicit in cost_components)\n")
cat("  Discounting: None (quantities are not discounted)\n")
cat("  Component cost: $30,000.00\n\n")

cat("Component 2: Second biologic injection (edge 7->10)\n")
cat("  Base cost: $30,000\n")
cat("  Quantity: 1 (from edge_properties$quantity)\n")
cat("  Discounting: None (quantities are not discounted)\n")
cat("  Component cost: $30,000.00\n\n")

cat("Component 3: Failed treatment (edge 7->10)\n")
cat("  Base cost: $5,000/year\n")
cat("  Duration: 1 year (from edge_properties$years)\n")
cat("  Discount rate: 3%\n")
cat("  Time offset: 0 years (first occurrence)\n")
component_3 <- apply_discount(5000, 1, 0.03, time_offset = 0)
cat(sprintf("  Component cost: $%.2f\n", component_3))
cat("  Formula: 5000 * discount_factor(1 year at 3%)\n\n")

cat("Component 4: Successful treatment (edge 10->19)\n")
cat("  Base cost: $1,000/year\n")
cat("  Duration: 4 years (from edge_properties$years)\n")
cat("  Discount rate: 3%\n")
cat("  Time offset: 1 year (starts after failed treatment)\n")
component_4 <- apply_discount(1000, 4, 0.03, time_offset = 1)
cat(sprintf("  Component cost: $%.2f\n", component_4))
cat("  Formula: sum(1000 / (1.03)^t) for t=1 to 4, starting at year 1\n\n")

cat(sprintf("TOTAL: $30,000 + $30,000 + $%.2f + $%.2f = $%.2f\n\n",
            component_3, component_4,
            30000 + 30000 + component_3 + component_4))


# =============================================================================
# EXAMPLE WITH MULTIPLE TIME UNITS
# =============================================================================

cat("===== EXAMPLE: MULTIPLE TIME UNITS =====\n\n")

# Edge properties with different time unit columns
edge_props_multi <- tribble(
  ~from_node, ~to_node, ~days, ~weeks, ~months, ~years,
  1, 2, 7, NA, NA, NA,          # 7 days
  2, 3, NA, 4, NA, NA,          # 4 weeks
  3, 4, NA, NA, 6, NA,          # 6 months
  4, 5, NA, NA, NA, 2           # 2 years
)

# Cost components using different time units
cost_comps_multi <- tribble(
  ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
  "c_complex", "c_daily", "days", NA, NA, 1, 2,        # Lookup days
  "c_complex", "c_weekly", "weeks", NA, NA, 2, 3,      # Lookup weeks
  "c_complex", "c_monthly", "months", NA, NA, 3, 4,    # Lookup months
  "c_complex", "c_annual", "years", NA, NA, 4, 5       # Lookup years
) %>%
  specify_cost_components()

base_costs_multi <- list(
  c_daily = 100,
  c_weekly = 500,
  c_monthly = 2000,
  c_annual = 10000
)

costs_multi <- calculate_composite_costs(
  cost_comps_multi,
  base_costs_multi,
  edge_props_multi,
  discount_rate = 0.03
)

cat("Cost components using multiple time units:\n")
print(cost_comps_multi)
cat("\n")

cat(sprintf("Total cost with mixed time units: $%.2f\n\n", costs_multi$c_complex))

cat("Calculation breakdown:\n")
cat("  7 days = 7/365.25 years\n")
cat("  4 weeks = 4/52.18 years\n")
cat("  6 months = 6/12 years = 0.5 years\n")
cat("  2 years = 2 years\n")
cat("All durations converted to years, then discounted and summed\n\n")


# =============================================================================
# EXAMPLE: ONE-TIME COSTS
# =============================================================================

cat("===== EXAMPLE: ONE-TIME COSTS =====\n\n")

# One-time costs use quantity_value = 1
one_time_comps <- tribble(
  ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
  "c_surgery", "c_surgery_base", NA, NA, 1, 3, 4,           # Surgery
  "c_surgery", "c_anesthesia", NA, NA, 1, 3, 4              # Anesthesia
) %>%
  specify_cost_components()

one_time_costs <- calculate_composite_costs(
  one_time_comps,
  list(c_surgery_base = 50000, c_anesthesia = 5000),
  edge_properties = NULL
)

cat("One-time cost calculation:\n")
cat(sprintf("  c_surgery = $%.2f\n", one_time_costs$c_surgery))
cat("  (Surgery $50,000 + Anesthesia $5,000 = $55,000)\n")
cat("  Uses quantity_value = 1 for each component\n\n")


# =============================================================================
# EXAMPLE: WITHOUT DISCOUNTING
# =============================================================================

cat("===== EXAMPLE: DURATION WITHOUT DISCOUNTING =====\n\n")

nodiscount_comps <- tribble(
  ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
  "c_undiscounted", "c_annual", "years", 3, NA, 1, 2
) %>%
  specify_cost_components()

nodiscount_costs <- calculate_composite_costs(
  nodiscount_comps,
  list(c_annual = 1000),
  edge_properties = NULL,
  discount_rate = NULL  # No discounting
)

cat(sprintf("c_undiscounted = $%.2f\n", nodiscount_costs$c_undiscounted))
cat("(1000 * 3 years = 3000, no discounting applied)\n\n")


# =============================================================================
# EXAMPLE: MIXED DURATION AND QUANTITY
# =============================================================================

cat("===== EXAMPLE: MIXED DURATION AND QUANTITY =====\n\n")

edge_props_mixed <- tribble(
  ~from_node, ~to_node, ~months, ~quantity,
  1, 2, 6, 2,               # 6 months duration, 2 quantity items
  2, 3, 12, 1               # 12 months duration, 1 quantity item
)

mixed_comps <- tribble(
  ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
  "c_treatment", "c_injection", NA, NA, NA, 1, 2,           # 2 injections (lookup quantity)
  "c_treatment", "c_monthly_care", "months", NA, NA, 1, 2,  # 6 months care (lookup months)
  "c_treatment", "c_injection", NA, NA, NA, 2, 3,           # 1 injection (lookup quantity)
  "c_treatment", "c_monthly_care", "months", NA, NA, 2, 3   # 12 months care (lookup months)
) %>%
  specify_cost_components()

mixed_costs <- calculate_composite_costs(
  mixed_comps,
  list(c_injection = 5000, c_monthly_care = 500),
  edge_props_mixed,
  discount_rate = 0.03
)

cat("Mixed duration and quantity components:\n")
print(mixed_comps)
cat("\n")

cat(sprintf("Total cost: $%.2f\n\n", mixed_costs$c_treatment))

cat("Breakdown:\n")
cat("  2 injections @ $5,000 = $10,000 (not discounted)\n")
cat("  6 months care @ $500/month = $3,000 * discount_factor (discounted)\n")
cat("  1 injection @ $5,000 = $5,000 (not discounted)\n")
cat("  12 months care @ $500/month = $6,000 * discount_factor (discounted)\n\n")


# =============================================================================
# INTEGRATION WITH build_decision_tree_table()
# =============================================================================

cat("===== USING WITH DECISION TREE TABLE =====\n\n")

# The calculated costs and edge properties can be used directly with
# build_decision_tree_table()

cat("The costs you calculated can be used directly:\n\n")
cat("tree_table <- build_decision_tree_table(\n")
cat("  tree_structure = tree_structure,\n")
cat("  costs = costs_calculated,  # <-- Your calculated costs\n")
cat("  probabilities = probabilities,\n")
cat("  health_outcomes = health_outcomes,\n")
cat("  cost_associations = cost_associations,\n")
cat("  probability_associations = probability_associations,\n")
cat("  health_outcome_associations = health_outcome_associations,\n")
cat("  edge_properties = edge_properties  # <-- Same edge properties\n")
cat(")\n\n")

cat("The edge_properties will be merged into the tree table,\n")
cat("making duration and other properties visible alongside costs.\n\n")

cat("IMPORTANT: The same edge_properties tibble can be used for both\n")
cat("cost calculations and health outcome calculations, as long as it\n")
cat("contains all the required columns (years, months, days, quantity, etc.)\n\n")


# =============================================================================
# KEY CONCEPTS
# =============================================================================

cat("===== KEY CONCEPTS =====\n\n")

cat("1. DURATION-BASED COSTS:\n")
cat("   - Set duration_unit to: hours, days, weeks, months, or years\n")
cat("   - Set duration_value to a number OR NA (to lookup from edge_properties)\n")
cat("   - Set quantity_value to NA\n")
cat("   - Durations are converted to years using convert_duration_to_years()\n")
cat("   - Discounting is applied if discount_rate is provided\n\n")

cat("2. QUANTITY-BASED COSTS:\n")
cat("   - Set quantity_value to a number OR NA (to lookup from edge_properties$quantity)\n")
cat("   - Set duration_unit and duration_value to NA\n")
cat("   - Used for countable items: injections, surgeries, procedures\n")
cat("   - No time conversion or discounting is applied\n")
cat("   - One-time costs use quantity_value = 1\n\n")

cat("3. VALIDATION:\n")
cat("   - Each row must have EITHER duration OR quantity, never both\n")
cat("   - specify_cost_components() validates this constraint\n")
cat("   - Error if both are specified or neither is specified\n\n")

cat("4. EDGE PROPERTY LOOKUP:\n")
cat("   - When duration_value is NA: lookup from edge_properties using duration_unit\n")
cat("     as the column name (e.g., edge_properties$months)\n")
cat("   - When quantity_value is NA: lookup from edge_properties$quantity\n")
cat("   - Edge match uses from_node and to_node\n\n")

cat("5. TIME UNIT CONVERSION:\n")
cat("   - All duration units converted to years before discounting:\n")
cat("     * hours: value / 8766\n")
cat("     * days: value / 365.25\n")
cat("     * weeks: value / 52.18\n")
cat("     * months: value / 12\n")
cat("     * years: value (no conversion)\n\n")

cat("6. DISCOUNTING:\n")
cat("   - Optional: Set discount_rate = NULL for no discounting\n")
cat("   - Only applied to duration-based costs, never to quantity-based costs\n")
cat("   - Uses time offsets computed from decision tree structure\n")
cat("   - Formula: apply_discount(base_cost, duration_years, rate, offset)\n\n")

cat("7. UNIFIED STRUCTURE:\n")
cat("   - Cost components work identically to health outcome components\n")
cat("   - Both use duration_unit for time-based calculations\n")
cat("   - Both support edge property lookup with NA values\n")
cat("   - Same edge_properties tibble can serve both systems\n\n")


# =============================================================================
# TIPS AND BEST PRACTICES
# =============================================================================

cat("===== TIPS AND BEST PRACTICES =====\n\n")

cat("1. Use meaningful cost labels that describe the pathway:\n")
cat("   Good: 'c_biol_2_success', 'c_surgery_with_rehab'\n")
cat("   Bad: 'c1', 'cost_temp'\n\n")

cat("2. Keep base costs primitive and reusable:\n")
cat("   - Store individual cost items (c_injection, c_annual_care)\n")
cat("   - Build composites using components\n")
cat("   - Same base cost can be used in multiple composite calculations\n\n")

cat("3. Document your edge properties:\n")
cat("   - Add comments explaining what each edge represents\n")
cat("   - Include units in column names or comments (years, months, quantity)\n")
cat("   - Use consistent time units across similar edges\n\n")

cat("4. Test without discounting first:\n")
cat("   - Set discount_rate = NULL to verify basic calculations\n")
cat("   - Add discounting once structure is correct\n")
cat("   - Check that quantity-based costs are never discounted\n\n")

cat("5. Use print() on cost_components to verify:\n")
cat("   - Shows summary of components per cost_label\n")
cat("   - Displays duration vs quantity component counts\n")
cat("   - Lists duration units used\n")
cat("   - Helps catch missing or duplicate components\n\n")

cat("6. Validate edge property lookups:\n")
cat("   - Ensure edge_properties has required columns\n")
cat("   - Match duration_unit values to column names\n")
cat("   - Include 'quantity' column if using NA quantity_value\n")
cat("   - Check for duplicate edges (causes errors)\n\n")

cat("7. Match structure with health outcomes:\n")
cat("   - Use same approach for costs and health outcomes\n")
cat("   - Share edge_properties tibble between both\n")
cat("   - Consistent duration_unit usage makes code clearer\n\n")


cat("===== END OF EXAMPLE =====\n\n")

cat("For more information:\n")
cat("  ?specify_cost_components\n")
cat("  ?calculate_composite_costs\n")
cat("  ?apply_discount\n")
cat("  ?convert_duration_to_years\n")
cat("  ?build_decision_tree_table\n")
