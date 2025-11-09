# Cost Calculation Example: Composite Costs with Edge Properties
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
cat("  Biologic injection:  $30,000\n")
cat("  Failed treatment:    $5,000/year\n")
cat("  Successful treatment: $1,000/year\n\n")


# =============================================================================
# Step 2: Define Edge Properties
# =============================================================================

cat("Step 2: Define edge properties for the decision tree\n")
cat("------------------------------------------------------\n")

# Edge properties map to transitions in your decision tree
# They capture information like duration or quantity for each edge

edge_properties <- tribble(
  ~from_node, ~to_node, ~duration,
  4, 7, NA,              # First injection (no duration)
  7, 10, 1,              # 1 year between nodes 7 and 10
  10, 19, 4              # 4 years between nodes 10 and 19
)

cat("Edge properties:\n")
print(edge_properties)
cat("\nExplanation:\n")
cat("  Edge 4->7:  Biologic injection given (quantity multiplier)\n")
cat("  Edge 7->10: 1 year of failed treatment (duration multiplier)\n")
cat("  Edge 10->19: 4 years of successful treatment (duration multiplier)\n\n")


# =============================================================================
# Step 3: Specify Cost Components
# =============================================================================

cat("Step 3: Specify cost components\n")
cat("----------------------------------\n")

# Define how base costs combine to create the composite cost
# Each row represents one component of the total cost

cost_components <- tribble(
  ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
  "c_biol_2_success", "c_biologic", "quantity", 1, 4, 7,        # 1st injection
  "c_biol_2_success", "c_biologic", "quantity", 1, 7, 10,       # 2nd injection
  "c_biol_2_success", "c_fail_year", "duration", NA, 7, 10,     # Failed treatment (1 year)
  "c_biol_2_success", "c_success_year", "duration", NA, 10, 19  # Successful treatment (4 years)
) %>%
  specify_cost_components()

cat("Cost components for 'c_biol_2_success':\n")
print(cost_components)
cat("\nKey points:\n")
cat("  - multiplier_type: 'quantity' = count items, 'duration' = time periods\n")
cat("  - multiplier_value: Specific value OR NA to lookup from edge_properties\n")
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
#   1. Look up multipliers from edge_properties (when multiplier_value is NA)
#   2. Apply discounting to duration-based costs (if discount_rate provided)
#   3. Sum all components for each cost_label

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
cat("  Multiplier: 1 (quantity)\n")
cat("  Discounting: None (quantities are not discounted)\n")
cat("  Component cost: $30,000.00\n\n")

cat("Component 2: Second biologic injection (edge 7->10)\n")
cat("  Base cost: $30,000\n")
cat("  Multiplier: 1 (quantity)\n")
cat("  Discounting: None (quantities are not discounted)\n")
cat("  Component cost: $30,000.00\n\n")

cat("Component 3: Failed treatment (edge 7->10)\n")
cat("  Base cost: $5,000/year\n")
cat("  Duration: 1 year (from edge_properties)\n")
cat("  Discount rate: 3%\n")
component_3 <- apply_discount(5000, 1, 0.03)
cat(sprintf("  Component cost: $%.2f\n", component_3))
cat("  Formula: 5000 / (1.03)^1\n\n")

cat("Component 4: Successful treatment (edge 10->19)\n")
cat("  Base cost: $1,000/year\n")
cat("  Duration: 4 years (from edge_properties)\n")
cat("  Discount rate: 3%\n")
component_4 <- apply_discount(1000, 4, 0.03)
cat(sprintf("  Component cost: $%.2f\n", component_4))
cat("  Formula: sum(1000 / (1.03)^t) for t=1 to 4\n\n")

cat(sprintf("TOTAL: $30,000 + $30,000 + $%.2f + $%.2f = $%.2f\n\n",
            component_3, component_4,
            30000 + 30000 + component_3 + component_4))


# =============================================================================
# ADDITIONAL EXAMPLES
# =============================================================================

cat("===== ADDITIONAL EXAMPLES =====\n\n")

# Example 1: Simple one-time cost (no multiplier)
cat("Example 1: Simple one-time cost\n")
cat("----------------------------------\n")

simple_components <- tribble(
  ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
  "c_surgery", "c_surgery_base", "none", NA, 3, 15
) %>%
  specify_cost_components()

simple_costs <- calculate_composite_costs(
  simple_components,
  list(c_surgery_base = 50000),
  edge_properties = NULL
)

cat(sprintf("c_surgery = $%.2f\n", simple_costs$c_surgery))
cat("(multiplier_type = 'none' means multiply by 1)\n\n")


# Example 2: Without discounting
cat("Example 2: Duration without discounting\n")
cat("-----------------------------------------\n")

nodiscount_components <- tribble(
  ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
  "c_undiscounted", "c_annual", "duration", 3, 1, 2
) %>%
  specify_cost_components()

nodiscount_costs <- calculate_composite_costs(
  nodiscount_components,
  list(c_annual = 1000),
  edge_properties = NULL,
  discount_rate = NULL  # No discounting
)

cat(sprintf("c_undiscounted = $%.2f\n", nodiscount_costs$c_undiscounted))
cat("(cost * time_periods = 1000 * 3 = 3000, no discounting)\n\n")


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


# =============================================================================
# KEY CONCEPTS
# =============================================================================

cat("===== KEY CONCEPTS =====\n\n")

cat("1. MULTIPLIER TYPES:\n")
cat("   - 'duration': Time-based (years, months). Gets discounted if discount_rate provided.\n")
cat("   - 'quantity': Count-based (injections, surgeries). Never discounted.\n")
cat("   - 'none': No multiplier (value = 1). Use for one-time costs.\n\n")

cat("2. MULTIPLIER VALUES:\n")
cat("   - Specific number: Use that value directly\n")
cat("   - NA: Look up from edge_properties using multiplier_type as column name\n\n")

cat("3. EDGE PROPERTIES:\n")
cat("   - Must have 'from_node' and 'to_node' columns\n")
cat("   - Additional columns (duration, quantity, etc.) are custom\n")
cat("   - Can be reused for health outcomes or other calculations\n\n")

cat("4. DISCOUNTING:\n")
cat("   - Optional: Set discount_rate = NULL for no discounting\n")
cat("   - Only applied to multiplier_type = 'duration'\n")
cat("   - Formula: sum(cost / (1 + rate)^t) for t=1 to duration\n\n")


# =============================================================================
# TIPS AND BEST PRACTICES
# =============================================================================

cat("===== TIPS AND BEST PRACTICES =====\n\n")

cat("1. Use meaningful cost labels that describe the pathway:\n")
cat("   Good: 'c_biol_2_success', 'c_surgery_with_rehab'\n")
cat("   Bad: 'c1', 'cost_temp'\n\n")

cat("2. Keep base costs primitive and reusable:\n")
cat("   - Store individual cost items (c_injection, c_annual_care)\n")
cat("   - Build composites using components\n\n")

cat("3. Document your edge properties:\n")
cat("   - Add comments explaining what each edge represents\n")
cat("   - Include units (years, months, count)\n\n")

cat("4. Test without discounting first:\n")
cat("   - Set discount_rate = NULL to verify basic calculations\n")
cat("   - Add discounting once structure is correct\n\n")

cat("5. Use print() on cost_components to verify:\n")
cat("   - Shows summary of components per cost_label\n")
cat("   - Helps catch missing or duplicate components\n\n")


cat("===== END OF EXAMPLE =====\n\n")

cat("For more information:\n")
cat("  ?specify_cost_components\n")
cat("  ?calculate_composite_costs\n")
cat("  ?apply_discount\n")
cat("  ?build_decision_tree_table\n")