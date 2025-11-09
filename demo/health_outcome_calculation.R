# Health Outcome Calculation Demo
# ================================
#
# This demo shows how to use the cedar package's health outcome calculation
# functions to compute composite health outcomes (QALYs, life years) for
# decision tree analysis in cost-effectiveness analysis (CEA).
#
# The health outcome calculation system consists of three main functions:
# 1. convert_duration_to_years() - Converts durations from various units to years
# 2. specify_health_outcome_components() - Validates health outcome specifications
# 3. calculate_composite_health_outcomes() - Calculates composite outcomes

library(cedar)
library(tibble)

# ==============================================================================
# Example 1: Basic QALY Calculation from Health Utility
# ==============================================================================
#
# Scenario: A treatment that provides 5 years at 0.85 utility
#
cat("\n=== Example 1: Basic QALY Calculation ===\n\n")

# Define base health outcomes (utility values)
base_outcomes_ex1 <- list(
  utility_treatment = 0.85
)

# Specify how to calculate QALYs
outcome_components_ex1 <- tribble(
  ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
  "qaly_treatment", "utility_treatment", "utility_to_qaly", "years", 2, 4, 5
) |>
  specify_health_outcome_components()

# Calculate composite outcomes
composite_outcomes_ex1 <- calculate_composite_health_outcomes(
  outcome_components_ex1,
  base_outcomes_ex1
)

cat("Treatment QALY calculation:\n")
cat("  Utility: 0.85\n")
cat("  Duration: 5 years\n")
cat("  Result: ", composite_outcomes_ex1$qaly_treatment, " QALYs\n")
cat("  Calculation: 0.85 × 5 = 4.25 QALYs\n\n")

# ==============================================================================
# Example 2: Multiple Duration Units
# ==============================================================================
#
# Scenario: Different treatment phases with different time units
#
cat("=== Example 2: Multiple Duration Units ===\n\n")

base_outcomes_ex2 <- list(
  utility_acute = 0.70,      # Acute phase utility
  utility_maintenance = 0.85  # Maintenance phase utility
)

outcome_components_ex2 <- tribble(
  ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
  "total_qaly", "utility_acute", "utility_to_qaly", "months", 1, 2, 6,
  "total_qaly", "utility_maintenance", "utility_to_qaly", "years", 2, 3, 4
) |>
  specify_health_outcome_components()

composite_outcomes_ex2 <- calculate_composite_health_outcomes(
  outcome_components_ex2,
  base_outcomes_ex2
)

cat("Multi-phase treatment QALY calculation:\n")
cat("  Acute phase (6 months): 0.70 × 0.5 years = 0.35 QALYs\n")
cat("  Maintenance phase (4 years): 0.85 × 4 = 3.40 QALYs\n")
cat("  Total QALYs: ", composite_outcomes_ex2$total_qaly, "\n")
cat("  Calculation: 0.35 + 3.40 = 3.75 QALYs\n\n")

# ==============================================================================
# Example 3: Mixed Outcome Types
# ==============================================================================
#
# Scenario: Combining utility-based QALYs, life years, and pre-calculated QALYs
#
cat("=== Example 3: Mixed Outcome Types ===\n\n")

base_outcomes_ex3 <- list(
  utility_treatment = 0.80,
  ly_multiplier = 1.0,
  literature_qaly = 2.5  # QALY value from published literature
)

outcome_components_ex3 <- tribble(
  ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
  "total_benefit", "utility_treatment", "utility_to_qaly", "years", 1, 2, 3,
  "total_benefit", "ly_multiplier", "life_years", "years", 2, 3, 5,
  "total_benefit", "literature_qaly", "qaly_direct", "years", 3, 4, 1
) |>
  specify_health_outcome_components()

composite_outcomes_ex3 <- calculate_composite_health_outcomes(
  outcome_components_ex3,
  base_outcomes_ex3
)

cat("Mixed outcome type calculation:\n")
cat("  Utility to QALY: 0.80 × 3 years = 2.40 QALYs\n")
cat("  Life years: 1.0 × 5 years = 5.00 years\n")
cat("  Pre-calculated QALY: 2.50 QALYs\n")
cat("  Total benefit: ", composite_outcomes_ex3$total_benefit, "\n")
cat("  Calculation: 2.40 + 5.00 + 2.50 = 9.90\n\n")

# ==============================================================================
# Example 4: Using Edge Properties for Duration Lookup
# ==============================================================================
#
# Scenario: Duration values stored in edge properties table
#
cat("=== Example 4: Duration Lookup from Edge Properties ===\n\n")

# Define edge properties with various duration units
edge_props_ex4 <- tribble(
  ~from_node, ~to_node, ~years, ~months, ~days,
  2, 4, 5, NA, NA,
  2, 5, NA, 6, NA,
  3, 6, NA, NA, 90
)

base_outcomes_ex4 <- list(
  utility_success = 0.85,
  utility_partial = 0.75,
  utility_temporary = 0.70
)

# Specify components with NA duration_value (will lookup from edge_props)
outcome_components_ex4 <- tribble(
  ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to,
  "qaly_success", "utility_success", "utility_to_qaly", "years", 2, 4,
  "qaly_partial", "utility_partial", "utility_to_qaly", "months", 2, 5,
  "qaly_temporary", "utility_temporary", "utility_to_qaly", "days", 3, 6
) |>
  specify_health_outcome_components()

composite_outcomes_ex4 <- calculate_composite_health_outcomes(
  outcome_components_ex4,
  base_outcomes_ex4,
  edge_props_ex4
)

cat("Duration lookup from edge properties:\n")
cat("  Success (5 years): ", composite_outcomes_ex4$qaly_success, " QALYs\n")
cat("  Partial (6 months): ", composite_outcomes_ex4$qaly_partial, " QALYs\n")
cat("  Temporary (90 days): ", composite_outcomes_ex4$qaly_temporary, " QALYs\n\n")

# ==============================================================================
# Example 5: Realistic Clinical Scenario
# ==============================================================================
#
# Scenario: Biologic therapy for rheumatoid arthritis with multiple outcomes
#
cat("=== Example 5: Realistic Clinical Scenario ===\n\n")
cat("Biologic therapy for rheumatoid arthritis\n")
cat("------------------------------------------\n\n")

# Define base outcomes
base_outcomes_ex5 <- list(
  utility_loading = 0.65,      # Loading dose period
  utility_response = 0.80,     # After successful response
  utility_maintenance = 0.85,  # Long-term maintenance
  utility_failure = 0.60,      # Treatment failure
  precalc_adverse_qaly = -0.1  # QALY loss from adverse events
)

# Define edge properties
edge_props_ex5 <- tribble(
  ~from_node, ~to_node, ~months, ~years,
  1, 2, 3, NA,        # Loading: 3 months
  2, 3, NA, 2,        # Response phase: 2 years
  3, 4, NA, 5,        # Maintenance: 5 years
  2, 5, NA, 1         # Failure: 1 year
)

# Specify outcome components
outcome_components_ex5 <- tribble(
  ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to,
  "qaly_success", "utility_loading", "utility_to_qaly", "months", 1, 2,
  "qaly_success", "utility_response", "utility_to_qaly", "years", 2, 3,
  "qaly_success", "utility_maintenance", "utility_to_qaly", "years", 3, 4,
  "qaly_success", "precalc_adverse_qaly", "qaly_direct", "years", 1, 2,
  "qaly_failure", "utility_loading", "utility_to_qaly", "months", 1, 2,
  "qaly_failure", "utility_failure", "utility_to_qaly", "years", 2, 5
) |>
  specify_health_outcome_components()

# Calculate composite outcomes
composite_outcomes_ex5 <- calculate_composite_health_outcomes(
  outcome_components_ex5,
  base_outcomes_ex5,
  edge_props_ex5
)

cat("Success pathway:\n")
cat("  Loading (3 months): 0.65 × 0.25 = 0.1625 QALYs\n")
cat("  Response (2 years): 0.80 × 2 = 1.60 QALYs\n")
cat("  Maintenance (5 years): 0.85 × 5 = 4.25 QALYs\n")
cat("  Adverse events: -0.10 QALYs\n")
cat("  Total Success QALYs: ", composite_outcomes_ex5$qaly_success, "\n\n")

cat("Failure pathway:\n")
cat("  Loading (3 months): 0.65 × 0.25 = 0.1625 QALYs\n")
cat("  Failure (1 year): 0.60 × 1 = 0.60 QALYs\n")
cat("  Total Failure QALYs: ", composite_outcomes_ex5$qaly_failure, "\n\n")

# ==============================================================================
# Example 6: Integration with Decision Tree
# ==============================================================================
#
# Scenario: Complete decision tree with health outcomes
#
cat("=== Example 6: Integration with Decision Tree ===\n\n")

# Define tree structure
tree_ex6 <- list(
  "1" = c(2, 3),     # Decision: Treatment A or B
  "2" = c(4, 5),     # Treatment A: Success or failure
  "3" = c(6, 7)      # Treatment B: Success or failure
)

# Define probabilities
probs_ex6 <- list(
  p_a_success = 0.70,
  p_a_failure = 0.30,
  p_b_success = 0.60,
  p_b_failure = 0.40
)

prob_assoc_ex6 <- tribble(
  ~name, ~from_node, ~to_node,
  "p_a_success", 2, 4,
  "p_a_failure", 2, 5,
  "p_b_success", 3, 6,
  "p_b_failure", 3, 7
)

# Define costs (keeping it simple for this demo)
costs_ex6 <- list(
  c_treatment_a = 50000,
  c_treatment_b = 45000
)

cost_assoc_ex6 <- tribble(
  ~name, ~from_node, ~to_node,
  "c_treatment_a", 2, 4,
  "c_treatment_a", 2, 5,
  "c_treatment_b", 3, 6,
  "c_treatment_b", 3, 7
)

# Define base health outcomes
base_outcomes_ex6 <- list(
  utility_a_success = 0.85,
  utility_a_failure = 0.65,
  utility_b_success = 0.80,
  utility_b_failure = 0.60
)

# Calculate composite health outcomes
outcome_components_ex6 <- tribble(
  ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
  "qaly_a_success", "utility_a_success", "utility_to_qaly", "years", 2, 4, 5,
  "qaly_a_failure", "utility_a_failure", "utility_to_qaly", "years", 2, 5, 5,
  "qaly_b_success", "utility_b_success", "utility_to_qaly", "years", 3, 6, 5,
  "qaly_b_failure", "utility_b_failure", "utility_to_qaly", "years", 3, 7, 5
) |>
  specify_health_outcome_components()

composite_outcomes_ex6 <- calculate_composite_health_outcomes(
  outcome_components_ex6,
  base_outcomes_ex6
)

# Create health outcome associations for the tree
outcome_assoc_ex6 <- tribble(
  ~name, ~from_node, ~to_node,
  "qaly_a_success", 2, 4,
  "qaly_a_failure", 2, 5,
  "qaly_b_success", 3, 6,
  "qaly_b_failure", 3, 7
)

# Build the complete decision tree table
tree_table_ex6 <- build_decision_tree_table(
  tree_ex6,
  costs_ex6,
  probs_ex6,
  composite_outcomes_ex6,
  cost_assoc_ex6,
  prob_assoc_ex6,
  outcome_assoc_ex6
)

cat("Complete decision tree with health outcomes:\n")
print(tree_table_ex6)
cat("\n")

# Calculate expected QALYs for each treatment
cat("Expected QALYs:\n")
cat("  Treatment A: (0.70 × 4.25) + (0.30 × 3.25) = 3.95 QALYs\n")
cat("  Treatment B: (0.60 × 4.00) + (0.40 × 3.00) = 3.60 QALYs\n\n")

# ==============================================================================
# Key Concepts Summary
# ==============================================================================
#
cat("=== Key Concepts ===\n\n")

cat("1. Outcome Types:\n")
cat("   - utility_to_qaly: Converts health utility (0-1) to QALYs by multiplying\n")
cat("     by duration in years\n")
cat("   - life_years: Multiplies base value by duration in years\n")
cat("   - qaly_direct: Uses pre-calculated QALY value directly (no duration used)\n\n")

cat("2. Duration Units:\n")
cat("   - Supported units: hours, days, weeks, months, years\n")
cat("   - All durations converted to years using 365.25 days/year\n")
cat("   - Can mix different units in the same calculation\n\n")

cat("3. Duration Specification:\n")
cat("   - Explicit: Set duration_value in outcome_components\n")
cat("   - Edge lookup: Leave duration_value as NA and provide edge_properties\n")
cat("     with columns matching duration_unit names\n\n")

cat("4. Best Practices:\n")
cat("   - Always use specify_health_outcome_components() to validate specifications\n")
cat("   - For utility values, ensure they're between 0 and 1\n")
cat("   - Use consistent time horizons across comparators\n")
cat("   - Document the source of utility values and QALYs\n")
cat("   - Consider half-cycle correction for discrete time models\n\n")