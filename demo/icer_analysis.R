# ICER Analysis Example: Incremental Cost-Effectiveness Ratio Calculation
# This example demonstrates how to perform cost-effectiveness analysis on
# decision tree results to calculate ICERs and identify dominated/dominant scenarios

# Load the cedar package
# If running from the package directory, use: devtools::load_all()
# If the package is installed, use: library(cedar)
library(cedar)
library(tibble)
library(dplyr)

# =============================================================================
# SCENARIO: Comparing Three Treatment Strategies
# =============================================================================

cat("===== ICER ANALYSIS EXAMPLE =====\n\n")

cat("Scenario: Compare three treatment strategies for a chronic condition\n")
cat("  - Standard Care: Lower cost, moderate effectiveness\n")
cat("  - Moderate Intervention: Medium cost, better effectiveness\n")
cat("  - Intensive Intervention: Higher cost, best effectiveness\n\n")


# =============================================================================
# Step 1: Define Decision Tree Structure
# =============================================================================

cat("Step 1: Define the decision tree structure\n")
cat("---------------------------------------------\n")

# Node 1 is the decision node with 3 treatment options
# Each treatment has two possible outcomes: success or failure
tree_structure <- list(
  "1" = c(2, 3, 4),      # Decision: Standard, Moderate, or Intensive
  "2" = c(5, 6),         # Standard Care outcomes
  "3" = c(7, 8),         # Moderate Intervention outcomes
  "4" = c(9, 10)         # Intensive Intervention outcomes
)

cat("Tree structure:\n")
cat("  Node 1 -> Nodes 2, 3, 4 (Treatment choices)\n")
cat("  Node 2 -> Nodes 5, 6 (Standard Care: Success/Failure)\n")
cat("  Node 3 -> Nodes 7, 8 (Moderate: Success/Failure)\n")
cat("  Node 4 -> Nodes 9, 10 (Intensive: Success/Failure)\n\n")


# =============================================================================
# Step 2: Define Costs, Probabilities, and Health Outcomes
# =============================================================================

cat("Step 2: Define values for terminal nodes\n")
cat("-------------------------------------------\n")

# Costs for each terminal outcome
costs <- list(
  "c_standard_success" = 10000,
  "c_standard_failure" = 15000,
  "c_moderate_success" = 18000,
  "c_moderate_failure" = 22000,
  "c_intensive_success" = 25000,
  "c_intensive_failure" = 30000
)

# Probabilities of success for each treatment
probabilities <- list(
  "p_standard_success" = 0.60,
  "p_standard_failure" = 0.40,
  "p_moderate_success" = 0.75,
  "p_moderate_failure" = 0.25,
  "p_intensive_success" = 0.85,
  "p_intensive_failure" = 0.15
)

# Health outcomes (QALYs) for each terminal outcome
health_outcomes <- list(
  "qaly_standard_success" = 8.0,
  "qaly_standard_failure" = 5.0,
  "qaly_moderate_success" = 9.0,
  "qaly_moderate_failure" = 6.0,
  "qaly_intensive_success" = 10.0,
  "qaly_intensive_failure" = 7.0
)

cat("Costs defined for 6 terminal outcomes\n")
cat("Probabilities: Standard 60%, Moderate 75%, Intensive 85% success\n")
cat("QALYs range from 5.0 (standard failure) to 10.0 (intensive success)\n\n")


# =============================================================================
# Step 3: Create Association Tables
# =============================================================================

cat("Step 3: Create association tables\n")
cat("------------------------------------\n")

# Associate costs with terminal nodes
cost_associations <- tribble(
  ~name, ~from_node, ~to_node,
  "c_standard_success", 2, 5,
  "c_standard_failure", 2, 6,
  "c_moderate_success", 3, 7,
  "c_moderate_failure", 3, 8,
  "c_intensive_success", 4, 9,
  "c_intensive_failure", 4, 10
)

# Associate probabilities with transitions
probability_associations <- tribble(
  ~name, ~from_node, ~to_node,
  "p_standard_success", 2, 5,
  "p_standard_failure", 2, 6,
  "p_moderate_success", 3, 7,
  "p_moderate_failure", 3, 8,
  "p_intensive_success", 4, 9,
  "p_intensive_failure", 4, 10
)

# Associate health outcomes with terminal nodes
health_outcome_associations <- tribble(
  ~name, ~from_node, ~to_node,
  "qaly_standard_success", 2, 5,
  "qaly_standard_failure", 2, 6,
  "qaly_moderate_success", 3, 7,
  "qaly_moderate_failure", 3, 8,
  "qaly_intensive_success", 4, 9,
  "qaly_intensive_failure", 4, 10
)

cat("Association tables created linking costs, probabilities, and QALYs\n")
cat("to their respective terminal nodes in the decision tree\n\n")


# =============================================================================
# Step 4: Build Decision Tree Table
# =============================================================================

cat("Step 4: Build the decision tree table\n")
cat("----------------------------------------\n")

decision_tree <- build_decision_tree_table(
  tree_structure = tree_structure,
  costs = costs,
  probabilities = probabilities,
  health_outcomes = health_outcomes,
  cost_associations = cost_associations,
  probability_associations = probability_associations,
  health_outcome_associations = health_outcome_associations
)

cat("Decision tree table built successfully\n")
cat("Total edges: ", nrow(decision_tree), "\n\n")


# =============================================================================
# Step 5: Calculate ICER Table (Base Mode)
# =============================================================================

cat("Step 5: Calculate ICER table using base mode\n")
cat("-----------------------------------------------\n")

# Define custom scenario labels
scenario_labels <- list(
  "2" = "Standard Care",
  "3" = "Moderate Intervention",
  "4" = "Intensive Intervention"
)

# Calculate ICERs with Standard Care as base (default)
icer_results <- calculate_icer_table(
  decision_tree_table = decision_tree,
  base_scenario = 2,
  incremental_mode = "base",
  scenario_labels = scenario_labels,
  base_label_symbol = "*"
)

cat("\n===== ICER RESULTS (Base Mode) =====\n\n")
print(icer_results, row.names = FALSE)
cat("\n")


# =============================================================================
# INTERPRETATION OF RESULTS
# =============================================================================

cat("===== INTERPRETATION =====\n\n")

cat("Expected Values (Probability-Weighted):\n")
cat("----------------------------------------\n")

for (i in 1:nrow(icer_results)) {
  scenario <- icer_results$scenario_label[i]
  cost <- icer_results$total_cost[i]
  qaly <- icer_results$total_health_outcome[i]

  cat(sprintf("%s:\n", scenario))
  cat(sprintf("  Total Cost: $%.2f\n", cost))
  cat(sprintf("  Total QALYs: %.2f\n", qaly))

  if (!grepl("\\*", scenario)) {
    inc_cost <- icer_results$incremental_cost[i]
    inc_qaly <- icer_results$incremental_health_outcome[i]
    icer <- icer_results$icer[i]

    cat(sprintf("  Incremental Cost: $%.2f\n", inc_cost))
    cat(sprintf("  Incremental QALYs: %.2f\n", inc_qaly))
    cat(sprintf("  ICER: %s\n", icer))

    if (icer != "Ref" && icer != "Dominant" && icer != "Dominated" && !grepl("Inf", icer)) {
      cat(sprintf("  (Costs $%s per additional QALY gained)\n", icer))
    }
  } else {
    cat("  (Reference/Base scenario)\n")
  }
  cat("\n")
}


# =============================================================================
# DETAILED CALCULATION BREAKDOWN
# =============================================================================

cat("===== CALCULATION BREAKDOWN =====\n\n")

cat("Standard Care (Base):\n")
cat("  Expected Cost = 0.60 * $10,000 + 0.40 * $15,000\n")
cat("                = $6,000 + $6,000 = $12,000\n")
cat("  Expected QALYs = 0.60 * 8.0 + 0.40 * 5.0\n")
cat("                 = 4.8 + 2.0 = 6.8\n\n")

cat("Moderate Intervention:\n")
cat("  Expected Cost = 0.75 * $18,000 + 0.25 * $22,000\n")
cat("                = $13,500 + $5,500 = $19,000\n")
cat("  Expected QALYs = 0.75 * 9.0 + 0.25 * 6.0\n")
cat("                 = 6.75 + 1.5 = 8.25\n")
cat("  Incremental vs Standard:\n")
cat("    Cost: $19,000 - $12,000 = $7,000\n")
cat("    QALYs: 8.25 - 6.8 = 1.45\n")
cat("    ICER: $7,000 / 1.45 = $4,827.59 per QALY\n\n")

cat("Intensive Intervention:\n")
cat("  Expected Cost = 0.85 * $25,000 + 0.15 * $30,000\n")
cat("                = $21,250 + $4,500 = $25,750\n")
cat("  Expected QALYs = 0.85 * 10.0 + 0.15 * 7.0\n")
cat("                 = 8.5 + 1.05 = 9.55\n")
cat("  Incremental vs Standard:\n")
cat("    Cost: $25,750 - $12,000 = $13,750\n")
cat("    QALYs: 9.55 - 6.8 = 2.75\n")
cat("    ICER: $13,750 / 2.75 = $5,000 per QALY\n\n")


# =============================================================================
# Step 6: ICER Analysis with Previous Mode
# =============================================================================

cat("===== ICER ANALYSIS (Previous Mode) =====\n\n")

cat("In 'previous' mode, each scenario is compared to the previous row\n")
cat("This is useful for efficiency frontier analysis\n\n")

icer_previous <- calculate_icer_table(
  decision_tree_table = decision_tree,
  incremental_mode = "previous",
  scenario_labels = scenario_labels
)

print(icer_previous, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  Row 1: Standard Care is the reference (first scenario)\n")
cat("  Row 2: Moderate vs Standard Care\n")
cat("  Row 3: Intensive vs Moderate Intervention\n\n")


# =============================================================================
# Step 7: Change Base Scenario
# =============================================================================

cat("===== CHANGING BASE SCENARIO =====\n\n")

cat("Let's use Moderate Intervention as the base comparator:\n\n")

icer_moderate_base <- calculate_icer_table(
  decision_tree_table = decision_tree,
  base_scenario = 3,
  scenario_labels = scenario_labels
)

print(icer_moderate_base, row.names = FALSE)
cat("\n")

cat("Note: Standard Care now shows negative incremental values\n")
cat("      (less costly but also less effective)\n\n")


# =============================================================================
# EXAMPLE: DOMINATED SCENARIO
# =============================================================================

cat("===== EXAMPLE: DOMINATED SCENARIO =====\n\n")

# Create a scenario where one treatment is dominated
tree_dominated <- list(
  "1" = c(2, 3, 4),
  "2" = c(5),
  "3" = c(6),
  "4" = c(7)
)

costs_dominated <- list(
  "c1" = 10000,
  "c2" = 20000,  # More expensive
  "c3" = 15000
)

probs_dominated <- list(
  "p1" = 1.0,
  "p2" = 1.0,
  "p3" = 1.0
)

outcomes_dominated <- list(
  "q1" = 8.0,
  "q2" = 7.0,  # Less effective
  "q3" = 9.0
)

tree_dom <- build_decision_tree_table(
  list("1" = c(2, 3, 4), "2" = c(5), "3" = c(6), "4" = c(7)),
  costs_dominated,
  probs_dominated,
  outcomes_dominated,
  data.frame(name = c("c1", "c2", "c3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7)),
  data.frame(name = c("p1", "p2", "p3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7)),
  data.frame(name = c("q1", "q2", "q3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7))
)

icer_dom <- calculate_icer_table(
  tree_dom,
  scenario_labels = list("2" = "Treatment A", "3" = "Treatment B", "4" = "Treatment C")
)

cat("Treatment comparison with dominated scenario:\n\n")
print(icer_dom, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  Treatment B is DOMINATED (more costly, less effective than Treatment A)\n")
cat("  Treatment B should be eliminated from consideration\n")
cat("  Treatment C is more expensive but also more effective\n\n")


# =============================================================================
# EXAMPLE: DOMINANT SCENARIO
# =============================================================================

cat("===== EXAMPLE: DOMINANT SCENARIO =====\n\n")

# Create a scenario where one treatment is dominant
costs_dominant <- list(
  "c1" = 15000,
  "c2" = 10000,  # Less expensive
  "c3" = 20000
)

outcomes_dominant <- list(
  "q1" = 7.0,
  "q2" = 9.0,  # More effective
  "q3" = 8.0
)

tree_dominant <- build_decision_tree_table(
  list("1" = c(2, 3, 4), "2" = c(5), "3" = c(6), "4" = c(7)),
  costs_dominant,
  probs_dominated,
  outcomes_dominant,
  data.frame(name = c("c1", "c2", "c3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7)),
  data.frame(name = c("p1", "p2", "p3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7)),
  data.frame(name = c("q1", "q2", "q3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7))
)

icer_dominant <- calculate_icer_table(
  tree_dominant,
  scenario_labels = list("2" = "Treatment X", "3" = "Treatment Y", "4" = "Treatment Z")
)

cat("Treatment comparison with dominant scenario:\n\n")
print(icer_dominant, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  Treatment Y is DOMINANT (less costly, more effective than Treatment X)\n")
cat("  Treatment Y should be strongly preferred\n")
cat("  All other treatments can be compared to Treatment Y\n\n")


# =============================================================================
# EXAMPLE: INFINITE ICER
# =============================================================================

cat("===== EXAMPLE: INFINITE ICER =====\n\n")

# Same outcomes but different costs
costs_inf <- list(
  "c1" = 10000,
  "c2" = 15000,  # More expensive
  "c3" = 12000
)

outcomes_inf <- list(
  "q1" = 8.0,
  "q2" = 8.0,  # Same outcome
  "q3" = 8.0
)

tree_inf <- build_decision_tree_table(
  list("1" = c(2, 3, 4), "2" = c(5), "3" = c(6), "4" = c(7)),
  costs_inf,
  probs_dominated,
  outcomes_inf,
  data.frame(name = c("c1", "c2", "c3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7)),
  data.frame(name = c("p1", "p2", "p3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7)),
  data.frame(name = c("q1", "q2", "q3"), from_node = c(2, 3, 4), to_node = c(5, 6, 7))
)

icer_inf <- calculate_icer_table(
  tree_inf,
  scenario_labels = list("2" = "Option 1", "3" = "Option 2", "4" = "Option 3")
)

cat("Treatment comparison with equal outcomes:\n\n")
print(icer_inf, row.names = FALSE)
cat("\n")

cat("Interpretation:\n")
cat("  Option 2 has INFINITE ICER (costs more with no additional benefit)\n")
cat("  Option 3 also costs more with no additional benefit\n")
cat("  When outcomes are equal, choose the least costly option (Option 1)\n\n")


# =============================================================================
# KEY CONCEPTS
# =============================================================================

cat("===== KEY CONCEPTS =====\n\n")

cat("1. EXPECTED VALUES:\n")
cat("   - Total cost and health outcome are probability-weighted\n")
cat("   - Each terminal node contributes: value × path_probability\n")
cat("   - Path probability = product of all edge probabilities\n")
cat("   - Decision node (node 1) branches have no probabilities\n\n")

cat("2. INCREMENTAL ANALYSIS:\n")
cat("   - Base mode: All scenarios compared to base_scenario\n")
cat("   - Previous mode: Each scenario compared to previous row\n")
cat("   - Incremental cost = scenario_cost - comparator_cost\n")
cat("   - Incremental effect = scenario_effect - comparator_effect\n\n")

cat("3. ICER INTERPRETATION:\n")
cat("   - Ref: Reference/base scenario (incremental values = 0)\n")
cat("   - Calculable: ICER = incremental_cost / incremental_effect\n")
cat("   - Dominant: Less costly AND more effective (clear winner)\n")
cat("   - Dominated: More costly AND less effective (should reject)\n")
cat("   - Inf: Same effect but higher cost (no benefit for extra cost)\n")
cat("   - -Inf: Same effect but lower cost (cost saving, no difference)\n\n")

cat("4. SCENARIO LABELS:\n")
cat("   - Default: 'Scenario X' where X is the node number\n")
cat("   - Custom: Provide named list with node numbers as strings\n")
cat("   - Base marker: Symbol appended to base scenario label\n")
cat("   - Only applied in base mode, not previous mode\n\n")

cat("5. SORTING:\n")
cat("   - Default: Results sorted by scenario node number\n")
cat("   - Can disable with sort_by_node = FALSE\n")
cat("   - In previous mode, sorting affects comparisons\n\n")


# =============================================================================
# TIPS AND BEST PRACTICES
# =============================================================================

cat("===== TIPS AND BEST PRACTICES =====\n\n")

cat("1. Choose appropriate base scenario:\n")
cat("   - Usually the current standard of care\n")
cat("   - Or the least expensive/intensive option\n")
cat("   - Should be clinically relevant comparator\n\n")

cat("2. Interpret ICER in context:\n")
cat("   - Compare to willingness-to-pay threshold\n")
cat("   - Common threshold: $50,000-$150,000 per QALY (US)\n")
cat("   - Consider budget impact and feasibility\n\n")

cat("3. Check for dominance:\n")
cat("   - Dominated strategies can be eliminated immediately\n")
cat("   - Dominant strategies should be strongly preferred\n")
cat("   - Recalculate ICERs after removing dominated options\n\n")

cat("4. Use previous mode for efficiency frontier:\n")
cat("   - Sort scenarios by effectiveness first\n")
cat("   - Calculate incremental ratios between adjacent options\n")
cat("   - Identify which options lie on the frontier\n\n")

cat("5. Validate probability weighting:\n")
cat("   - Check that probabilities sum to 1.0 for each parent node\n")
cat("   - build_decision_tree_table() validates this automatically\n")
cat("   - Expected values should reflect realistic scenarios\n\n")

cat("6. Consider uncertainty:\n")
cat("   - ICERs are point estimates\n")
cat("   - Consider sensitivity analysis for key parameters\n")
cat("   - Vary probabilities, costs, and outcomes\n")
cat("   - Use probabilistic sensitivity analysis when appropriate\n\n")

cat("7. Document assumptions:\n")
cat("   - Clearly state the base scenario and why it was chosen\n")
cat("   - Document sources for probabilities and values\n")
cat("   - Explain any excluded scenarios or dominated options\n\n")


# =============================================================================
# INTEGRATION WITH OTHER FUNCTIONS
# =============================================================================

cat("===== INTEGRATION WITH OTHER CEDAR FUNCTIONS =====\n\n")

cat("The calculate_icer_table() function integrates with:\n\n")

cat("1. build_decision_tree_table():\n")
cat("   - Required input for ICER calculation\n")
cat("   - Validates tree structure and associations\n")
cat("   - Computes probability-weighted paths automatically\n\n")

cat("2. calculate_composite_costs():\n")
cat("   - Use to build complex cost structures\n")
cat("   - Apply discounting to multi-period costs\n")
cat("   - Pass calculated costs to build_decision_tree_table()\n\n")

cat("3. calculate_composite_health_outcomes():\n")
cat("   - Use to build complex QALY calculations\n")
cat("   - Apply utility weights and durations\n")
cat("   - Pass calculated outcomes to build_decision_tree_table()\n\n")

cat("4. plot_decision_tree():\n")
cat("   - Visualize the decision tree structure\n")
cat("   - Add ICER results as annotations\n")
cat("   - Help communicate results to stakeholders\n\n")


cat("===== END OF EXAMPLE =====\n\n")

cat("For more information:\n")
cat("  ?calculate_icer_table\n")
cat("  ?build_decision_tree_table\n")
cat("  ?calculate_composite_costs\n")
cat("  ?calculate_composite_health_outcomes\n")
