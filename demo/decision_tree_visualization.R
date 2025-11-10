# Decision Tree Visualization Demo
# This demo shows how to use plot_decision_tree() to visualize decision trees

library(cedar)

# ============================================================================
# Example 1: Simple Decision Tree
# ============================================================================

cat("\n=== Example 1: Simple Decision Tree ===\n")

# Define a simple tree structure
tree1 <- list(
  "1" = c(2, 3),  # Decision node branches to two options
  "2" = c(4, 5),  # Option A branches to success/failure
  "3" = c(6, 7)   # Option B branches to success/failure
)

# Define costs for terminal nodes
costs1 <- list(
  "c1" = 1000,  # Cost for node 4
  "c2" = 1500,  # Cost for node 5
  "c3" = 2000,  # Cost for node 6
  "c4" = 2500   # Cost for node 7
)

# Define probabilities for chance nodes
probs1 <- list(
  "p1" = 0.6,  # Probability 2->4
  "p2" = 0.4,  # Probability 2->5
  "p3" = 0.7,  # Probability 3->6
  "p4" = 0.3   # Probability 3->7
)

# Define health outcomes (QALYs)
outcomes1 <- list(
  "qaly1" = 0.8,
  "qaly2" = 0.6,
  "qaly3" = 0.9,
  "qaly4" = 0.7
)

# Define associations
cost_assoc1 <- data.frame(
  name = c("c1", "c2", "c3", "c4"),
  from_node = c(2, 2, 3, 3),
  to_node = c(4, 5, 6, 7)
)

prob_assoc1 <- data.frame(
  name = c("p1", "p2", "p3", "p4"),
  from_node = c(2, 2, 3, 3),
  to_node = c(4, 5, 6, 7)
)

outcome_assoc1 <- data.frame(
  name = c("qaly1", "qaly2", "qaly3", "qaly4"),
  from_node = c(2, 2, 3, 3),
  to_node = c(4, 5, 6, 7)
)

# Build the tree table
tree_table1 <- build_decision_tree_table(
  tree1, costs1, probs1, outcomes1,
  cost_assoc1, prob_assoc1, outcome_assoc1
)

# Create the visualization
plot1 <- plot_decision_tree(tree_table1)
print(plot1)

# ============================================================================
# Example 2: Decision Tree with Custom Node Names
# ============================================================================

cat("\n=== Example 2: Decision Tree with Custom Node Names ===\n")

# Use the same tree structure but add custom names
node_names1 <- list(
  "2" = "Treatment A",
  "3" = "Treatment B",
  "4" = "Success (A)",
  "5" = "Failure (A)",
  "6" = "Success (B)",
  "7" = "Failure (B)"
)

plot2 <- plot_decision_tree(tree_table1, node_names = node_names1)
print(plot2)

# ============================================================================
# Example 3: Decision Tree with Time Durations
# ============================================================================

cat("\n=== Example 3: Decision Tree with Time Durations ===\n")

# Add edge properties with time durations in years
edge_props1 <- data.frame(
  from_node = c(2, 2, 3, 3),
  to_node = c(4, 5, 6, 7),
  years = c(5, 10, 3, 8)
)

tree_table2 <- build_decision_tree_table(
  tree1, costs1, probs1, outcomes1,
  cost_assoc1, prob_assoc1, outcome_assoc1,
  edge_properties = edge_props1
)

plot3 <- plot_decision_tree(tree_table2, node_names = node_names1)
print(plot3)

# ============================================================================
# Example 4: Minimal Visualization (No Labels)
# ============================================================================

cat("\n=== Example 4: Minimal Visualization ===\n")

plot4 <- plot_decision_tree(
  tree_table2,
  show_probabilities = FALSE,
  show_time = FALSE,
  show_costs = FALSE,
  show_health_outcomes = FALSE
)
print(plot4)

# ============================================================================
# Example 5: Complex Tree with Multiple Levels
# ============================================================================

cat("\n=== Example 5: Complex Multi-Level Tree ===\n")

# More complex tree structure
tree2 <- list(
  "1" = c(2, 3, 4),     # Three treatment options
  "2" = c(5, 6, 7),     # Treatment A: 3 outcomes
  "3" = c(8, 9),        # Treatment B: 2 outcomes
  "4" = c(10, 11)       # Treatment C: 2 outcomes
)

# Define costs
costs2 <- list(
  "c1" = 1000, "c2" = 1100, "c3" = 1200,
  "c4" = 1300, "c5" = 1400,
  "c6" = 1500, "c7" = 1600
)

# Define probabilities
probs2 <- list(
  "p1" = 0.3, "p2" = 0.4, "p3" = 0.3,  # Treatment A
  "p4" = 0.6, "p5" = 0.4,              # Treatment B
  "p6" = 0.5, "p7" = 0.5               # Treatment C
)

# Define health outcomes
outcomes2 <- list(
  "qaly1" = 0.85, "qaly2" = 0.75, "qaly3" = 0.65,
  "qaly4" = 0.80, "qaly5" = 0.70,
  "qaly6" = 0.90, "qaly7" = 0.60
)

# Define associations
cost_assoc2 <- data.frame(
  name = c("c1", "c2", "c3", "c4", "c5", "c6", "c7"),
  from_node = c(2, 2, 2, 3, 3, 4, 4),
  to_node = c(5, 6, 7, 8, 9, 10, 11)
)

prob_assoc2 <- data.frame(
  name = c("p1", "p2", "p3", "p4", "p5", "p6", "p7"),
  from_node = c(2, 2, 2, 3, 3, 4, 4),
  to_node = c(5, 6, 7, 8, 9, 10, 11)
)

outcome_assoc2 <- data.frame(
  name = c("qaly1", "qaly2", "qaly3", "qaly4", "qaly5", "qaly6", "qaly7"),
  from_node = c(2, 2, 2, 3, 3, 4, 4),
  to_node = c(5, 6, 7, 8, 9, 10, 11)
)

# Build tree table
tree_table3 <- build_decision_tree_table(
  tree2, costs2, probs2, outcomes2,
  cost_assoc2, prob_assoc2, outcome_assoc2
)

# Add node names
node_names2 <- list(
  "2" = "Treatment A",
  "3" = "Treatment B",
  "4" = "Treatment C",
  "5" = "Best (A)",
  "6" = "Medium (A)",
  "7" = "Worst (A)",
  "8" = "Success (B)",
  "9" = "Failure (B)",
  "10" = "Success (C)",
  "11" = "Failure (C)"
)

plot5 <- plot_decision_tree(tree_table3, node_names = node_names2)
print(plot5)

# ============================================================================
# Example 6: Different Time Units
# ============================================================================

cat("\n=== Example 6: Mixed Time Units ===\n")

# Create a tree with different time units for different edges
tree3 <- list(
  "1" = c(2, 3),
  "2" = c(4, 5),
  "3" = c(6, 7)
)

# Use different time units for different edges
edge_props2 <- data.frame(
  from_node = c(2, 2, 3, 3),
  to_node = c(4, 5, 6, 7),
  years = c(5, NA, NA, NA),
  months = c(NA, 6, NA, NA),
  days = c(NA, NA, 30, NA),
  hours = c(NA, NA, NA, 24)
)

tree_table4 <- build_decision_tree_table(
  tree1, costs1, probs1, outcomes1,
  cost_assoc1, prob_assoc1, outcome_assoc1,
  edge_properties = edge_props2
)

plot6 <- plot_decision_tree(tree_table4, node_names = node_names1)
print(plot6)

# ============================================================================
# Example 7: Custom Node Spacing
# ============================================================================

cat("\n=== Example 7: Custom Node Spacing ===\n")

# Wide tree with many branches
tree4 <- list(
  "1" = c(2, 3, 4, 5, 6)
)

tree_table5 <- build_decision_tree_table(
  tree4,
  list(),
  list(),
  list(),
  data.frame(name = character(), from_node = integer(), to_node = integer()),
  data.frame(name = character(), from_node = integer(), to_node = integer()),
  data.frame(name = character(), from_node = integer(), to_node = integer())
)

# Default spacing
plot7a <- plot_decision_tree(tree_table5)
print(plot7a)

# Tight spacing
plot7b <- plot_decision_tree(tree_table5, node_spacing = 1)
print(plot7b)

# Wide spacing
plot7c <- plot_decision_tree(tree_table5, node_spacing = 4)
print(plot7c)

# ============================================================================
# Example 8: Saving the Plot
# ============================================================================

cat("\n=== Example 8: Saving the Plot ===\n")

# Create a plot
plot8 <- plot_decision_tree(tree_table2, node_names = node_names1)

# Save to file (requires ggplot2)
# Uncomment the following lines to save the plot:
# ggplot2::ggsave("decision_tree.png", plot8, width = 10, height = 6, dpi = 300)
# ggplot2::ggsave("decision_tree.pdf", plot8, width = 10, height = 6)

cat("To save a plot, use ggplot2::ggsave():\n")
cat("  ggplot2::ggsave('my_tree.png', plot, width = 10, height = 6)\n")

cat("\nDemo complete!\n")
