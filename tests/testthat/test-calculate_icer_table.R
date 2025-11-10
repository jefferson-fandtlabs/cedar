test_that("calculate_icer_table calculates expected values correctly", {
  # Simple tree with known probabilities
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5),
    "3" = c(6, 7)
  )

  costs <- list("c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500)
  probs <- list("p1" = 0.6, "p2" = 0.4, "p3" = 0.7, "p4" = 0.3)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.6, "qaly3" = 0.9, "qaly4" = 0.7)

  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3", "c4"),
    from_node = c(2, 2, 3, 3),
    to_node = c(4, 5, 6, 7)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3", "p4"),
    from_node = c(2, 2, 3, 3),
    to_node = c(4, 5, 6, 7)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2", "qaly3", "qaly4"),
    from_node = c(2, 2, 3, 3),
    to_node = c(4, 5, 6, 7)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table)

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c(
    "scenario_node", "scenario_label", "total_cost", "total_health_outcome",
    "incremental_cost", "incremental_health_outcome", "icer", "icer_interpretation"
  ))

  # Check number of scenarios
  expect_equal(nrow(result), 2)

  # Check expected values (probability-weighted)
  # Scenario 2: 0.6*1000 + 0.4*1500 = 600 + 600 = 1200
  # Scenario 3: 0.7*2000 + 0.3*2500 = 1400 + 750 = 2150
  expect_equal(result$total_cost[result$scenario_node == 2], 1200)
  expect_equal(result$total_cost[result$scenario_node == 3], 2150)

  # Scenario 2: 0.6*0.8 + 0.4*0.6 = 0.48 + 0.24 = 0.72
  # Scenario 3: 0.7*0.9 + 0.3*0.7 = 0.63 + 0.21 = 0.84
  expect_equal(result$total_health_outcome[result$scenario_node == 2], 0.72)
  expect_equal(result$total_health_outcome[result$scenario_node == 3], 0.84)
})

test_that("calculate_icer_table calculates incremental values in base mode", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.9)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # Base scenario (2) should have 0 incremental values
  expect_equal(result$incremental_cost[result$scenario_node == 2], 0)
  expect_equal(result$incremental_health_outcome[result$scenario_node == 2], 0)
  expect_equal(result$icer[result$scenario_node == 2], "Ref")

  # Scenario 3 incremental values vs scenario 2
  expect_equal(result$incremental_cost[result$scenario_node == 3], 500)  # 1500 - 1000
  expect_equal(result$incremental_health_outcome[result$scenario_node == 3], 0.1)  # 0.9 - 0.8
})

test_that("calculate_icer_table calculates incremental values in previous mode", {
  tree <- list(
    "1" = c(2, 3, 4),
    "2" = c(5),
    "3" = c(6),
    "4" = c(7)
  )

  costs <- list("c1" = 1000, "c2" = 1200, "c3" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0, "p3" = 1.0)
  outcomes <- list("qaly1" = 0.7, "qaly2" = 0.8, "qaly3" = 0.9)

  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3"),
    from_node = c(2, 3, 4),
    to_node = c(5, 6, 7)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3"),
    from_node = c(2, 3, 4),
    to_node = c(5, 6, 7)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2", "qaly3"),
    from_node = c(2, 3, 4),
    to_node = c(5, 6, 7)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, incremental_mode = "previous")

  # First scenario should be reference
  expect_equal(result$incremental_cost[1], 0)
  expect_equal(result$incremental_health_outcome[1], 0)
  expect_equal(result$icer[1], "Ref")

  # Second scenario compared to first
  expect_equal(result$incremental_cost[2], 200)  # 1200 - 1000
  expect_equal(result$incremental_health_outcome[2], 0.1)  # 0.8 - 0.7

  # Third scenario compared to second
  expect_equal(result$incremental_cost[3], 300)  # 1500 - 1200
  expect_equal(result$incremental_health_outcome[3], 0.1)  # 0.9 - 0.8
})

test_that("calculate_icer_table identifies dominated scenarios", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)  # More expensive
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.6)  # Less effective

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # Scenario 3 should be dominated (more costly, less effective)
  expect_equal(result$icer[result$scenario_node == 3], "Dominated")
  expect_equal(result$icer_interpretation[result$scenario_node == 3], "Dominated")
})

test_that("calculate_icer_table identifies dominant scenarios", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1500, "c2" = 1000)  # Less expensive
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.6, "qaly2" = 0.8)  # More effective

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # Scenario 3 should be dominant (less costly, more effective)
  expect_equal(result$icer[result$scenario_node == 3], "Dominant")
  expect_equal(result$icer_interpretation[result$scenario_node == 3], "Dominant")
})

test_that("calculate_icer_table identifies infinite ICER", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)  # Different costs
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.8)  # Same outcomes

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # Scenario 3 should have Inf ICER (more costly, same outcome)
  expect_equal(result$icer[result$scenario_node == 3], "Inf")
  expect_match(result$icer_interpretation[result$scenario_node == 3], "Infinite")
})

test_that("calculate_icer_table calculates ICER numerically", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 2000)
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.5, "qaly2" = 0.7)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # ICER = (2000 - 1000) / (0.7 - 0.5) = 1000 / 0.2 = 5000
  expect_equal(result$icer[result$scenario_node == 3], "5000")
  expect_equal(result$icer_interpretation[result$scenario_node == 3], "Calculable")
})

test_that("calculate_icer_table applies custom scenario labels", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.9)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  labels <- list("2" = "Standard Care", "3" = "New Treatment")
  result <- calculate_icer_table(tree_table, scenario_labels = labels)

  expect_equal(result$scenario_label[result$scenario_node == 2], "Standard Care*")
  expect_equal(result$scenario_label[result$scenario_node == 3], "New Treatment")
})

test_that("calculate_icer_table marks base scenario with symbol", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.9)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2, base_label_symbol = "**")

  expect_match(result$scenario_label[result$scenario_node == 2], "\\*\\*$")
  expect_equal(result$scenario_label[result$scenario_node == 2], "Scenario 2**")
})

test_that("calculate_icer_table sorts by node number", {
  tree <- list(
    "1" = c(3, 2, 4),
    "2" = c(5),
    "3" = c(6),
    "4" = c(7)
  )

  costs <- list("c1" = 1000, "c2" = 1200, "c3" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0, "p3" = 1.0)
  outcomes <- list("qaly1" = 0.7, "qaly2" = 0.8, "qaly3" = 0.9)

  cost_assoc <- data.frame(
    name = c("c2", "c1", "c3"),
    from_node = c(3, 2, 4),
    to_node = c(6, 5, 7)
  )

  prob_assoc <- data.frame(
    name = c("p2", "p1", "p3"),
    from_node = c(3, 2, 4),
    to_node = c(6, 5, 7)
  )

  outcome_assoc <- data.frame(
    name = c("qaly2", "qaly1", "qaly3"),
    from_node = c(3, 2, 4),
    to_node = c(6, 5, 7)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, sort_by_node = TRUE)

  # Should be sorted 2, 3, 4
  expect_equal(result$scenario_node, c(2, 3, 4))
})

test_that("calculate_icer_table validates input types", {
  expect_error(
    calculate_icer_table("not a data frame"),
    "decision_tree_table must be a data frame"
  )
})

test_that("calculate_icer_table validates required columns", {
  bad_table <- data.frame(from_node = c(1, 2), to_node = c(2, 3))

  expect_error(
    calculate_icer_table(bad_table),
    "decision_tree_table is missing required columns"
  )
})

test_that("calculate_icer_table validates tree starts with node 1", {
  bad_table <- data.frame(
    from_node = c(2, 3),
    to_node = c(3, 4),
    probability = c(1.0, 1.0),
    cost = c(1000, 1500),
    health_outcome = c(0.8, 0.9)
  )

  expect_error(
    calculate_icer_table(bad_table),
    "must contain node 1"
  )
})

test_that("calculate_icer_table validates base_scenario is valid", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.9)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  expect_error(
    calculate_icer_table(tree_table, base_scenario = 5),
    "is not a valid scenario"
  )
})

test_that("calculate_icer_table handles single scenario with warning", {
  tree <- list(
    "1" = c(2),
    "2" = c(3)
  )

  costs <- list("c1" = 1000)
  probs <- list("p1" = 1.0)
  outcomes <- list("qaly1" = 0.8)

  cost_assoc <- data.frame(
    name = "c1",
    from_node = 2,
    to_node = 3
  )

  prob_assoc <- data.frame(
    name = "p1",
    from_node = 2,
    to_node = 3
  )

  outcome_assoc <- data.frame(
    name = "qaly1",
    from_node = 2,
    to_node = 3
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  expect_warning(
    result <- calculate_icer_table(tree_table, base_scenario = 2),
    "Only one scenario found"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$icer[1], "Ref")
})

test_that("calculate_icer_table detects missing terminal costs", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000)  # Missing c2
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.9)

  cost_assoc <- data.frame(
    name = "c1",  # Only cost for node 4
    from_node = 2,
    to_node = 4
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  expect_error(
    calculate_icer_table(tree_table, base_scenario = 2),
    "Terminal node.*has missing cost value"
  )
})

test_that("calculate_icer_table detects missing terminal outcomes", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4),
    "3" = c(5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 1.0, "p2" = 1.0)
  outcomes <- list("qaly1" = 0.8)  # Missing qaly2

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 3),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = "qaly1",  # Only outcome for node 4
    from_node = 2,
    to_node = 4
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  expect_error(
    calculate_icer_table(tree_table, base_scenario = 2),
    "Terminal node.*has missing health_outcome value"
  )
})

test_that("calculate_icer_table handles complex multi-branch trees", {
  # Tree with 3 scenarios, each with multiple branches
  tree <- list(
    "1" = c(2, 3, 4),
    "2" = c(5, 6),
    "3" = c(7, 8, 9),
    "4" = c(10, 11)
  )

  costs <- list(
    "c1" = 1000, "c2" = 1200,
    "c3" = 1500, "c4" = 1800, "c5" = 2000,
    "c6" = 2200, "c7" = 2500
  )

  probs <- list(
    "p1" = 0.6, "p2" = 0.4,
    "p3" = 0.5, "p4" = 0.3, "p5" = 0.2,
    "p6" = 0.7, "p7" = 0.3
  )

  outcomes <- list(
    "q1" = 0.7, "q2" = 0.8,
    "q3" = 0.75, "q4" = 0.85, "q5" = 0.9,
    "q6" = 0.65, "q7" = 0.95
  )

  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3", "c4", "c5", "c6", "c7"),
    from_node = c(2, 2, 3, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3", "p4", "p5", "p6", "p7"),
    from_node = c(2, 2, 3, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11)
  )

  outcome_assoc <- data.frame(
    name = c("q1", "q2", "q3", "q4", "q5", "q6", "q7"),
    from_node = c(2, 2, 3, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # Should have 3 scenarios
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$scenario_node), c(2, 3, 4))

  # Scenario 2: 0.6*1000 + 0.4*1200 = 600 + 480 = 1080
  expect_equal(result$total_cost[result$scenario_node == 2], 1080)

  # Scenario 2: 0.6*0.7 + 0.4*0.8 = 0.42 + 0.32 = 0.74
  expect_equal(result$total_health_outcome[result$scenario_node == 2], 0.74)

  # Base scenario should have Ref ICER
  expect_equal(result$icer[result$scenario_node == 2], "Ref")
})

test_that("calculate_icer_table handles zero probability paths", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5),
    "3" = c(6)
  )

  costs <- list("c1" = 1000, "c2" = 1500, "c3" = 2000)
  probs <- list("p1" = 0.0, "p2" = 1.0, "p3" = 1.0)  # Zero probability path
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.9, "qaly3" = 0.85)

  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3"),
    from_node = c(2, 2, 3),
    to_node = c(4, 5, 6)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3"),
    from_node = c(2, 2, 3),
    to_node = c(4, 5, 6)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2", "qaly3"),
    from_node = c(2, 2, 3),
    to_node = c(4, 5, 6)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- calculate_icer_table(tree_table, base_scenario = 2)

  # Scenario 2 should only count node 5 (0.0*1000 + 1.0*1500 = 1500)
  expect_equal(result$total_cost[result$scenario_node == 2], 1500)
  expect_equal(result$total_health_outcome[result$scenario_node == 2], 0.9)
})
