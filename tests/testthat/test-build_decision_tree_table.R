test_that("build_decision_tree_table creates correct table structure", {
  # Define a simple tree structure
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5),
    "3" = c(6, 7)
  )

  # Define costs
  costs <- list("c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500)

  # Define probabilities
  probs <- list("p1" = 0.6, "p2" = 0.4, "p3" = 0.7, "p4" = 0.3)

  # Define health outcomes
  outcomes <- list("qaly1" = 0.8, "qaly2" = 0.6, "qaly3" = 0.9, "qaly4" = 0.7)

  # Define associations
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

  # Build table
  result <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  # Check structure
  expect_s3_class(result, "data.frame")
  expect_named(result, c("from_node", "to_node", "probability", "cost", "health_outcome"))

  # Check number of rows (should have 6 connections)
  expect_equal(nrow(result), 6)

  # Check that node 1 connections have NA probabilities
  node1_connections <- result[result$from_node == 1, ]
  expect_true(all(is.na(node1_connections$probability)))

  # Check that non-node 1 connections have probabilities
  non_node1_connections <- result[result$from_node != 1, ]
  expect_false(any(is.na(non_node1_connections$probability)))

  # Check that only terminal nodes have costs and outcomes
  non_terminal_rows <- result[result$to_node %in% c(2, 3), ]
  expect_true(all(is.na(non_terminal_rows$cost)))
  expect_true(all(is.na(non_terminal_rows$health_outcome)))

  terminal_rows <- result[result$to_node %in% c(4, 5, 6, 7), ]
  expect_false(any(is.na(terminal_rows$cost)))
  expect_false(any(is.na(terminal_rows$health_outcome)))
})

test_that("build_decision_tree_table handles multiple branches from node 1", {
  # Tree with 4 branches from decision node (node 1)
  tree <- list(
    "1" = c(2, 3, 4, 5),
    "2" = c(6, 7),
    "3" = c(8, 9),
    "4" = c(10, 11),
    "5" = c(12, 13)
  )

  # Define costs for all terminal nodes
  costs <- list(
    "c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500,
    "c5" = 3000, "c6" = 3500, "c7" = 4000, "c8" = 4500
  )

  # Define probabilities for all non-node-1 connections
  probs <- list(
    "p1" = 0.6, "p2" = 0.4, "p3" = 0.7, "p4" = 0.3,
    "p5" = 0.5, "p6" = 0.5, "p7" = 0.8, "p8" = 0.2
  )

  # Define health outcomes
  outcomes <- list(
    "qaly1" = 0.8, "qaly2" = 0.6, "qaly3" = 0.9, "qaly4" = 0.7,
    "qaly5" = 0.85, "qaly6" = 0.65, "qaly7" = 0.75, "qaly8" = 0.55
  )

  # Define associations for terminal nodes (6-13)
  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8"),
    from_node = c(2, 2, 3, 3, 4, 4, 5, 5),
    to_node = c(6, 7, 8, 9, 10, 11, 12, 13)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8"),
    from_node = c(2, 2, 3, 3, 4, 4, 5, 5),
    to_node = c(6, 7, 8, 9, 10, 11, 12, 13)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2", "qaly3", "qaly4", "qaly5", "qaly6", "qaly7", "qaly8"),
    from_node = c(2, 2, 3, 3, 4, 4, 5, 5),
    to_node = c(6, 7, 8, 9, 10, 11, 12, 13)
  )

  # Build table
  result <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  # Check that we have all connections (4 from node 1 + 8 to terminal nodes = 12)
  expect_equal(nrow(result), 12)

  # Check that node 1 has 4 connections with NA probabilities
  node1_connections <- result[result$from_node == 1, ]
  expect_equal(nrow(node1_connections), 4)
  expect_true(all(is.na(node1_connections$probability)))
  expect_equal(sort(node1_connections$to_node), c(2, 3, 4, 5))

  # Check that all non-node-1 connections have probabilities
  non_node1_connections <- result[result$from_node != 1, ]
  expect_equal(nrow(non_node1_connections), 8)
  expect_false(any(is.na(non_node1_connections$probability)))

  # Check that only terminal nodes (6-13) have costs and outcomes
  terminal_rows <- result[result$to_node %in% 6:13, ]
  expect_equal(nrow(terminal_rows), 8)
  expect_false(any(is.na(terminal_rows$cost)))
  expect_false(any(is.na(terminal_rows$health_outcome)))

  # Check that non-terminal nodes (2-5) don't have costs or outcomes
  non_terminal_rows <- result[result$to_node %in% 2:5, ]
  expect_equal(nrow(non_terminal_rows), 4)
  expect_true(all(is.na(non_terminal_rows$cost)))
  expect_true(all(is.na(non_terminal_rows$health_outcome)))
})

test_that("build_decision_tree_table handles multiple branches from non-decision node", {
  # Tree with 5 branches from node 2 (chance node)
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5, 6, 7, 8),  # 5 branches from node 2
    "3" = c(9, 10)
  )

  # Define costs for all terminal nodes (4-10)
  costs <- list(
    "c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500,
    "c5" = 3000, "c6" = 3500, "c7" = 4000
  )

  # Define probabilities - need 5 for node 2 branches and 2 for node 3 branches
  probs <- list(
    "p1" = 0.2, "p2" = 0.2, "p3" = 0.2, "p4" = 0.2, "p5" = 0.2,  # Node 2 branches
    "p6" = 0.6, "p7" = 0.4  # Node 3 branches
  )

  # Define health outcomes for all terminal nodes
  outcomes <- list(
    "qaly1" = 0.8, "qaly2" = 0.6, "qaly3" = 0.9, "qaly4" = 0.7,
    "qaly5" = 0.85, "qaly6" = 0.65, "qaly7" = 0.75
  )

  # Define associations for terminal nodes (4-10)
  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3", "c4", "c5", "c6", "c7"),
    from_node = c(2, 2, 2, 2, 2, 3, 3),
    to_node = c(4, 5, 6, 7, 8, 9, 10)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3", "p4", "p5", "p6", "p7"),
    from_node = c(2, 2, 2, 2, 2, 3, 3),
    to_node = c(4, 5, 6, 7, 8, 9, 10)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2", "qaly3", "qaly4", "qaly5", "qaly6", "qaly7"),
    from_node = c(2, 2, 2, 2, 2, 3, 3),
    to_node = c(4, 5, 6, 7, 8, 9, 10)
  )

  # Build table
  result <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  # Check total connections (2 from node 1 + 5 from node 2 + 2 from node 3 = 9)
  expect_equal(nrow(result), 9)

  # Check node 2 has 5 branches with probabilities
  node2_connections <- result[result$from_node == 2, ]
  expect_equal(nrow(node2_connections), 5)
  expect_equal(sort(node2_connections$to_node), c(4, 5, 6, 7, 8))
  expect_false(any(is.na(node2_connections$probability)))

  # Verify the probabilities sum to 1.0 for node 2 branches
  expect_equal(sum(node2_connections$probability), 1.0)

  # Check that all node 2 terminal nodes have costs and outcomes
  expect_false(any(is.na(node2_connections$cost)))
  expect_false(any(is.na(node2_connections$health_outcome)))

  # Check node 3 has 2 branches
  node3_connections <- result[result$from_node == 3, ]
  expect_equal(nrow(node3_connections), 2)
  expect_equal(sort(node3_connections$to_node), c(9, 10))
  expect_false(any(is.na(node3_connections$probability)))
})

test_that("build_decision_tree_table handles complex tree with multiple multi-branch nodes", {
  # Complex tree: node 1 has 3 branches, node 2 has 4 branches, node 3 has 3 branches
  tree <- list(
    "1" = c(2, 3, 4),
    "2" = c(5, 6, 7, 8),
    "3" = c(9, 10, 11),
    "4" = c(12, 13)
  )

  # Define costs for all terminal nodes (5-13)
  costs <- list(
    "c1" = 1000, "c2" = 1100, "c3" = 1200, "c4" = 1300,
    "c5" = 1400, "c6" = 1500, "c7" = 1600, "c8" = 1700, "c9" = 1800
  )

  # Define probabilities
  probs <- list(
    "p1" = 0.25, "p2" = 0.25, "p3" = 0.25, "p4" = 0.25,  # Node 2 branches
    "p5" = 0.33, "p6" = 0.33, "p7" = 0.34,  # Node 3 branches
    "p8" = 0.5, "p9" = 0.5  # Node 4 branches
  )

  # Define health outcomes
  outcomes <- list(
    "qaly1" = 0.81, "qaly2" = 0.82, "qaly3" = 0.83, "qaly4" = 0.84,
    "qaly5" = 0.85, "qaly6" = 0.86, "qaly7" = 0.87, "qaly8" = 0.88, "qaly9" = 0.89
  )

  # Define associations
  cost_assoc <- data.frame(
    name = c("c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9"),
    from_node = c(2, 2, 2, 2, 3, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11, 12, 13)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3", "p4", "p5", "p6", "p7", "p8", "p9"),
    from_node = c(2, 2, 2, 2, 3, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11, 12, 13)
  )

  outcome_assoc <- data.frame(
    name = c("qaly1", "qaly2", "qaly3", "qaly4", "qaly5", "qaly6", "qaly7", "qaly8", "qaly9"),
    from_node = c(2, 2, 2, 2, 3, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11, 12, 13)
  )

  # Build table
  result <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  # Check total connections (3 from node 1 + 4 from node 2 + 3 from node 3 + 2 from node 4 = 12)
  expect_equal(nrow(result), 12)

  # Verify node 1 branches (3 branches, no probabilities)
  node1_connections <- result[result$from_node == 1, ]
  expect_equal(nrow(node1_connections), 3)
  expect_true(all(is.na(node1_connections$probability)))

  # Verify node 2 branches (4 branches with probabilities)
  node2_connections <- result[result$from_node == 2, ]
  expect_equal(nrow(node2_connections), 4)
  expect_false(any(is.na(node2_connections$probability)))
  expect_equal(sum(node2_connections$probability), 1.0)

  # Verify node 3 branches (3 branches with probabilities)
  node3_connections <- result[result$from_node == 3, ]
  expect_equal(nrow(node3_connections), 3)
  expect_false(any(is.na(node3_connections$probability)))
  expect_equal(sum(node3_connections$probability), 1.0)

  # Verify node 4 branches (2 branches with probabilities)
  node4_connections <- result[result$from_node == 4, ]
  expect_equal(nrow(node4_connections), 2)
  expect_false(any(is.na(node4_connections$probability)))
  expect_equal(sum(node4_connections$probability), 1.0)

  # Verify all terminal nodes have costs and outcomes
  terminal_rows <- result[result$to_node %in% 5:13, ]
  expect_equal(nrow(terminal_rows), 9)
  expect_false(any(is.na(terminal_rows$cost)))
  expect_false(any(is.na(terminal_rows$health_outcome)))
})

test_that("build_decision_tree_table validates tree structure starts with node 1", {
  # Tree that doesn't start with node 1
  tree <- list(
    "2" = c(3, 4)
  )

  costs <- list()
  probs <- list()
  outcomes <- list()
  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  prob_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, cost_assoc, prob_assoc, outcome_assoc),
    "Tree structure must start with node 1"
  )
})

test_that("build_decision_tree_table validates probability values", {
  tree <- list("1" = c(2, 3))

  costs <- list()
  probs <- list("p1" = 1.5)  # Invalid: > 1
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  prob_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, cost_assoc, prob_assoc, outcome_assoc),
    "All probabilities must be between 0 and 1"
  )
})

test_that("build_decision_tree_table requires probabilities for non-node-1 connections", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list()
  probs <- list("p1" = 0.6)  # Missing p2
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  # Missing probability for 2->5 connection
  prob_assoc <- data.frame(
    name = "p1",
    from_node = 2,
    to_node = 4
  )
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, cost_assoc, prob_assoc, outcome_assoc),
    "Missing probability associations for connections"
  )
})

test_that("build_decision_tree_table only allows costs on terminal nodes", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list("c1" = 1000)
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list()

  # Trying to assign cost to non-terminal node 2
  cost_assoc <- data.frame(
    name = "c1",
    from_node = 1,
    to_node = 2
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, cost_assoc, prob_assoc, outcome_assoc),
    "Costs can only be assigned to terminal nodes"
  )
})

test_that("build_decision_tree_table only allows health outcomes on terminal nodes", {
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list()
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list("qaly1" = 0.8)

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  # Trying to assign health outcome to non-terminal node 2
  outcome_assoc <- data.frame(
    name = "qaly1",
    from_node = 1,
    to_node = 2
  )

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, cost_assoc, prob_assoc, outcome_assoc),
    "Health outcomes can only be assigned to terminal nodes"
  )
})

test_that("build_decision_tree_table handles empty associations", {
  tree <- list("1" = c(2, 3))

  costs <- list()
  probs <- list()
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  prob_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  result <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  expect_equal(nrow(result), 2)
  expect_true(all(is.na(result$probability)))
  expect_true(all(is.na(result$cost)))
  expect_true(all(is.na(result$health_outcome)))
})

test_that("build_decision_tree_table validates input types", {
  expect_error(
    build_decision_tree_table(
      "not a list",
      list(),
      list(),
      list(),
      data.frame(),
      data.frame(),
      data.frame()
    ),
    "tree_structure must be a list"
  )

  expect_error(
    build_decision_tree_table(
      list("1" = c(2, 3)),
      "not a list",
      list(),
      list(),
      data.frame(),
      data.frame(),
      data.frame()
    ),
    "costs must be a list"
  )

  expect_error(
    build_decision_tree_table(
      list("1" = c(2, 3)),
      list(),
      "not a list",
      list(),
      data.frame(),
      data.frame(),
      data.frame()
    ),
    "probabilities must be a list"
  )

  expect_error(
    build_decision_tree_table(
      list("1" = c(2, 3)),
      list(),
      list(),
      "not a list",
      data.frame(),
      data.frame(),
      data.frame()
    ),
    "health_outcomes must be a list"
  )
})

test_that("build_decision_tree_table detects column name issues with spaces", {
  tree <- list("1" = c(2, 3))
  costs <- list()
  probs <- list()
  outcomes <- list()

  # Create a data frame with a space in the column name (simulating tribble error)
  bad_cost_assoc <- data.frame(
    name = character(),
    from_node = integer(),
    stringsAsFactors = FALSE
  )
  # Add column with space
  bad_cost_assoc[" to_node"] <- integer()

  prob_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  # Should provide helpful error message about the space
  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, bad_cost_assoc, prob_assoc, outcome_assoc),
    "to_node.*contains extra spaces"
  )

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, bad_cost_assoc, prob_assoc, outcome_assoc),
    "tribble\\(\\) or data\\.frame\\(\\) for extra spaces"
  )
})

test_that("build_decision_tree_table detects missing column names", {
  tree <- list("1" = c(2, 3))
  costs <- list()
  probs <- list()
  outcomes <- list()

  # Create data frame missing to_node column
  bad_prob_assoc <- data.frame(
    name = character(),
    from_node = integer()
  )

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  expect_error(
    build_decision_tree_table(tree, costs, probs, outcomes, cost_assoc, bad_prob_assoc, outcome_assoc),
    "probability_associations is missing required columns: to_node"
  )
})