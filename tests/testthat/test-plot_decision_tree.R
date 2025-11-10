test_that("plot_decision_tree creates a ggplot object", {
  skip_if_not_installed("ggplot2")

  # Simple tree
  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list("q1" = 0.8, "q2" = 0.6)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("q1", "q2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree validates input types", {
  skip_if_not_installed("ggplot2")

  # Not a data frame
  expect_error(
    plot_decision_tree("not a data frame"),
    "tree_table must be a data frame"
  )

  # Missing required columns
  bad_df <- data.frame(x = 1, y = 2)
  expect_error(
    plot_decision_tree(bad_df),
    "tree_table is missing required columns"
  )
})


test_that("plot_decision_tree validates node_names parameter", {
  skip_if_not_installed("ggplot2")

  tree <- list("1" = c(2, 3))
  tree_table <- build_decision_tree_table(
    tree,
    list(),
    list(),
    list(),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer())
  )

  # Not a list
  expect_error(
    plot_decision_tree(tree_table, node_names = "not a list"),
    "node_names must be a named list or NULL"
  )

  # Unnamed list
  expect_error(
    plot_decision_tree(tree_table, node_names = list("A", "B")),
    "node_names must be a named list with node numbers as names"
  )
})


test_that("plot_decision_tree validates logical parameters", {
  skip_if_not_installed("ggplot2")

  tree <- list("1" = c(2, 3))
  tree_table <- build_decision_tree_table(
    tree,
    list(),
    list(),
    list(),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer())
  )

  expect_error(
    plot_decision_tree(tree_table, show_probabilities = "yes"),
    "show_probabilities must be TRUE or FALSE"
  )

  expect_error(
    plot_decision_tree(tree_table, show_time = 1),
    "show_time must be TRUE or FALSE"
  )

  expect_error(
    plot_decision_tree(tree_table, show_costs = "no"),
    "show_costs must be TRUE or FALSE"
  )

  expect_error(
    plot_decision_tree(tree_table, show_health_outcomes = 0),
    "show_health_outcomes must be TRUE or FALSE"
  )
})


test_that("plot_decision_tree validates node_spacing parameter", {
  skip_if_not_installed("ggplot2")

  tree <- list("1" = c(2, 3))
  tree_table <- build_decision_tree_table(
    tree,
    list(),
    list(),
    list(),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer())
  )

  expect_error(
    plot_decision_tree(tree_table, node_spacing = "2"),
    "node_spacing must be a positive number"
  )

  expect_error(
    plot_decision_tree(tree_table, node_spacing = -1),
    "node_spacing must be a positive number"
  )

  expect_error(
    plot_decision_tree(tree_table, node_spacing = 0),
    "node_spacing must be a positive number"
  )
})


test_that("plot_decision_tree works with minimal tree", {
  skip_if_not_installed("ggplot2")

  # Tree with just decision node and two terminal nodes
  tree <- list("1" = c(2, 3))

  tree_table <- build_decision_tree_table(
    tree,
    list(),
    list(),
    list(),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer())
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree works with complex tree", {
  skip_if_not_installed("ggplot2")

  # Complex tree with multiple levels
  tree <- list(
    "1" = c(2, 3, 4),
    "2" = c(5, 6, 7),
    "3" = c(8, 9),
    "4" = c(10, 11)
  )

  costs <- list()
  probs <- list(
    "p1" = 0.3, "p2" = 0.3, "p3" = 0.4,
    "p4" = 0.5, "p5" = 0.5,
    "p6" = 0.6, "p7" = 0.4
  )
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3", "p4", "p5", "p6", "p7"),
    from_node = c(2, 2, 2, 3, 3, 4, 4),
    to_node = c(5, 6, 7, 8, 9, 10, 11)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree works with custom node names", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list()
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  node_names <- list(
    "2" = "Treatment A",
    "3" = "Treatment B",
    "4" = "Success",
    "5" = "Failure"
  )

  result <- plot_decision_tree(tree_table, node_names = node_names)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree can hide all labels", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list("c1" = 1000, "c2" = 1500)
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list("q1" = 0.8, "q2" = 0.6)

  cost_assoc <- data.frame(
    name = c("c1", "c2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(
    name = c("q1", "q2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- plot_decision_tree(
    tree_table,
    show_probabilities = FALSE,
    show_time = FALSE,
    show_costs = FALSE,
    show_health_outcomes = FALSE
  )

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree handles time duration in years", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list()
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  edge_props <- data.frame(
    from_node = c(2, 2),
    to_node = c(4, 5),
    years = c(5, 10)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc,
    edge_properties = edge_props
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree handles time duration in months", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list()
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  edge_props <- data.frame(
    from_node = c(2, 2),
    to_node = c(4, 5),
    months = c(6, 12)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc,
    edge_properties = edge_props
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree handles mixed time units", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3),
    "2" = c(4, 5)
  )

  costs <- list()
  probs <- list("p1" = 0.6, "p2" = 0.4)
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2"),
    from_node = c(2, 2),
    to_node = c(4, 5)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  # One edge with years, one with months
  edge_props <- data.frame(
    from_node = c(2, 2),
    to_node = c(4, 5),
    years = c(5, NA),
    months = c(NA, 12)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc,
    edge_properties = edge_props
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree handles all time units", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3, 4, 5, 6)
  )

  costs <- list()
  probs <- list()
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  prob_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())
  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  # Different time units for each edge
  edge_props <- data.frame(
    from_node = c(1, 1, 1, 1, 1),
    to_node = c(2, 3, 4, 5, 6),
    years = c(1, NA, NA, NA, NA),
    months = c(NA, 6, NA, NA, NA),
    days = c(NA, NA, 30, NA, NA),
    hours = c(NA, NA, NA, 24, NA),
    minutes = c(NA, NA, NA, NA, 60)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc,
    edge_properties = edge_props
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree works with custom node_spacing", {
  skip_if_not_installed("ggplot2")

  tree <- list(
    "1" = c(2, 3, 4, 5)
  )

  tree_table <- build_decision_tree_table(
    tree,
    list(),
    list(),
    list(),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer())
  )

  # Test with different spacing values
  result1 <- plot_decision_tree(tree_table, node_spacing = 1)
  result2 <- plot_decision_tree(tree_table, node_spacing = 5)

  expect_s3_class(result1, "ggplot")
  expect_s3_class(result2, "ggplot")
})


test_that("plot_decision_tree works with deep tree structure", {
  skip_if_not_installed("ggplot2")

  # Tree with 4 levels
  tree <- list(
    "1" = c(2),
    "2" = c(3),
    "3" = c(4),
    "4" = c(5)
  )

  costs <- list()
  probs <- list("p1" = 1.0, "p2" = 1.0, "p3" = 1.0)
  outcomes <- list()

  cost_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  prob_assoc <- data.frame(
    name = c("p1", "p2", "p3"),
    from_node = c(2, 3, 4),
    to_node = c(3, 4, 5)
  )

  outcome_assoc <- data.frame(name = character(), from_node = integer(), to_node = integer())

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree works with wide tree structure", {
  skip_if_not_installed("ggplot2")

  # Tree with many branches from decision node
  tree <- list(
    "1" = c(2, 3, 4, 5, 6, 7, 8)
  )

  tree_table <- build_decision_tree_table(
    tree,
    list(),
    list(),
    list(),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer()),
    data.frame(name = character(), from_node = integer(), to_node = integer())
  )

  result <- plot_decision_tree(tree_table)

  expect_s3_class(result, "ggplot")
})


test_that("plot_decision_tree handles complete example with all features", {
  skip_if_not_installed("ggplot2")

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

  edge_props <- data.frame(
    from_node = c(2, 2, 3, 3),
    to_node = c(4, 5, 6, 7),
    years = c(5, 10, 3, 8)
  )

  tree_table <- build_decision_tree_table(
    tree, costs, probs, outcomes,
    cost_assoc, prob_assoc, outcome_assoc,
    edge_properties = edge_props
  )

  node_names <- list(
    "2" = "Treatment A",
    "3" = "Treatment B",
    "4" = "Success (A)",
    "5" = "Failure (A)",
    "6" = "Success (B)",
    "7" = "Failure (B)"
  )

  result <- plot_decision_tree(tree_table, node_names = node_names)

  expect_s3_class(result, "ggplot")
})
