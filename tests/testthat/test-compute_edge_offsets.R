test_that("compute_edge_offsets handles simple sequential chain", {
  # Test case: 1->4->7->11->13
  # Durations: [0, 1, 1, 1]
  # Expected offsets: [0, 0, 1, 2]
  edges <- data.frame(
    from_node = c(1, 4, 7, 11),
    to_node = c(4, 7, 11, 13),
    duration = c(0, 1, 1, 1)
  )

  result <- compute_edge_offsets(edges)

  expect_true("time_offset" %in% names(result))
  expect_equal(result$time_offset, c(0, 0, 1, 2))
})

test_that("compute_edge_offsets handles branching structure", {
  # Test case: Tree with branches
  #     1
  #    / \
  #   2   3
  #  / \
  # 4   5
  edges <- data.frame(
    from_node = c(1, 1, 2, 2),
    to_node = c(2, 3, 4, 5),
    duration = c(1, 1, 2, 2)
  )

  result <- compute_edge_offsets(edges)

  expect_equal(result$time_offset, c(0, 0, 1, 1))
  # Both branches from node 1 start at t=0
  # Both branches from node 2 start at t=1 (after 1 year from node 1)
})

test_that("compute_edge_offsets handles NA durations as 0", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    duration = c(NA, 1, NA)
  )

  result <- compute_edge_offsets(edges)

  expect_equal(result$time_offset, c(0, 0, 1))
  # First edge: offset 0, duration 0 (NA treated as 0)
  # Second edge: offset 0 (at node 4), duration 1
  # Third edge: offset 1 (at node 7), duration 0
})

test_that("compute_edge_offsets preserves existing time_offset column", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    duration = c(0, 1, 1),
    time_offset = c(0, 5, 10)  # Manual overrides
  )

  expect_message(
    result <- compute_edge_offsets(edges),
    "already contains time_offset"
  )

  # Should preserve the existing values
  expect_equal(result$time_offset, c(0, 5, 10))
})

test_that("compute_edge_offsets handles missing duration column", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11)
    # No duration column
  )

  result <- compute_edge_offsets(edges)

  # Should set all offsets to 0
  expect_equal(result$time_offset, c(0, 0, 0))
})

test_that("compute_edge_offsets handles all NA durations", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    duration = c(NA, NA, NA)
  )

  result <- compute_edge_offsets(edges)

  # Should treat all as 0 duration
  expect_equal(result$time_offset, c(0, 0, 0))
})

test_that("compute_edge_offsets errors on invalid input", {
  # Not a data frame
  expect_error(
    compute_edge_offsets(list(from_node = 1, to_node = 2)),
    "must be a data frame"
  )

  # Missing required columns
  expect_error(
    compute_edge_offsets(data.frame(from = 1, to = 2)),
    "must contain 'from_node' and 'to_node'"
  )
})

test_that("compute_edge_offsets handles complex tree structure", {
  # User's example: 1->4->7->11->13->21
  # With durations: [0, 1, 1, 1, 1]
  # Expected offsets: [0, 0, 1, 2, 3]
  edges <- data.frame(
    from_node = c(1, 4, 7, 11, 13),
    to_node = c(4, 7, 11, 13, 21),
    duration = c(0, 1, 1, 1, 1)
  )

  result <- compute_edge_offsets(edges)

  expect_equal(result$time_offset, c(0, 0, 1, 2, 3))
})

test_that("compute_edge_offsets handles edges in arbitrary order", {
  # Edges provided in non-sequential order
  edges <- data.frame(
    from_node = c(7, 1, 11, 4),
    to_node = c(11, 4, 13, 7),
    duration = c(1, 0, 1, 1)
  )

  result <- compute_edge_offsets(edges)

  # Should still compute correct offsets
  # Edge 7->11: offset 1
  # Edge 1->4: offset 0
  # Edge 11->13: offset 2
  # Edge 4->7: offset 0
  expect_equal(result$time_offset, c(1, 0, 2, 0))
})

test_that("compute_edge_offsets warns on orphaned nodes", {
  # Edge with from_node that's never a to_node and isn't node 1
  edges <- data.frame(
    from_node = c(1, 4, 99),  # Node 99 is orphaned
    to_node = c(4, 7, 100),
    duration = c(0, 1, 1)
  )

  expect_warning(
    result <- compute_edge_offsets(edges),
    "Cannot compute offsets.*Orphaned from_nodes: 99"
  )

  # Orphaned edge should get offset 0
  expect_equal(result$time_offset[3], 0)
})

test_that("compute_edge_offsets handles multiple edges to same node", {
  # Two different paths to node 7
  edges <- data.frame(
    from_node = c(1, 1, 4, 5),
    to_node = c(4, 5, 7, 7),
    duration = c(1, 2, 1, 1)
  )

  # This should work, but might warn if paths have different cumulative times
  # Path 1->4->7: offset at 7 is 1+1=2
  # Path 1->5->7: offset at 7 is 2+1=3
  expect_warning(
    result <- compute_edge_offsets(edges),
    "Inconsistent times"
  )
})

test_that("compute_edge_offsets preserves other columns", {
  edges <- data.frame(
    from_node = c(1, 4),
    to_node = c(4, 7),
    duration = c(0, 1),
    custom_column = c("A", "B"),
    another_col = c(10, 20)
  )

  result <- compute_edge_offsets(edges)

  # Should preserve all original columns plus time_offset
  expect_true("custom_column" %in% names(result))
  expect_true("another_col" %in% names(result))
  expect_equal(result$custom_column, c("A", "B"))
  expect_equal(result$another_col, c(10, 20))
})