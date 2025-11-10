# =============================================================================
# Tests for compute_edge_offsets() with auto-detection and duration_column parameter
# =============================================================================

test_that("compute_edge_offsets handles simple sequential chain with 'years' column", {
  # Test case: 1->4->7->11->13
  # Durations: [0, 1, 1, 1] years
  # Expected offsets: [0, 0, 1, 2]
  edges <- data.frame(
    from_node = c(1, 4, 7, 11),
    to_node = c(4, 7, 11, 13),
    years = c(0, 1, 1, 1)
  )

  expect_message(
    result <- compute_edge_offsets(edges),
    "Using 'years' column"
  )

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
    years = c(1, 1, 2, 2)
  )

  result <- suppressMessages(compute_edge_offsets(edges))

  expect_equal(result$time_offset, c(0, 0, 1, 1))
  # Both branches from node 1 start at t=0
  # Both branches from node 2 start at t=1 (after 1 year from node 1)
})

test_that("compute_edge_offsets handles NA durations as 0", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    years = c(NA, 1, NA)
  )

  result <- suppressMessages(compute_edge_offsets(edges))

  expect_equal(result$time_offset, c(0, 0, 1))
  # First edge: offset 0, duration 0 (NA treated as 0)
  # Second edge: offset 0 (at node 4), duration 1
  # Third edge: offset 1 (at node 7), duration 0
})

test_that("compute_edge_offsets preserves existing time_offset column", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    years = c(0, 1, 1),
    time_offset = c(0, 5, 10)  # Manual overrides
  )

  expect_message(
    result <- compute_edge_offsets(edges),
    "already contains time_offset"
  )

  # Should preserve the existing values
  expect_equal(result$time_offset, c(0, 5, 10))
})

test_that("compute_edge_offsets warns when no duration columns found", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11)
    # No duration column at all
  )

  expect_warning(
    result <- compute_edge_offsets(edges),
    "No duration columns found"
  )

  # Should set all offsets to 0
  expect_equal(result$time_offset, c(0, 0, 0))
})

test_that("compute_edge_offsets warns when all durations are NA", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    years = c(NA, NA, NA)
  )

  expect_warning(
    result <- compute_edge_offsets(edges),
    "All values in 'years' column are NA"
  )

  # Should set all offsets to 0
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
    years = c(0, 1, 1, 1, 1)
  )

  result <- suppressMessages(compute_edge_offsets(edges))

  expect_equal(result$time_offset, c(0, 0, 1, 2, 3))
})

test_that("compute_edge_offsets handles edges in arbitrary order", {
  # Edges provided in non-sequential order
  edges <- data.frame(
    from_node = c(7, 1, 11, 4),
    to_node = c(11, 4, 13, 7),
    years = c(1, 0, 1, 1)
  )

  result <- suppressMessages(compute_edge_offsets(edges))

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
    years = c(0, 1, 1)
  )

  expect_warning(
    result <- suppressMessages(compute_edge_offsets(edges)),
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
    years = c(1, 2, 1, 1)
  )

  # This should work, but might warn if paths have different cumulative times
  # Path 1->4->7: offset at 7 is 1+1=2
  # Path 1->5->7: offset at 7 is 2+1=3
  expect_warning(
    result <- suppressMessages(compute_edge_offsets(edges)),
    "Inconsistent times"
  )
})

test_that("compute_edge_offsets preserves other columns", {
  edges <- data.frame(
    from_node = c(1, 4),
    to_node = c(4, 7),
    years = c(0, 1),
    custom_column = c("A", "B"),
    another_col = c(10, 20)
  )

  result <- suppressMessages(compute_edge_offsets(edges))

  # Should preserve all original columns plus time_offset
  expect_true("custom_column" %in% names(result))
  expect_true("another_col" %in% names(result))
  expect_equal(result$custom_column, c("A", "B"))
  expect_equal(result$another_col, c(10, 20))
})

# =============================================================================
# Tests for duration_column parameter
# =============================================================================

test_that("compute_edge_offsets uses explicit duration_column parameter", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    months = c(0, 6, 12)
  )

  result <- compute_edge_offsets(edges, duration_column = "months")

  # Months should be converted to years: 0/12, 6/12, 12/12 = 0, 0.5, 1
  # Offsets: [0, 0, 0.5]
  expect_equal(result$time_offset, c(0, 0, 0.5))
})

test_that("compute_edge_offsets auto-detects 'months' when 'years' not available", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    months = c(0, 6, 12)
  )

  expect_message(
    result <- compute_edge_offsets(edges),
    "Using 'months' column"
  )

  expect_equal(result$time_offset, c(0, 0, 0.5))
})

test_that("compute_edge_offsets prioritizes 'years' over other duration columns", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    years = c(0, 1, 2),
    months = c(0, 12, 24),  # Same values but in months
    days = c(0, 365, 730)   # Same values but in days
  )

  expect_message(
    result <- compute_edge_offsets(edges),
    "Using 'years' column.*other available: months, days"
  )

  # Should use years column
  expect_equal(result$time_offset, c(0, 0, 1))
})

test_that("compute_edge_offsets converts days to years correctly", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    days = c(0, 365.25, 730.5)  # 0, 1, 2 years
  )

  result <- suppressMessages(compute_edge_offsets(edges, duration_column = "days"))

  expect_equal(result$time_offset, c(0, 0, 1), tolerance = 0.001)
})

test_that("compute_edge_offsets converts weeks to years correctly", {
  edges <- data.frame(
    from_node = c(1, 4, 7),
    to_node = c(4, 7, 11),
    weeks = c(0, 52.1775, 104.355)  # ~0, 1, 2 years
  )

  result <- suppressMessages(compute_edge_offsets(edges, duration_column = "weeks"))

  expect_equal(result$time_offset, c(0, 0, 1), tolerance = 0.001)
})

test_that("compute_edge_offsets converts hours to years correctly", {
  edges <- data.frame(
    from_node = c(1, 4),
    to_node = c(4, 7),
    hours = c(0, 8766)  # ~0, 1 year
  )

  result <- suppressMessages(compute_edge_offsets(edges, duration_column = "hours"))

  expect_equal(result$time_offset, c(0, 0), tolerance = 0.001)
})

test_that("compute_edge_offsets errors on invalid duration_column", {
  edges <- data.frame(
    from_node = c(1, 4),
    to_node = c(4, 7),
    years = c(0, 1)
  )

  # Non-character
  expect_error(
    compute_edge_offsets(edges, duration_column = 123),
    "duration_column must be a single character string"
  )

  # Multiple values
  expect_error(
    compute_edge_offsets(edges, duration_column = c("years", "months")),
    "duration_column must be a single character string"
  )

  # Column doesn't exist
  expect_error(
    compute_edge_offsets(edges, duration_column = "nonexistent"),
    "Specified duration_column 'nonexistent' not found"
  )
})

test_that("compute_edge_offsets works with mixed NA and non-NA durations", {
  edges <- data.frame(
    from_node = c(1, 4, 7, 11),
    to_node = c(4, 7, 11, 13),
    years = c(0, NA, 1, 2)  # NA in middle
  )

  result <- suppressMessages(compute_edge_offsets(edges))

  # NA treated as 0: offsets = [0, 0, 0, 1]
  expect_equal(result$time_offset, c(0, 0, 0, 1))
})

# =============================================================================
# Integration tests with user's scenario
# =============================================================================

test_that("compute_edge_offsets works with user's full tree structure", {
  # User's actual edge properties structure
  edges <- data.frame(
    from_node = c(1, 1, 1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 10, 11, 11, 12, 13),
    to_node = c(2, 3, 4, 14, 15, 5, 6, 7, 8, 9, 18, 10, 11, 19, 12, 13, 20, 21),
    years = c(0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 5, 1, 1, 1, 1, 1, 2, 1)
  )

  result <- suppressMessages(compute_edge_offsets(edges))

  # Verify key offsets for discounting
  expect_equal(result$time_offset[result$from_node == 7 & result$to_node == 10], 0)
  expect_equal(result$time_offset[result$from_node == 10 & result$to_node == 19], 1)
  expect_equal(result$time_offset[result$from_node == 7 & result$to_node == 11], 0)
  expect_equal(result$time_offset[result$from_node == 11 & result$to_node == 12], 1)
})
