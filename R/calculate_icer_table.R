#' Calculate Incremental Cost-Effectiveness Ratio (ICER) Table
#'
#' @description
#' Performs incremental cost-effectiveness analysis on decision tree results.
#' Takes the output from \code{\link{build_decision_tree_table}} and calculates
#' total costs, total health outcomes, incremental values, and ICERs for each
#' scenario (initial choice node from node 1). Uses probability weighting along
#' paths to terminal nodes to calculate expected values.
#'
#' @param decision_tree_table Data frame output from \code{\link{build_decision_tree_table}}.
#'   Must contain columns: from_node, to_node, probability, cost, health_outcome.
#' @param base_scenario Integer specifying which scenario node to use as the base
#'   comparator for incremental calculations. Must be a direct child of node 1.
#'   Default is 2.
#' @param incremental_mode Character string specifying how to calculate incremental
#'   values. Options are:
#'   \itemize{
#'     \item "base" (default): All scenarios compared to base_scenario
#'     \item "previous": Each scenario compared to the previous row (after sorting)
#'   }
#' @param scenario_labels Optional named list providing custom labels for scenarios.
#'   Names should be node numbers as strings, values are display labels.
#'   Example: \code{list("2" = "Standard Care", "3" = "New Treatment")}.
#'   If NULL (default), labels will be "Scenario X" where X is the node number.
#' @param base_label_symbol Character string to append to the base scenario label
#'   to mark it in the output. Default is "*".
#' @param sort_by_node Logical indicating whether to sort results by scenario node
#'   number. Default is TRUE.
#'
#' @return A data frame with one row per scenario containing:
#'   \itemize{
#'     \item scenario_node: Integer node number representing the scenario
#'     \item scenario_label: Character display name (with base marker if applicable)
#'     \item total_cost: Numeric probability-weighted expected cost
#'     \item total_health_outcome: Numeric probability-weighted expected health outcome
#'     \item incremental_cost: Numeric cost difference from comparator
#'     \item incremental_health_outcome: Numeric health outcome difference from comparator
#'     \item icer: Character or numeric ICER value, or special label
#'     \item icer_interpretation: Character description of ICER result
#'   }
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Identifies all scenarios (direct children of node 1)
#'   \item For each scenario, traces all paths to terminal nodes
#'   \item Calculates probability-weighted expected costs and health outcomes
#'   \item Computes incremental values based on selected mode
#'   \item Calculates and interprets ICER values
#' }
#'
#' ICER interpretation follows standard health economics conventions:
#' \itemize{
#'   \item "Ref": Reference/base scenario (incremental values = 0)
#'   \item "Dominant": Less costly and more effective (should be preferred)
#'   \item "Dominated": More costly and less effective (should be rejected)
#'   \item "Inf": Same effectiveness but higher cost (no benefit for extra cost)
#'   \item "-Inf": Same effectiveness but lower cost (cost saving, no effect change)
#'   \item Numeric: Standard ICER = incremental_cost / incremental_health_outcome
#' }
#'
#' Probability weighting: Each terminal node's cost and health outcome are
#' multiplied by the product of all probabilities along the path from the
#' scenario node to that terminal node. These weighted values are summed
#' across all terminal nodes reachable from each scenario.
#'
#' @examples
#' \dontrun{
#' # Define tree structure
#' tree <- list(
#'   "1" = c(2, 3),
#'   "2" = c(4, 5),
#'   "3" = c(6, 7)
#' )
#'
#' # Define values
#' costs <- list("c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500)
#' probs <- list("p1" = 0.6, "p2" = 0.4, "p3" = 0.7, "p4" = 0.3)
#' outcomes <- list("qaly1" = 0.8, "qaly2" = 0.6, "qaly3" = 0.9, "qaly4" = 0.7)
#'
#' # Define associations
#' cost_assoc <- tibble::tibble(
#'   name = c("c1", "c2", "c3", "c4"),
#'   from_node = c(2, 2, 3, 3),
#'   to_node = c(4, 5, 6, 7)
#' )
#'
#' prob_assoc <- tibble::tibble(
#'   name = c("p1", "p2", "p3", "p4"),
#'   from_node = c(2, 2, 3, 3),
#'   to_node = c(4, 5, 6, 7)
#' )
#'
#' outcome_assoc <- tibble::tibble(
#'   name = c("qaly1", "qaly2", "qaly3", "qaly4"),
#'   from_node = c(2, 2, 3, 3),
#'   to_node = c(4, 5, 6, 7)
#' )
#'
#' # Build decision tree table
#' tree_table <- build_decision_tree_table(
#'   tree, costs, probs, outcomes,
#'   cost_assoc, prob_assoc, outcome_assoc
#' )
#'
#' # Calculate ICER table with default settings (base mode, scenario 2 as base)
#' icer_results <- calculate_icer_table(tree_table)
#'
#' # Calculate ICER with custom labels
#' labels <- list("2" = "Standard Care", "3" = "New Treatment")
#' icer_results <- calculate_icer_table(
#'   tree_table,
#'   scenario_labels = labels
#' )
#'
#' # Calculate ICER using previous row comparison
#' icer_results <- calculate_icer_table(
#'   tree_table,
#'   incremental_mode = "previous"
#' )
#' }
#'
#' @export
calculate_icer_table <- function(decision_tree_table,
                                  base_scenario = 2,
                                  incremental_mode = c("base", "previous"),
                                  scenario_labels = NULL,
                                  base_label_symbol = "*",
                                  sort_by_node = TRUE) {

  # Match incremental_mode argument
  incremental_mode <- match.arg(incremental_mode)

  # Input validation
  if (!is.data.frame(decision_tree_table)) {
    stop("decision_tree_table must be a data frame")
  }

  required_cols <- c("from_node", "to_node", "probability", "cost", "health_outcome")
  missing_cols <- required_cols[!required_cols %in% names(decision_tree_table)]
  if (length(missing_cols) > 0) {
    stop(paste(
      "decision_tree_table is missing required columns:",
      paste(missing_cols, collapse = ", ")
    ))
  }

  # Verify tree starts with node 1
  if (!1 %in% decision_tree_table$from_node) {
    stop("decision_tree_table must contain node 1 as a from_node (root decision node)")
  }

  # Validate base_scenario
  if (!is.numeric(base_scenario) || length(base_scenario) != 1) {
    stop("base_scenario must be a single numeric value")
  }
  base_scenario <- as.integer(base_scenario)

  # Validate scenario_labels if provided
  if (!is.null(scenario_labels)) {
    if (!is.list(scenario_labels)) {
      stop("scenario_labels must be a named list or NULL")
    }
    if (is.null(names(scenario_labels)) || any(names(scenario_labels) == "")) {
      stop("scenario_labels must be a named list with all elements named")
    }
  }

  # Validate base_label_symbol
  if (!is.character(base_label_symbol) || length(base_label_symbol) != 1) {
    stop("base_label_symbol must be a single character string")
  }

  # Identify scenario nodes (direct children of node 1)
  scenario_nodes <- sort(unique(decision_tree_table$to_node[decision_tree_table$from_node == 1]))

  if (length(scenario_nodes) == 0) {
    stop("No scenarios found (node 1 has no children)")
  }

  if (length(scenario_nodes) == 1) {
    warning("Only one scenario found. ICER analysis requires at least two scenarios for comparison.")
  }

  # Validate base_scenario is a valid scenario
  if (!base_scenario %in% scenario_nodes) {
    stop(paste(
      "base_scenario", base_scenario,
      "is not a valid scenario (not a direct child of node 1).",
      "Available scenarios:", paste(scenario_nodes, collapse = ", ")
    ))
  }

  # Identify all nodes
  all_from_nodes <- unique(decision_tree_table$from_node)
  all_to_nodes <- unique(decision_tree_table$to_node)
  terminal_nodes <- setdiff(all_to_nodes, all_from_nodes)

  # Helper function to trace all paths from a node to terminal nodes
  trace_paths_from_node <- function(start_node, current_path = integer(0),
                                     current_prob = 1.0, visited = integer(0)) {
    # Check for cycles
    if (start_node %in% visited) {
      stop(paste("Cycle detected in tree structure at node", start_node))
    }

    # Add current node to path and visited set
    new_path <- c(current_path, start_node)
    new_visited <- c(visited, start_node)

    # Check if this is a terminal node
    if (start_node %in% terminal_nodes) {
      # Get cost and health outcome for this terminal node
      # Find the edge leading to this terminal node
      if (length(current_path) > 0) {
        parent_node <- current_path[length(current_path)]
        edge_row <- decision_tree_table[
          decision_tree_table$from_node == parent_node &
          decision_tree_table$to_node == start_node,
        ]

        if (nrow(edge_row) == 0) {
          stop(paste("No edge found from", parent_node, "to", start_node))
        }

        terminal_cost <- edge_row$cost[1]
        terminal_outcome <- edge_row$health_outcome[1]

        # Validate terminal values
        if (is.na(terminal_cost)) {
          stop(paste("Terminal node", start_node, "has missing cost value"))
        }
        if (is.na(terminal_outcome)) {
          stop(paste("Terminal node", start_node, "has missing health_outcome value"))
        }
        if (terminal_outcome < 0) {
          warning(paste("Negative health outcome detected at terminal node", start_node))
        }

        return(list(list(
          path = new_path,
          probability = current_prob,
          cost = terminal_cost,
          health_outcome = terminal_outcome
        )))
      } else {
        stop(paste("Terminal node", start_node, "has no parent"))
      }
    }

    # Not a terminal node, continue tracing
    # Find all children of this node
    children_edges <- decision_tree_table[decision_tree_table$from_node == start_node, ]

    if (nrow(children_edges) == 0) {
      stop(paste("Non-terminal node", start_node, "has no children"))
    }

    # Recursively trace paths through all children
    all_paths <- list()
    for (i in seq_len(nrow(children_edges))) {
      child_node <- children_edges$to_node[i]
      edge_prob <- children_edges$probability[i]

      # Check for missing probability (except from node 1)
      if (start_node != 1 && is.na(edge_prob)) {
        stop(paste(
          "Missing probability on edge from node", start_node,
          "to node", child_node
        ))
      }

      # Calculate cumulative probability
      # For edges from node 1 (decision node), don't multiply probability
      if (start_node == 1) {
        new_prob <- current_prob
      } else {
        new_prob <- current_prob * edge_prob
      }

      # Skip paths with zero probability
      if (new_prob > 0) {
        child_paths <- trace_paths_from_node(
          child_node,
          new_path,
          new_prob,
          new_visited
        )
        all_paths <- c(all_paths, child_paths)
      }
    }

    return(all_paths)
  }

  # Calculate expected values for each scenario
  scenario_results <- vector("list", length(scenario_nodes))

  for (i in seq_along(scenario_nodes)) {
    scenario_node <- scenario_nodes[i]

    # Trace all paths from this scenario to terminal nodes
    paths <- trace_paths_from_node(scenario_node)

    # Calculate probability-weighted expected values
    total_cost <- 0
    total_outcome <- 0

    for (path_info in paths) {
      weighted_cost <- path_info$cost * path_info$probability
      weighted_outcome <- path_info$health_outcome * path_info$probability
      total_cost <- total_cost + weighted_cost
      total_outcome <- total_outcome + weighted_outcome
    }

    scenario_results[[i]] <- list(
      scenario_node = scenario_node,
      total_cost = total_cost,
      total_health_outcome = total_outcome
    )
  }

  # Convert to data frame
  results_df <- do.call(rbind, lapply(scenario_results, function(x) {
    data.frame(
      scenario_node = x$scenario_node,
      total_cost = x$total_cost,
      total_health_outcome = x$total_health_outcome,
      stringsAsFactors = FALSE
    )
  }))

  # Sort by node number if requested
  if (sort_by_node) {
    results_df <- results_df[order(results_df$scenario_node), ]
  }

  # Calculate incremental values based on mode
  if (incremental_mode == "base") {
    # All scenarios compared to base scenario
    base_idx <- which(results_df$scenario_node == base_scenario)
    if (length(base_idx) == 0) {
      stop("Internal error: base_scenario not found in results")
    }

    base_cost <- results_df$total_cost[base_idx]
    base_outcome <- results_df$total_health_outcome[base_idx]

    results_df$incremental_cost <- results_df$total_cost - base_cost
    results_df$incremental_health_outcome <- results_df$total_health_outcome - base_outcome

  } else if (incremental_mode == "previous") {
    # Each scenario compared to previous row
    results_df$incremental_cost <- numeric(nrow(results_df))
    results_df$incremental_health_outcome <- numeric(nrow(results_df))

    for (i in seq_len(nrow(results_df))) {
      if (i == 1) {
        # First row: reference scenario
        results_df$incremental_cost[i] <- 0
        results_df$incremental_health_outcome[i] <- 0
      } else {
        # Compare to previous row
        results_df$incremental_cost[i] <- results_df$total_cost[i] - results_df$total_cost[i - 1]
        results_df$incremental_health_outcome[i] <- results_df$total_health_outcome[i] - results_df$total_health_outcome[i - 1]
      }
    }
  }

  # Calculate ICER with interpretation
  tolerance <- 1e-10
  results_df$icer <- character(nrow(results_df))
  results_df$icer_interpretation <- character(nrow(results_df))

  for (i in seq_len(nrow(results_df))) {
    inc_cost <- results_df$incremental_cost[i]
    inc_outcome <- results_df$incremental_health_outcome[i]

    # Check if this is the base/reference scenario
    is_reference <- FALSE
    if (incremental_mode == "base") {
      is_reference <- (results_df$scenario_node[i] == base_scenario)
    } else if (incremental_mode == "previous" && i == 1) {
      is_reference <- TRUE
    }

    if (is_reference) {
      results_df$icer[i] <- "Ref"
      results_df$icer_interpretation[i] <- "Reference"
    } else if (abs(inc_cost) < tolerance && abs(inc_outcome) < tolerance) {
      # Equal scenarios
      results_df$icer[i] <- "Equal"
      results_df$icer_interpretation[i] <- "Equal"
    } else if (inc_cost < 0 && inc_outcome > 0) {
      # Dominant: less costly, more effective
      results_df$icer[i] <- "Dominant"
      results_df$icer_interpretation[i] <- "Dominant"
    } else if (inc_cost > 0 && inc_outcome < 0) {
      # Dominated: more costly, less effective
      results_df$icer[i] <- "Dominated"
      results_df$icer_interpretation[i] <- "Dominated"
    } else if (abs(inc_outcome) < tolerance) {
      # Same outcome, different cost
      if (inc_cost > 0) {
        results_df$icer[i] <- "Inf"
        results_df$icer_interpretation[i] <- "Infinite (no benefit for extra cost)"
      } else if (inc_cost < 0) {
        results_df$icer[i] <- "-Inf"
        results_df$icer_interpretation[i] <- "Negative Infinite (cost saving, no effect change)"
      } else {
        results_df$icer[i] <- "Equal"
        results_df$icer_interpretation[i] <- "Equal"
      }
    } else {
      # Calculable ICER
      icer_value <- inc_cost / inc_outcome
      results_df$icer[i] <- as.character(round(icer_value, 2))
      results_df$icer_interpretation[i] <- "Calculable"
    }
  }

  # Apply scenario labels
  results_df$scenario_label <- character(nrow(results_df))

  for (i in seq_len(nrow(results_df))) {
    node_num <- results_df$scenario_node[i]
    node_str <- as.character(node_num)

    # Get custom label if provided, otherwise use default
    if (!is.null(scenario_labels) && node_str %in% names(scenario_labels)) {
      label <- scenario_labels[[node_str]]
    } else {
      label <- paste("Scenario", node_num)
    }

    # Add base marker if this is the base scenario (in base mode)
    if (incremental_mode == "base" && node_num == base_scenario) {
      label <- paste0(label, base_label_symbol)
    }

    results_df$scenario_label[i] <- label
  }

  # Reorder columns for better presentation
  results_df <- results_df[, c(
    "scenario_node",
    "scenario_label",
    "total_cost",
    "total_health_outcome",
    "incremental_cost",
    "incremental_health_outcome",
    "icer",
    "icer_interpretation"
  )]

  # Reset row names
  rownames(results_df) <- NULL

  return(results_df)
}
