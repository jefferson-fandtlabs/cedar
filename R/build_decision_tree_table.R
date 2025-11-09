#' Build Decision Tree Table
#'
#' @description
#' Converts a tree structure representation into a tabular format for decision tree
#' analysis. Takes tree structure, costs, probabilities, and health outcomes along
#' with their associations to create a table showing all node connections with their
#' associated values.
#'
#' @param tree_structure A named list where each name is a node number and the value
#'   is a vector of child node numbers. For example, list("1" = c(2, 3)) means node 1
#'   connects to nodes 2 and 3. All trees must start with node 1.
#' @param costs A named list where each name is a cost label and the value is the
#'   cost amount (numeric). For example, list("c1" = 1000.50).
#' @param probabilities A named list where each name is a probability label and the
#'   value is a probability between 0 and 1. For example, list("p1" = 0.75).
#' @param health_outcomes A named list where each name is a health outcome label and
#'   the value is the outcome value (numeric). For example, list("qaly1" = 0.8).
#' @param cost_associations A data frame or tibble with columns: name, from_node,
#'   to_node. Associates cost labels to node connections. Only terminal nodes should
#'   have cost associations.
#' @param probability_associations A data frame or tibble with columns: name, from_node,
#'   to_node. Associates probability labels to node connections. Required for all
#'   connections except those originating from node 1 (decision node).
#' @param health_outcome_associations A data frame or tibble with columns: name,
#'   from_node, to_node. Associates health outcome labels to node connections. Only
#'   terminal nodes should have health outcome associations.
#'
#' @return A data frame with columns: from_node, to_node, probability, cost,
#'   health_outcome. Each row represents a connection in the decision tree.
#'
#' @details
#' The function follows these rules:
#' \itemize{
#'   \item All trees must start with node 1
#'   \item Node 1 is the decision node and connections from it do not require probabilities
#'   \item All other connections require probabilities
#'   \item Costs and health outcomes are only assigned to terminal nodes
#'   \item One parent node can have multiple child nodes
#' }
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
#' # Define costs
#' costs <- list("c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500)
#'
#' # Define probabilities
#' probs <- list("p1" = 0.6, "p2" = 0.4, "p3" = 0.7, "p4" = 0.3)
#'
#' # Define health outcomes
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
#' # Build table
#' tree_table <- build_decision_tree_table(
#'   tree, costs, probs, outcomes,
#'   cost_assoc, prob_assoc, outcome_assoc
#' )
#' }
#'
#' @export
build_decision_tree_table <- function(tree_structure,
                                      costs,
                                      probabilities,
                                      health_outcomes,
                                      cost_associations,
                                      probability_associations,
                                      health_outcome_associations) {

  # Input validation
  if (!is.list(tree_structure)) {
    stop("tree_structure must be a list")
  }

  if (!"1" %in% names(tree_structure)) {
    stop("Tree structure must start with node 1")
  }

  if (!is.list(costs)) {
    stop("costs must be a list")
  }

  if (!is.list(probabilities)) {
    stop("probabilities must be a list")
  }

  if (!is.list(health_outcomes)) {
    stop("health_outcomes must be a list")
  }

  # Validate probability values
  prob_values <- unlist(probabilities)
  if (any(prob_values < 0 | prob_values > 1)) {
    stop("All probabilities must be between 0 and 1")
  }

  # Helper function to validate and provide helpful error messages for column names
  validate_association_columns <- function(df, context_name) {
    df <- as.data.frame(df)
    col_names <- names(df)

    required_cols <- c("name", "from_node", "to_node")
    missing_cols <- required_cols[!required_cols %in% col_names]

    if (length(missing_cols) > 0) {
      # Check for common issues like extra spaces
      trimmed_cols <- trimmed <- gsub("^\\s+|\\s+$", "", col_names)

      # Build detailed error message
      error_msg <- paste0(
        context_name, " is missing required columns: ",
        paste(missing_cols, collapse = ", "), "\n",
        "Found columns: ", paste(col_names, collapse = ", "), "\n"
      )

      # Check if any missing columns exist with spaces
      space_issues <- c()
      for (missing_col in missing_cols) {
        # Check for leading/trailing spaces
        space_variants <- c(
          paste0(" ", missing_col),
          paste0(missing_col, " "),
          paste0(" ", missing_col, " ")
        )
        found_variants <- space_variants[space_variants %in% col_names]
        if (length(found_variants) > 0) {
          space_issues <- c(space_issues, paste0(
            "  - '", missing_col, "' might be '", found_variants[1], "' (contains extra spaces)"
          ))
        }
      }

      if (length(space_issues) > 0) {
        error_msg <- paste0(
          error_msg,
          "\nPossible issues:\n",
          paste(space_issues, collapse = "\n"),
          "\n\nTip: Check your tribble() or data.frame() for extra spaces in column names (e.g., '~ to_node' should be '~to_node')"
        )
      }

      stop(error_msg)
    }

    return(df)
  }

  # Validate all association data frames
  cost_associations <- validate_association_columns(cost_associations, "cost_associations")
  probability_associations <- validate_association_columns(probability_associations, "probability_associations")
  health_outcome_associations <- validate_association_columns(health_outcome_associations, "health_outcome_associations")

  # Build base tree structure table from tree_structure list
  tree_connections <- vector("list", length = 0)

  for (from_node in names(tree_structure)) {
    to_nodes <- tree_structure[[from_node]]
    for (to_node in to_nodes) {
      tree_connections <- c(tree_connections, list(list(
        from_node = as.integer(from_node),
        to_node = as.integer(to_node)
      )))
    }
  }

  # Convert to data frame
  tree_df <- do.call(rbind, lapply(tree_connections, function(x) {
    data.frame(
      from_node = x$from_node,
      to_node = x$to_node,
      stringsAsFactors = FALSE
    )
  }))

  # Identify terminal nodes (nodes that are not parents)
  all_from_nodes <- unique(tree_df$from_node)
  all_to_nodes <- unique(tree_df$to_node)
  terminal_nodes <- setdiff(all_to_nodes, all_from_nodes)

  # Add probability associations
  if (nrow(probability_associations) > 0) {
    # Validate probability associations
    prob_assoc_df <- as.data.frame(probability_associations)

    # Check that probabilities exist for all non-node-1 connections
    non_decision_connections <- tree_df[tree_df$from_node != 1, ]

    if (nrow(non_decision_connections) > 0) {
      prob_check <- merge(
        non_decision_connections,
        prob_assoc_df,
        by = c("from_node", "to_node"),
        all.x = TRUE
      )

      missing_probs <- is.na(prob_check$name)
      if (any(missing_probs)) {
        missing_connections <- prob_check[missing_probs, c("from_node", "to_node")]
        stop(paste(
          "Missing probability associations for connections:",
          paste(apply(missing_connections, 1, function(x) paste(x, collapse = "->")), collapse = ", ")
        ))
      }
    }

    # Map probability names to values
    prob_assoc_df$probability <- probabilities[prob_assoc_df$name]
    prob_assoc_df$probability <- unlist(prob_assoc_df$probability)

    # Merge probabilities with tree structure
    tree_df <- merge(
      tree_df,
      prob_assoc_df[, c("from_node", "to_node", "probability")],
      by = c("from_node", "to_node"),
      all.x = TRUE
    )

    # Validate that probabilities sum to 1.0 for each parent node (except node 1)
    non_node1_with_probs <- tree_df[tree_df$from_node != 1 & !is.na(tree_df$probability), ]

    if (nrow(non_node1_with_probs) > 0) {
      # Calculate sum of probabilities for each parent node
      prob_sums <- aggregate(
        probability ~ from_node,
        data = non_node1_with_probs,
        FUN = sum
      )

      # Check for parent nodes where probabilities don't sum to 1.0
      # Use tolerance of 0.0001 for floating point comparison
      tolerance <- 0.0001
      invalid_sums <- prob_sums[abs(prob_sums$probability - 1.0) > tolerance, ]

      if (nrow(invalid_sums) > 0) {
        # Build detailed error message for each invalid parent node
        error_details <- lapply(invalid_sums$from_node, function(parent) {
          # Get all child nodes and their probabilities for this parent
          children <- non_node1_with_probs[non_node1_with_probs$from_node == parent, ]
          children <- children[order(children$to_node), ]

          # Format child node details
          child_details <- paste(
            sprintf("    Node %d: %.4f", children$to_node, children$probability),
            collapse = "\n"
          )

          # Calculate actual sum
          actual_sum <- sum(children$probability)

          # Build message for this parent node
          sprintf(
            "  Parent Node %d (sum = %.4f):\n%s",
            parent,
            actual_sum,
            child_details
          )
        })

        error_msg <- paste0(
          "Probabilities for child nodes must sum to 1.0 for each parent node.\n",
          "The following parent nodes have invalid probability sums:\n\n",
          paste(error_details, collapse = "\n\n"),
          "\n\nNote: Probabilities for branches from node 1 (decision node) are not required to sum to 1.0"
        )

        stop(error_msg)
      }
    }
  } else {
    tree_df$probability <- NA_real_
  }

  # Add cost associations (only for terminal nodes)
  if (nrow(cost_associations) > 0) {
    cost_assoc_df <- as.data.frame(cost_associations)

    # Validate that costs are only for terminal nodes
    non_terminal_costs <- cost_assoc_df[!cost_assoc_df$to_node %in% terminal_nodes, ]
    if (nrow(non_terminal_costs) > 0) {
      stop(paste(
        "Costs can only be assigned to terminal nodes. Non-terminal nodes found:",
        paste(unique(non_terminal_costs$to_node), collapse = ", ")
      ))
    }

    # Map cost names to values
    cost_assoc_df$cost <- costs[cost_assoc_df$name]
    cost_assoc_df$cost <- unlist(cost_assoc_df$cost)

    # Merge costs with tree structure
    tree_df <- merge(
      tree_df,
      cost_assoc_df[, c("from_node", "to_node", "cost")],
      by = c("from_node", "to_node"),
      all.x = TRUE
    )
  } else {
    tree_df$cost <- NA_real_
  }

  # Add health outcome associations (only for terminal nodes)
  if (nrow(health_outcome_associations) > 0) {
    outcome_assoc_df <- as.data.frame(health_outcome_associations)

    # Validate that outcomes are only for terminal nodes
    non_terminal_outcomes <- outcome_assoc_df[!outcome_assoc_df$to_node %in% terminal_nodes, ]
    if (nrow(non_terminal_outcomes) > 0) {
      stop(paste(
        "Health outcomes can only be assigned to terminal nodes. Non-terminal nodes found:",
        paste(unique(non_terminal_outcomes$to_node), collapse = ", ")
      ))
    }

    # Map outcome names to values
    outcome_assoc_df$health_outcome <- health_outcomes[outcome_assoc_df$name]
    outcome_assoc_df$health_outcome <- unlist(outcome_assoc_df$health_outcome)

    # Merge outcomes with tree structure
    tree_df <- merge(
      tree_df,
      outcome_assoc_df[, c("from_node", "to_node", "health_outcome")],
      by = c("from_node", "to_node"),
      all.x = TRUE
    )
  } else {
    tree_df$health_outcome <- NA_real_
  }

  # Sort by from_node and to_node for consistent output
  tree_df <- tree_df[order(tree_df$from_node, tree_df$to_node), ]

  # Reset row names
  rownames(tree_df) <- NULL

  return(tree_df)
}