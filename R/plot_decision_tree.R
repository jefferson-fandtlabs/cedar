#' Plot Decision Tree
#'
#' @description
#' Creates a visual representation of a decision tree from the table structure
#' created by \code{\link{build_decision_tree_table}}. The visualization uses
#' different shapes and colors to distinguish between decision nodes, chance
#' nodes, and terminal nodes, with labeled edges showing probabilities and
#' time durations.
#'
#' @param tree_table A data frame created by \code{\link{build_decision_tree_table}}
#'   containing columns: from_node, to_node, probability, probability_label,
#'   cost, cost_label, health_outcome, health_outcome_label, and optionally
#'   edge properties like years, months, days, hours, or minutes.
#' @param node_names Optional named list where names are node numbers (as
#'   character or integer) and values are custom names for the nodes. For
#'   example, \code{list("2" = "Treatment A", "3" = "Treatment B")}. If NULL,
#'   no custom names are displayed.
#' @param show_probabilities Logical. If TRUE, displays probability labels
#'   above edges. Default is TRUE.
#' @param show_time Logical. If TRUE, displays time duration labels below edges
#'   when available in edge_properties. Default is TRUE.
#' @param show_costs Logical. If TRUE, displays cost labels below terminal
#'   nodes. Default is TRUE.
#' @param show_health_outcomes Logical. If TRUE, displays health outcome labels
#'   below terminal nodes (and below costs if present). Default is TRUE.
#' @param node_spacing Numeric. Controls vertical spacing between nodes at the
#'   same level. Default is 2.
#'
#' @return A ggplot2 object representing the decision tree visualization. The
#'   plot can be displayed, saved, or further customized using ggplot2 functions.
#'
#' @details
#' The function creates a hierarchical left-to-right visualization with the
#' following characteristics:
#'
#' \strong{Node Types and Styling:}
#' \itemize{
#'   \item \strong{Decision Node (Node 1):} Green circle with black outline
#'   \item \strong{Chance Nodes:} Yellow squares with black outline (nodes with
#'     both parents and children)
#'   \item \strong{Terminal Nodes:} Red triangles (rotated -90 degrees) with
#'     black outline
#' }
#'
#' \strong{Node Labels:}
#' \itemize{
#'   \item Node number displayed inside each shape
#'   \item Optional custom name displayed below node (if node_names provided)
#'   \item Cost label displayed below custom name (if present and show_costs = TRUE)
#'   \item Health outcome label displayed below cost (if present and
#'     show_health_outcomes = TRUE)
#' }
#'
#' \strong{Edge Labels:}
#' \itemize{
#'   \item \strong{Probability:} Displayed above edge in format "label = value"
#'     (e.g., "p1 = 0.6"). Only shown for edges with probabilities and when
#'     show_probabilities = TRUE.
#'   \item \strong{Time Duration:} Displayed below edge in format "time = amount
#'     unit" (e.g., "time = 5 years"). The function searches for time columns
#'     in this order: years, months, days, hours, minutes. Only shown when
#'     show_time = TRUE and time data is available.
#' }
#'
#' \strong{Layout Algorithm:}
#' \itemize{
#'   \item Nodes are arranged left-to-right based on tree depth
#'   \item Vertical spacing is distributed evenly for nodes at the same depth
#'   \item Horizontal spacing is consistent between levels
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
#' # Define costs, probabilities, and outcomes
#' costs <- list("c1" = 1000, "c2" = 1500, "c3" = 2000, "c4" = 2500)
#' probs <- list("p1" = 0.6, "p2" = 0.4, "p3" = 0.7, "p4" = 0.3)
#' outcomes <- list("qaly1" = 0.8, "qaly2" = 0.6, "qaly3" = 0.9, "qaly4" = 0.7)
#'
#' # Define associations
#' cost_assoc <- data.frame(
#'   name = c("c1", "c2", "c3", "c4"),
#'   from_node = c(2, 2, 3, 3),
#'   to_node = c(4, 5, 6, 7)
#' )
#'
#' prob_assoc <- data.frame(
#'   name = c("p1", "p2", "p3", "p4"),
#'   from_node = c(2, 2, 3, 3),
#'   to_node = c(4, 5, 6, 7)
#' )
#'
#' outcome_assoc <- data.frame(
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
#'
#' # Create visualization
#' plot_decision_tree(tree_table)
#'
#' # With custom node names
#' node_names <- list(
#'   "2" = "Treatment A",
#'   "3" = "Treatment B",
#'   "4" = "Success (A)",
#'   "5" = "Failure (A)",
#'   "6" = "Success (B)",
#'   "7" = "Failure (B)"
#' )
#' plot_decision_tree(tree_table, node_names = node_names)
#'
#' # Minimal visualization (no labels)
#' plot_decision_tree(
#'   tree_table,
#'   show_probabilities = FALSE,
#'   show_costs = FALSE,
#'   show_health_outcomes = FALSE
#' )
#' }
#'
#' @seealso
#' \code{\link{build_decision_tree_table}} for creating the tree table input
#'
#' @export
plot_decision_tree <- function(tree_table,
                                node_names = NULL,
                                show_probabilities = TRUE,
                                show_time = TRUE,
                                show_costs = TRUE,
                                show_health_outcomes = TRUE,
                                node_spacing = 2) {

  # Check if ggplot2 is available
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function. Please install it with: install.packages('ggplot2')",
         call. = FALSE)
  }

  # Input validation
  if (!is.data.frame(tree_table)) {
    stop("tree_table must be a data frame", call. = FALSE)
  }

  required_cols <- c("from_node", "to_node")
  missing_cols <- required_cols[!required_cols %in% names(tree_table)]
  if (length(missing_cols) > 0) {
    stop(paste0(
      "tree_table is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }

  if (!is.null(node_names)) {
    if (!is.list(node_names)) {
      stop("node_names must be a named list or NULL", call. = FALSE)
    }
    if (is.null(names(node_names)) || any(names(node_names) == "")) {
      stop("node_names must be a named list with node numbers as names", call. = FALSE)
    }
  }

  if (!is.logical(show_probabilities)) {
    stop("show_probabilities must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(show_time)) {
    stop("show_time must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(show_costs)) {
    stop("show_costs must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.logical(show_health_outcomes)) {
    stop("show_health_outcomes must be TRUE or FALSE", call. = FALSE)
  }

  if (!is.numeric(node_spacing) || node_spacing <= 0) {
    stop("node_spacing must be a positive number", call. = FALSE)
  }

  # Classify nodes
  all_nodes <- unique(c(tree_table$from_node, tree_table$to_node))
  parent_nodes <- unique(tree_table$from_node)
  child_nodes <- unique(tree_table$to_node)
  terminal_nodes <- setdiff(child_nodes, parent_nodes)
  chance_nodes <- setdiff(parent_nodes, 1)

  # Calculate node depths (distance from node 1)
  node_depths <- calculate_node_depths(tree_table, all_nodes)

  # Calculate node positions
  node_positions <- calculate_node_positions(tree_table, node_depths, node_spacing)

  # Create node data frame for plotting
  node_df <- data.frame(
    node = all_nodes,
    x = node_positions$x[match(all_nodes, node_positions$node)],
    y = node_positions$y[match(all_nodes, node_positions$node)],
    stringsAsFactors = FALSE
  )

  # Add node type classification
  node_df$node_type <- ifelse(node_df$node == 1, "decision",
                               ifelse(node_df$node %in% terminal_nodes, "terminal", "chance"))

  # Prepare edge data with positions
  edge_df <- merge(
    tree_table,
    node_df[, c("node", "x", "y")],
    by.x = "from_node",
    by.y = "node"
  )
  names(edge_df)[names(edge_df) == "x"] <- "x_start"
  names(edge_df)[names(edge_df) == "y"] <- "y_start"

  edge_df <- merge(
    edge_df,
    node_df[, c("node", "x", "y")],
    by.x = "to_node",
    by.y = "node"
  )
  names(edge_df)[names(edge_df) == "x"] <- "x_end"
  names(edge_df)[names(edge_df) == "y"] <- "y_end"

  # Calculate midpoints for edge labels
  edge_df$x_mid <- (edge_df$x_start + edge_df$x_end) / 2
  edge_df$y_mid <- (edge_df$y_start + edge_df$y_end) / 2

  # Extract time information if present
  if (show_time) {
    time_cols <- c("years", "months", "days", "hours", "minutes")
    time_col_found <- time_cols[time_cols %in% names(edge_df)]
    if (length(time_col_found) > 0) {
      edge_df$time_label <- apply(edge_df, 1, function(row) {
        for (col in time_col_found) {
          if (!is.na(row[col])) {
            return(paste0("time = ", row[col], " ", col))
          }
        }
        return(NA_character_)
      })
    } else {
      edge_df$time_label <- NA_character_
    }
  } else {
    edge_df$time_label <- NA_character_
  }

  # Build probability labels
  if (show_probabilities && "probability_label" %in% names(edge_df) && "probability" %in% names(edge_df)) {
    edge_df$prob_label <- ifelse(
      !is.na(edge_df$probability_label) & !is.na(edge_df$probability),
      paste0(edge_df$probability_label, " = ", edge_df$probability),
      NA_character_
    )
  } else {
    edge_df$prob_label <- NA_character_
  }

  # Build node labels (cost and health outcome)
  node_df$cost_label_text <- NA_character_
  node_df$health_outcome_label_text <- NA_character_

  if (show_costs && "cost_label" %in% names(tree_table) && "cost" %in% names(tree_table)) {
    # Get terminal node costs
    terminal_costs <- tree_table[tree_table$to_node %in% terminal_nodes & !is.na(tree_table$cost_label), ]
    for (i in 1:nrow(terminal_costs)) {
      node_idx <- which(node_df$node == terminal_costs$to_node[i])
      if (length(node_idx) > 0) {
        node_df$cost_label_text[node_idx] <- paste0(
          terminal_costs$cost_label[i], " = ", terminal_costs$cost[i]
        )
      }
    }
  }

  if (show_health_outcomes && "health_outcome_label" %in% names(tree_table) && "health_outcome" %in% names(tree_table)) {
    # Get terminal node health outcomes
    terminal_outcomes <- tree_table[tree_table$to_node %in% terminal_nodes & !is.na(tree_table$health_outcome_label), ]
    for (i in 1:nrow(terminal_outcomes)) {
      node_idx <- which(node_df$node == terminal_outcomes$to_node[i])
      if (length(node_idx) > 0) {
        node_df$health_outcome_label_text[node_idx] <- paste0(
          terminal_outcomes$health_outcome_label[i], " = ", terminal_outcomes$health_outcome[i]
        )
      }
    }
  }

  # Add custom node names if provided
  node_df$custom_name <- NA_character_
  if (!is.null(node_names)) {
    for (node_id in names(node_names)) {
      node_num <- as.integer(node_id)
      node_idx <- which(node_df$node == node_num)
      if (length(node_idx) > 0) {
        node_df$custom_name[node_idx] <- node_names[[node_id]]
      }
    }
  }

  # Create the plot
  p <- ggplot2::ggplot()

  # Add edges (solid black lines)
  p <- p + ggplot2::geom_segment(
    data = edge_df,
    ggplot2::aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
    color = "black",
    linewidth = 0.5
  )

  # Add probability labels above edges
  if (show_probabilities) {
    prob_labels_df <- edge_df[!is.na(edge_df$prob_label), ]
    if (nrow(prob_labels_df) > 0) {
      p <- p + ggplot2::geom_text(
        data = prob_labels_df,
        ggplot2::aes(x = x_mid, y = y_mid + 0.15, label = prob_label),
        size = 3,
        color = "black"
      )
    }
  }

  # Add time labels below edges
  if (show_time) {
    time_labels_df <- edge_df[!is.na(edge_df$time_label), ]
    if (nrow(time_labels_df) > 0) {
      p <- p + ggplot2::geom_text(
        data = time_labels_df,
        ggplot2::aes(x = x_mid, y = y_mid - 0.15, label = time_label),
        size = 3,
        color = "black"
      )
    }
  }

  # Add nodes with different shapes and colors
  # Decision node (node 1): green circle
  decision_nodes <- node_df[node_df$node_type == "decision", ]
  if (nrow(decision_nodes) > 0) {
    p <- p + ggplot2::geom_point(
      data = decision_nodes,
      ggplot2::aes(x = x, y = y),
      shape = 21,  # Circle
      size = 8,
      fill = "green",
      color = "black",
      stroke = 1
    )
  }

  # Chance nodes: yellow square
  chance_nodes_df <- node_df[node_df$node_type == "chance", ]
  if (nrow(chance_nodes_df) > 0) {
    p <- p + ggplot2::geom_point(
      data = chance_nodes_df,
      ggplot2::aes(x = x, y = y),
      shape = 22,  # Square
      size = 8,
      fill = "yellow",
      color = "black",
      stroke = 1
    )
  }

  # Terminal nodes: red triangle rotated -90 degrees
  terminal_nodes_df <- node_df[node_df$node_type == "terminal", ]
  if (nrow(terminal_nodes_df) > 0) {
    p <- p + ggplot2::geom_point(
      data = terminal_nodes_df,
      ggplot2::aes(x = x, y = y),
      shape = 25,  # Downward triangle (rotated -90 from upward)
      size = 8,
      fill = "red",
      color = "black",
      stroke = 1
    )
  }

  # Add node numbers inside shapes
  p <- p + ggplot2::geom_text(
    data = node_df,
    ggplot2::aes(x = x, y = y, label = node),
    size = 4,
    color = "black",
    fontface = "bold"
  )

  # Add custom node names below nodes
  if (!is.null(node_names)) {
    custom_names_df <- node_df[!is.na(node_df$custom_name), ]
    if (nrow(custom_names_df) > 0) {
      p <- p + ggplot2::geom_text(
        data = custom_names_df,
        ggplot2::aes(x = x, y = y - 0.4, label = custom_name),
        size = 3,
        color = "black"
      )
    }
  }

  # Add cost labels below custom names (or below nodes if no custom names)
  if (show_costs) {
    cost_labels_df <- node_df[!is.na(node_df$cost_label_text), ]
    if (nrow(cost_labels_df) > 0) {
      # Adjust y position based on whether custom names are present
      cost_labels_df$y_pos <- ifelse(
        !is.na(cost_labels_df$custom_name),
        cost_labels_df$y - 0.7,
        cost_labels_df$y - 0.4
      )
      p <- p + ggplot2::geom_text(
        data = cost_labels_df,
        ggplot2::aes(x = x, y = y_pos, label = cost_label_text),
        size = 3,
        color = "black"
      )
    }
  }

  # Add health outcome labels below cost labels
  if (show_health_outcomes) {
    outcome_labels_df <- node_df[!is.na(node_df$health_outcome_label_text), ]
    if (nrow(outcome_labels_df) > 0) {
      # Adjust y position based on what's above
      outcome_labels_df$y_pos <- outcome_labels_df$y - 0.4
      if (!is.null(node_names)) {
        outcome_labels_df$y_pos <- ifelse(
          !is.na(outcome_labels_df$custom_name),
          outcome_labels_df$y_pos - 0.3,
          outcome_labels_df$y_pos
        )
      }
      if (show_costs) {
        outcome_labels_df$y_pos <- ifelse(
          !is.na(outcome_labels_df$cost_label_text),
          outcome_labels_df$y_pos - 0.3,
          outcome_labels_df$y_pos
        )
      }
      p <- p + ggplot2::geom_text(
        data = outcome_labels_df,
        ggplot2::aes(x = x, y = y_pos, label = health_outcome_label_text),
        size = 3,
        color = "black"
      )
    }
  }

  # Apply theme
  p <- p + ggplot2::theme_void()
  p <- p + ggplot2::coord_fixed()

  return(p)
}


# Helper function to calculate node depths (distance from root node 1)
calculate_node_depths <- function(tree_table, all_nodes) {
  depths <- rep(NA_integer_, length(all_nodes))
  names(depths) <- all_nodes

  # Node 1 is at depth 0
  depths["1"] <- 0
  current_depth <- 0
  processed <- c(1)

  while (any(is.na(depths))) {
    # Find all nodes that are children of nodes at current_depth
    current_level_nodes <- all_nodes[depths == current_depth & !is.na(depths)]
    if (length(current_level_nodes) == 0) break

    children <- tree_table$to_node[tree_table$from_node %in% current_level_nodes]
    children <- unique(children[!children %in% processed])

    if (length(children) == 0) break

    depths[as.character(children)] <- current_depth + 1
    processed <- c(processed, children)
    current_depth <- current_depth + 1
  }

  return(depths)
}


# Helper function to calculate node x,y positions
calculate_node_positions <- function(tree_table, node_depths, node_spacing) {
  all_nodes <- as.integer(names(node_depths))
  max_depth <- max(node_depths, na.rm = TRUE)

  # Calculate x positions based on depth
  x_positions <- node_depths * 3  # Horizontal spacing of 3 units per level

  # Calculate y positions - distribute nodes evenly within each level
  y_positions <- rep(NA_real_, length(all_nodes))

  for (depth in 0:max_depth) {
    nodes_at_depth <- all_nodes[node_depths == depth]
    n_nodes <- length(nodes_at_depth)

    if (n_nodes == 1) {
      # Single node centered at y = 0
      y_positions[all_nodes %in% nodes_at_depth] <- 0
    } else {
      # Multiple nodes distributed evenly
      y_vals <- seq(
        from = -(n_nodes - 1) * node_spacing / 2,
        to = (n_nodes - 1) * node_spacing / 2,
        length.out = n_nodes
      )
      y_positions[all_nodes %in% nodes_at_depth] <- y_vals
    }
  }

  return(data.frame(
    node = all_nodes,
    x = x_positions,
    y = y_positions,
    stringsAsFactors = FALSE
  ))
}
