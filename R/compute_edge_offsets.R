#' Compute Time Offsets for Edges in a Decision Tree
#'
#' @description
#' Calculates cumulative time offsets for each edge in a decision tree, starting
#' from the root node (node 1) at time t=0. The offset represents when each edge's
#' time period begins in the overall timeline.
#'
#' @param edge_properties A data frame containing edge definitions with columns:
#'   \itemize{
#'     \item \code{from_node}: Starting node ID
#'     \item \code{to_node}: Ending node ID
#'     \item \code{duration}: Optional duration of the edge (can be NA or missing)
#'     \item \code{time_offset}: Optional manual offset override
#'   }
#'
#' @details
#' The function traces paths from the root node (node 1) through the tree,
#' accumulating time as it goes. For each edge:
#' \itemize{
#'   \item The offset = cumulative time at the from_node
#'   \item The cumulative time at to_node = cumulative time at from_node + edge duration
#'   \item NA or missing durations are treated as 0
#'   \item If time_offset is already present in edge_properties, it is preserved
#' }
#'
#' @return A data frame identical to edge_properties but with an added (or updated)
#'   \code{time_offset} column containing the computed cumulative time offsets.
#'
#' @examples
#' # Simple sequential chain
#' edges <- data.frame(
#'   from_node = c(1, 4, 7, 11),
#'   to_node = c(4, 7, 11, 13),
#'   duration = c(0, 1, 1, 1)
#' )
#' compute_edge_offsets(edges)
#' # Returns offsets: [0, 0, 1, 2]
#'
#' @export
compute_edge_offsets <- function(edge_properties) {
  # Validate input
  if (!is.data.frame(edge_properties)) {
    stop("edge_properties must be a data frame")
  }

  if (!all(c("from_node", "to_node") %in% names(edge_properties))) {
    stop("edge_properties must contain 'from_node' and 'to_node' columns")
  }

  # If time_offset already exists, preserve it and return
  if ("time_offset" %in% names(edge_properties)) {
    message("edge_properties already contains time_offset column; preserving existing values")
    return(edge_properties)
  }

  # If no duration column or all NA, set all offsets to 0
  if (!("duration" %in% names(edge_properties)) ||
      all(is.na(edge_properties$duration))) {
    edge_properties$time_offset <- 0
    return(edge_properties)
  }

  # Initialize cumulative time tracker for each node
  # Start with node 1 (root) at time 0
  node_times <- list()
  node_times[[as.character(1)]] <- 0

  # Make a working copy with row numbers for tracking
  edges_working <- edge_properties
  edges_working$row_id <- seq_len(nrow(edges_working))
  edges_working$time_offset <- NA_real_
  edges_working$processed <- FALSE

  # Treat NA durations as 0
  edges_working$duration[is.na(edges_working$duration)] <- 0

  # Process edges iteratively until all are processed
  # In each iteration, process edges whose from_node has a known time
  max_iterations <- nrow(edges_working) * 2  # Safeguard against infinite loops
  iteration <- 0

  while (any(!edges_working$processed) && iteration < max_iterations) {
    iteration <- iteration + 1
    made_progress <- FALSE

    for (i in seq_len(nrow(edges_working))) {
      if (edges_working$processed[i]) {
        next
      }

      from_node_key <- as.character(edges_working$from_node[i])
      to_node_key <- as.character(edges_working$to_node[i])

      # Check if we know the cumulative time at the from_node
      if (from_node_key %in% names(node_times)) {
        # Set the edge's offset to the cumulative time at from_node
        edges_working$time_offset[i] <- node_times[[from_node_key]]

        # Update cumulative time at to_node
        new_time <- node_times[[from_node_key]] + edges_working$duration[i]

        # If to_node already has a time, verify consistency
        if (to_node_key %in% names(node_times)) {
          existing_time <- node_times[[to_node_key]]
          if (abs(existing_time - new_time) > 1e-10) {
            warning(sprintf(
              "Inconsistent times for node %s: existing=%g, new=%g (edge %d->%d)",
              to_node_key, existing_time, new_time,
              edges_working$from_node[i], edges_working$to_node[i]
            ))
          }
        } else {
          node_times[[to_node_key]] <- new_time
        }

        edges_working$processed[i] <- TRUE
        made_progress <- TRUE
      }
    }

    # If we didn't process any edges in this iteration, we might have orphaned edges
    if (!made_progress) {
      unprocessed_edges <- edges_working[!edges_working$processed, ]
      if (nrow(unprocessed_edges) > 0) {
        orphan_nodes <- unique(unprocessed_edges$from_node)
        warning(sprintf(
          "Cannot compute offsets for some edges. Orphaned from_nodes: %s. Setting their offsets to 0.",
          paste(orphan_nodes, collapse = ", ")
        ))
        # Set remaining edges to offset 0
        edges_working$time_offset[!edges_working$processed] <- 0
        edges_working$processed[!edges_working$processed] <- TRUE
        break
      }
    }
  }

  if (iteration >= max_iterations) {
    stop("Maximum iterations reached. Possible cycle in edge structure.")
  }

  # Restore original duration values (convert 0s back to NA where appropriate)
  if ("duration" %in% names(edge_properties)) {
    edges_working$duration <- edge_properties$duration
  }

  # Return the result with time_offset column added
  edges_working$row_id <- NULL
  edges_working$processed <- NULL

  return(edges_working)
}