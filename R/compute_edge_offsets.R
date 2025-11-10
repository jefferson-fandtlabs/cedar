#' Compute Time Offsets for Edges in a Decision Tree
#'
#' @description
#' Calculates cumulative time offsets for each edge in a decision tree, starting
#' from the root node (node 1) at time t=0. The offset represents when each edge's
#' time period begins in the overall timeline, which is critical for proper
#' discounting calculations.
#'
#' @param edge_properties A data frame containing edge definitions with columns:
#'   \itemize{
#'     \item \code{from_node}: Starting node ID (required)
#'     \item \code{to_node}: Ending node ID (required)
#'     \item Duration column: One or more columns containing duration values (e.g.,
#'       "years", "months", "days", "weeks", "hours"). Used to calculate time offsets.
#'     \item \code{time_offset}: Optional manual offset override. If present, existing
#'       values are preserved.
#'   }
#' @param duration_column Character string specifying which column to use for
#'   duration values (e.g., "years", "months", "days"). If NULL (default), the
#'   function auto-detects in this priority order: "years", "months", "weeks",
#'   "days", "hours". If no duration columns are found, all offsets are set to 0
#'   with a warning.
#'
#' @details
#' The function traces paths from the root node (node 1) through the tree,
#' accumulating time as it goes. For each edge:
#' \itemize{
#'   \item The offset = cumulative time at the from_node (in years)
#'   \item The cumulative time at to_node = cumulative time at from_node + edge duration (in years)
#'   \item NA or missing durations are treated as 0
#'   \item If time_offset is already present in edge_properties, it is preserved
#'   \item Duration values are automatically converted to years for consistent offset calculation
#' }
#'
#' **Duration Column Selection:**
#'
#' If \code{duration_column} is not specified, the function searches for duration
#' columns in this order:
#' \enumerate{
#'   \item "years" - Standard for discounting calculations
#'   \item "months" - Converted to years (month / 12)
#'   \item "weeks" - Converted to years (week / 52.1775)
#'   \item "days" - Converted to years (day / 365.25)
#'   \item "hours" - Converted to years (hour / 8766)
#' }
#'
#' If multiple duration columns exist, specifying \code{duration_column} allows
#' you to control which one is used for offset calculation.
#'
#' @return A data frame identical to edge_properties but with an added (or updated)
#'   \code{time_offset} column containing the computed cumulative time offsets in years.
#'
#' @examples
#' # Example 1: Using "years" column (auto-detected)
#' edges <- data.frame(
#'   from_node = c(1, 4, 7, 11),
#'   to_node = c(4, 7, 11, 13),
#'   years = c(0, 1, 1, 1)
#' )
#' compute_edge_offsets(edges)
#' # Returns offsets: [0, 0, 1, 2]
#'
#' # Example 2: Using "months" column with explicit specification
#' edges_months <- data.frame(
#'   from_node = c(1, 2, 3),
#'   to_node = c(2, 3, 4),
#'   months = c(0, 6, 12)
#' )
#' compute_edge_offsets(edges_months, duration_column = "months")
#' # Returns offsets in years: [0, 0, 0.5]
#'
#' # Example 3: Multiple duration columns - specify which to use
#' edges_mixed <- data.frame(
#'   from_node = c(1, 2),
#'   to_node = c(2, 3),
#'   years = c(1, 2),
#'   months = c(12, 24)
#' )
#' compute_edge_offsets(edges_mixed, duration_column = "years")
#' # Uses "years" column for offsets
#'
#' @export
compute_edge_offsets <- function(edge_properties, duration_column = NULL) {
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

  # Determine which duration column to use
  if (is.null(duration_column)) {
    # Auto-detect duration column in priority order
    possible_duration_cols <- c("years", "months", "weeks", "days", "hours")
    available_duration_cols <- possible_duration_cols[possible_duration_cols %in% names(edge_properties)]

    if (length(available_duration_cols) == 0) {
      # No duration columns found - set all offsets to 0 and warn
      warning(
        "No duration columns found in edge_properties (checked: ",
        paste(possible_duration_cols, collapse = ", "),
        "). Setting all time_offset values to 0.",
        call. = FALSE
      )
      edge_properties$time_offset <- 0
      return(edge_properties)
    }

    # Use the first available duration column (highest priority)
    duration_column <- available_duration_cols[1]
    message(
      "Using '", duration_column, "' column for time offset calculation",
      if (length(available_duration_cols) > 1) {
        paste0(" (other available: ", paste(available_duration_cols[-1], collapse = ", "), ")")
      } else {
        ""
      }
    )
  } else {
    # Validate explicitly specified duration_column
    if (!is.character(duration_column) || length(duration_column) != 1) {
      stop("duration_column must be a single character string", call. = FALSE)
    }

    if (!duration_column %in% names(edge_properties)) {
      stop(
        "Specified duration_column '", duration_column, "' not found in edge_properties. ",
        "Available columns: ", paste(names(edge_properties), collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Check if all durations are NA
  duration_values <- edge_properties[[duration_column]]
  if (all(is.na(duration_values))) {
    warning(
      "All values in '", duration_column, "' column are NA. ",
      "Setting all time_offset values to 0.",
      call. = FALSE
    )
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

  # Convert duration values to years for consistent offset calculation
  # Use convert_duration_to_years() function
  edges_working$duration_in_years <- sapply(seq_len(nrow(edges_working)), function(i) {
    dur_value <- edges_working[[duration_column]][i]
    if (is.na(dur_value)) {
      return(0)  # Treat NA as 0
    }
    # Convert to years based on duration_column unit
    convert_duration_to_years(dur_value, duration_column)
  })

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

        # Update cumulative time at to_node (using duration_in_years)
        new_time <- node_times[[from_node_key]] + edges_working$duration_in_years[i]

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

  # Return the result with time_offset column added
  # Remove working columns
  edges_working$row_id <- NULL
  edges_working$processed <- NULL
  edges_working$duration_in_years <- NULL

  return(edges_working)
}