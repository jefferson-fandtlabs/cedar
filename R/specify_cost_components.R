#' Specify Cost Components for Composite Cost Calculations
#'
#' @description
#' Creates and validates a cost component specification data frame for use with
#' \code{\link{calculate_composite_costs}}. This function structures the
#' information needed to calculate composite costs from primitive cost values
#' and multipliers (duration, quantity, etc.).
#'
#' @param components A data frame or tibble with the following required columns:
#'   \describe{
#'     \item{cost_label}{Character. The name of the composite cost being defined}
#'     \item{base_cost_name}{Character. The name of the primitive cost to be multiplied}
#'     \item{multiplier_type}{Character. Type of multiplier: "duration", "quantity", or "none"}
#'     \item{edge_from}{Integer. The source node for this cost component}
#'     \item{edge_to}{Integer. The destination node for this cost component}
#'   }
#'   And one optional column:
#'   \describe{
#'     \item{multiplier_value}{Numeric. The multiplier value. Use NA to lookup
#'       from edge_properties based on multiplier_type}
#'   }
#'
#' @details
#' The \code{multiplier_type} determines how costs are calculated:
#' \itemize{
#'   \item \strong{"duration"}: Cost occurs over time. Use with time-based costs
#'     like yearly medical costs. If discounting is enabled, will be automatically
#'     applied for duration multipliers.
#'   \item \strong{"quantity"}: Cost occurs multiple times. Use for countable items
#'     like injections, surgeries, or doses.
#'   \item \strong{"none"}: No multiplier (multiplier = 1). Use for one-time costs.
#' }
#'
#' When \code{multiplier_value} is NA, the function will lookup the value from
#' the \code{edge_properties} parameter in \code{calculate_composite_costs()}.
#' The lookup uses the \code{multiplier_type} as the column name (e.g., "duration"
#' column for multiplier_type = "duration").
#'
#' @return A validated tibble with class "cost_components" containing the cost
#'   component specification
#'
#' @examples
#' \dontrun{
#' # Define components for a composite cost with biologic injections and yearly costs
#' components <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
#'   "c_biol_2_success", "c_biologic", "quantity", 1, 4, 7,
#'   "c_biol_2_success", "c_biologic", "quantity", 1, 7, 10,
#'   "c_biol_2_success", "c_fail_year", "duration", NA, 7, 10,  # Use duration from edge
#'   "c_biol_2_success", "c_success_year", "duration", NA, 10, 19  # Use duration from edge
#' )
#'
#' cost_spec <- specify_cost_components(components)
#' }
#'
#' @seealso \code{\link{calculate_composite_costs}}
#'
#' @export
specify_cost_components <- function(components) {

  # Convert to tibble if not already
  if (!inherits(components, "tbl_df")) {
    components <- tibble::as_tibble(components)
  }

  # Check required columns
  required_cols <- c("cost_label", "base_cost_name", "multiplier_type",
                     "edge_from", "edge_to")
  missing_cols <- setdiff(required_cols, names(components))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in cost components specification: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Add multiplier_value column if missing
  if (!"multiplier_value" %in% names(components)) {
    components$multiplier_value <- NA_real_
  }

  # Validate multiplier_type values
  valid_types <- c("duration", "quantity", "none")
  invalid_types <- setdiff(unique(components$multiplier_type), valid_types)

  if (length(invalid_types) > 0) {
    stop(
      "Invalid multiplier_type values: ",
      paste(invalid_types, collapse = ", "),
      "\nValid values are: ",
      paste(valid_types, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate that edge nodes are integers or can be coerced to integers
  if (!is.numeric(components$edge_from) || !is.numeric(components$edge_to)) {
    stop(
      "edge_from and edge_to must be numeric (node IDs)",
      call. = FALSE
    )
  }

  # Validate cost_label is character
  if (!is.character(components$cost_label)) {
    stop("cost_label must be character", call. = FALSE)
  }

  # Validate base_cost_name is character
  if (!is.character(components$base_cost_name)) {
    stop("base_cost_name must be character", call. = FALSE)
  }

  # Validate multiplier_value is numeric when present
  if (!is.numeric(components$multiplier_value) && !all(is.na(components$multiplier_value))) {
    stop("multiplier_value must be numeric or NA", call. = FALSE)
  }

  # Warning for multiplier_type = "none" with non-NA multiplier_value
  none_with_value <- components$multiplier_type == "none" &
    !is.na(components$multiplier_value) &
    components$multiplier_value != 1

  if (any(none_with_value)) {
    warning(
      "multiplier_type = 'none' but multiplier_value is not 1 or NA. ",
      "These values will be ignored and treated as 1.",
      call. = FALSE
    )
  }

  # Set class for method dispatch
  class(components) <- c("cost_components", class(components))

  return(components)
}


#' Print method for cost_components
#'
#' @param x A cost_components object
#' @param ... Additional arguments passed to print
#'
#' @export
print.cost_components <- function(x, ...) {
  cat("Cost Components Specification\n")
  cat("==============================\n\n")

  # Count unique cost labels
  n_costs <- length(unique(x$cost_label))
  n_components <- nrow(x)

  cat(sprintf("Number of composite costs: %d\n", n_costs))
  cat(sprintf("Total cost components: %d\n\n", n_components))

  # Print summary by cost_label
  cat("Components by cost label:\n")
  component_counts <- table(x$cost_label)
  for (label in names(component_counts)) {
    cat(sprintf("  %s: %d component(s)\n", label, component_counts[label]))
  }

  cat("\nFull specification:\n")
  NextMethod("print")
}