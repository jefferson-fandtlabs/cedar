#' Specify Cost Components for Composite Cost Calculations
#'
#' @description
#' Creates and validates a cost component specification data frame for use with
#' \code{\link{calculate_composite_costs}}. This function structures the
#' information needed to calculate composite costs from primitive cost values
#' and multipliers (duration or quantity).
#'
#' @param components A data frame or tibble with the following required columns:
#'   \describe{
#'     \item{cost_label}{Character. The name of the composite cost being defined}
#'     \item{base_cost_name}{Character. The name of the primitive cost to be multiplied}
#'     \item{edge_from}{Integer. The source node for this cost component}
#'     \item{edge_to}{Integer. The destination node for this cost component}
#'   }
#'   And columns for specifying the multiplier (must have EITHER duration OR quantity):
#'   \describe{
#'     \item{duration_unit}{Character. For duration-based costs, specifies the time
#'       unit: "hours", "days", "weeks", "months", or "years". Use NA for quantity-based costs.}
#'     \item{duration_value}{Numeric. For duration-based costs, the duration in the
#'       units specified by duration_unit. Use NA to lookup from edge_properties
#'       using duration_unit as the column name. Must be NA for quantity-based costs.}
#'     \item{quantity_value}{Numeric. For quantity-based costs, the count or multiplier
#'       (e.g., number of injections, surgeries). Use NA to lookup from edge_properties
#'       $quantity column. Must be NA for duration-based costs.}
#'   }
#'
#' @details
#' Each cost component must specify EITHER a duration OR a quantity, never both:
#'
#' \strong{Duration-based costs:}
#' \itemize{
#'   \item Set \code{duration_unit} to one of: "hours", "days", "weeks", "months", "years"
#'   \item Set \code{duration_value} to a numeric value OR NA to lookup from edge_properties
#'   \item Set \code{quantity_value} to NA
#'   \item Used for time-based costs like annual medical costs, monthly medication
#'   \item Automatically converted to years for discounting calculations
#'   \item Discounting is applied if discount_rate is provided to calculate_composite_costs
#' }
#'
#' \strong{Quantity-based costs:}
#' \itemize{
#'   \item Set \code{quantity_value} to a numeric value OR NA to lookup from edge_properties
#'   \item Set \code{duration_unit} and \code{duration_value} to NA
#'   \item Used for countable items like injections, surgeries, or procedures
#'   \item No time conversion or discounting is applied
#'   \item One-time costs use quantity_value = 1
#' }
#'
#' When \code{duration_value} or \code{quantity_value} is NA, the value will be
#' looked up from the \code{edge_properties} parameter in
#' \code{calculate_composite_costs()}. For duration, the lookup uses
#' \code{duration_unit} as the column name (e.g., edge_properties$months for
#' duration_unit = "months"). For quantity, it uses edge_properties$quantity.
#'
#' @return A validated tibble with class "cost_components" containing the cost
#'   component specification
#'
#' @examples
#' \dontrun{
#' # Duration-based costs with edge property lookup
#' components1 <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
#'   "c_treatment", "c_annual", "years", NA, NA, 7, 10,
#'   "c_treatment", "c_annual", "years", NA, NA, 10, 19
#' ) %>%
#'   specify_cost_components()
#'
#' # Mixed duration and quantity costs
#' components2 <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
#'   "c_biol_treatment", "c_injection", NA, NA, 2, 4, 7,        # 2 injections
#'   "c_biol_treatment", "c_annual", "years", NA, NA, 7, 10,    # Lookup years
#'   "c_biol_treatment", "c_monthly", "months", 48, NA, 10, 19  # 48 months explicit
#' ) %>%
#'   specify_cost_components()
#'
#' # One-time cost (using quantity)
#' components3 <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
#'   "c_surgery", "c_surgery_base", NA, NA, 1, 3, 4
#' ) %>%
#'   specify_cost_components()
#' }
#'
#' @seealso \code{\link{calculate_composite_costs}}, \code{\link{convert_duration_to_years}}
#'
#' @export
specify_cost_components <- function(components) {

  # Convert to tibble if not already
  if (!inherits(components, "tbl_df")) {
    components <- tibble::as_tibble(components)
  }

  # Check required columns
  required_cols <- c("cost_label", "base_cost_name", "edge_from", "edge_to")
  missing_cols <- setdiff(required_cols, names(components))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in cost components specification: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Add optional columns if missing
  if (!"duration_unit" %in% names(components)) {
    components$duration_unit <- NA_character_
  }
  if (!"duration_value" %in% names(components)) {
    components$duration_value <- NA_real_
  }
  if (!"quantity_value" %in% names(components)) {
    components$quantity_value <- NA_real_
  }

  # Validate that edge nodes are numeric
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

  # Validate duration_unit values
  valid_duration_units <- c("hours", "days", "weeks", "months", "years")
  has_duration_unit <- !is.na(components$duration_unit)

  if (any(has_duration_unit)) {
    invalid_units <- setdiff(
      unique(components$duration_unit[has_duration_unit]),
      valid_duration_units
    )

    if (length(invalid_units) > 0) {
      stop(
        "Invalid duration_unit values: ",
        paste(invalid_units, collapse = ", "),
        "\nValid values are: ",
        paste(valid_duration_units, collapse = ", "),
        call. = FALSE
      )
    }
  }

  # Validate that each row has EITHER duration OR quantity, not both
  # The key distinction: duration_unit presence determines the cost type
  # - If duration_unit is NOT NA: duration-based cost (duration_value can be NA for lookup)
  # - If duration_unit IS NA: quantity-based cost (quantity_value can be NA for lookup)
  for (i in seq_len(nrow(components))) {
    row <- components[i, ]
    is_duration <- !is.na(row$duration_unit)
    is_quantity <- is.na(row$duration_unit)  # Quantity-based when duration_unit is NA

    # Validate duration-based costs
    if (is_duration) {
      # quantity_value must be NA for duration-based costs
      if (!is.na(row$quantity_value)) {
        stop(
          sprintf(
            "Row %d (cost_label: %s, base_cost: %s, edge %d->%d): ",
            i, row$cost_label, row$base_cost_name, row$edge_from, row$edge_to
          ),
          "quantity_value must be NA when duration_unit is specified.",
          call. = FALSE
        )
      }
    }

    # Validate quantity-based costs (duration_unit is NA)
    if (is_quantity) {
      # duration_value must also be NA for quantity-based costs
      if (!is.na(row$duration_value)) {
        stop(
          sprintf(
            "Row %d (cost_label: %s, base_cost: %s, edge %d->%d): ",
            i, row$cost_label, row$base_cost_name, row$edge_from, row$edge_to
          ),
          "duration_value must be NA when duration_unit is NA (quantity-based cost).",
          call. = FALSE
        )
      }
    }
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

  # Count duration vs quantity components
  n_duration <- sum(!is.na(x$duration_unit))
  n_quantity <- sum(!is.na(x$quantity_value))

  cat(sprintf("Duration-based components: %d\n", n_duration))
  cat(sprintf("Quantity-based components: %d\n\n", n_quantity))

  # Print summary by cost_label
  cat("Components by cost label:\n")
  for (label in unique(x$cost_label)) {
    label_components <- x[x$cost_label == label, ]
    n_dur <- sum(!is.na(label_components$duration_unit))
    n_qty <- sum(!is.na(label_components$quantity_value))

    cat(sprintf("  %s: %d component(s) (%d duration, %d quantity)\n",
                label, nrow(label_components), n_dur, n_qty))

    # Show time units used for duration components
    if (n_dur > 0) {
      units_used <- unique(label_components$duration_unit[!is.na(label_components$duration_unit)])
      cat(sprintf("    Duration units: %s\n", paste(units_used, collapse = ", ")))
    }
  }

  cat("\nFull specification:\n")
  NextMethod("print")
}
