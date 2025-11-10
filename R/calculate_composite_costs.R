#' Calculate Composite Costs from Cost Components
#'
#' @description
#' Calculates composite costs from specified cost components, base costs, and
#' edge properties. This function handles cost multipliers (duration in various
#' time units, quantity), optional time discounting, and aggregation of multiple
#' cost components into final composite cost values.
#'
#' @param cost_components A cost_components object created by
#'   \code{\link{specify_cost_components}}
#' @param base_costs A named list of primitive cost values. Names must match the
#'   \code{base_cost_name} values in \code{cost_components}
#' @param edge_properties A data frame with columns for edge properties. Must
#'   include \code{from_node} and \code{to_node} columns. For duration-based
#'   costs with NA duration_value, must include columns matching the
#'   \code{duration_unit} values (e.g., "years", "months", "days"). For
#'   quantity-based costs with NA quantity_value, must include a "quantity"
#'   column. Only required if \code{cost_components} uses NA duration_value
#'   or quantity_value entries.
#' @param discount_rate Optional numeric discount rate as a decimal (e.g., 0.03
#'   for 3%). If NULL (default), no discounting is applied. When provided,
#'   discounting is automatically applied to duration-based components (after
#'   converting durations to years). Quantity-based components are never
#'   discounted.
#'
#' @details
#' The function processes cost components in the following steps:
#' \enumerate{
#'   \item If \code{edge_properties} is provided, compute cumulative time offsets for
#'     each edge based on its position in the decision tree (using node 1 as the
#'     root at time 0). This ensures costs are discounted correctly based on when
#'     they occur in the tree's timeline.
#'   \item For each cost component, retrieve the base cost value from \code{base_costs}
#'   \item Determine if component is duration-based or quantity-based:
#'     \describe{
#'       \item{Duration-based}{Has non-NA \code{duration_unit}. Used for time-based
#'         costs like annual medical costs or monthly medication.}
#'       \item{Quantity-based}{Has non-NA \code{quantity_value}. Used for countable
#'         items like injections, surgeries, or one-time costs.}
#'     }
#'   \item For duration-based costs:
#'     \itemize{
#'       \item If \code{duration_value} is not NA, use that value
#'       \item If \code{duration_value} is NA, lookup from \code{edge_properties}
#'         using \code{duration_unit} as the column name
#'       \item Convert duration to years using \code{\link{convert_duration_to_years}}
#'       \item Apply discounting if \code{discount_rate} is provided (uses edge's
#'         time offset)
#'     }
#'   \item For quantity-based costs:
#'     \itemize{
#'       \item If \code{quantity_value} is numeric, use that value
#'       \item If \code{quantity_value} is NA, lookup from \code{edge_properties$quantity}
#'       \item Simple multiplication (no time conversion or discounting)
#'     }
#'   \item Calculate component cost: \code{base_cost * multiplier * discount_factor}
#'   \item Sum all components for each unique \code{cost_label}
#' }
#'
#' Time offsets can be manually overridden by including a \code{time_offset} column
#' in \code{edge_properties}.
#'
#' @return A named list of calculated composite costs, ready to be used with
#'   \code{\link{build_decision_tree_table}}
#'
#' @examples
#' \dontrun{
#' # Define base costs
#' base_costs <- list(
#'   c_biologic = 30000,
#'   c_fail_year = 5000,
#'   c_success_year = 1000
#' )
#'
#' # Define edge properties with multiple time unit columns
#' edge_props <- tibble::tribble(
#'   ~from_node, ~to_node, ~years, ~months, ~quantity,
#'   4, 7, NA, NA, 1,
#'   7, 10, 1, 12, 1,
#'   10, 19, 4, 48, NA
#' )
#'
#' # Define cost components with mixed duration and quantity
#' cost_comps <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
#'   "c_biol_2_success", "c_biologic", NA, NA, 1, 4, 7,       # Quantity: 1 injection
#'   "c_biol_2_success", "c_biologic", NA, NA, 1, 7, 10,      # Quantity: 1 injection
#'   "c_biol_2_success", "c_fail_year", "years", NA, NA, 7, 10,    # Duration: lookup years
#'   "c_biol_2_success", "c_success_year", "years", NA, NA, 10, 19  # Duration: lookup years
#' ) %>%
#'   specify_cost_components()
#'
#' # Calculate costs with discounting
#' costs <- calculate_composite_costs(
#'   cost_comps,
#'   base_costs,
#'   edge_props,
#'   discount_rate = 0.03
#' )
#'
#' # Calculate costs without discounting
#' costs_no_disc <- calculate_composite_costs(
#'   cost_comps,
#'   base_costs,
#'   edge_props,
#'   discount_rate = NULL
#' )
#'
#' # Example with multiple time units
#' cost_comps_mixed <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~duration_unit, ~duration_value, ~quantity_value, ~edge_from, ~edge_to,
#'   "c_treatment", "c_daily_med", "days", 30, NA, 1, 2,
#'   "c_treatment", "c_monthly_visit", "months", 6, NA, 2, 3,
#'   "c_treatment", "c_annual_followup", "years", 2, NA, 3, 4
#' ) %>%
#'   specify_cost_components()
#' }
#'
#' @seealso
#'   \code{\link{specify_cost_components}},
#'   \code{\link{apply_discount}},
#'   \code{\link{convert_duration_to_years}},
#'   \code{\link{compute_edge_offsets}},
#'   \code{\link{build_decision_tree_table}}
#'
#' @export
calculate_composite_costs <- function(cost_components,
                                      base_costs,
                                      edge_properties = NULL,
                                      discount_rate = NULL) {

  # Validate inputs
  if (!inherits(cost_components, "cost_components")) {
    stop(
      "cost_components must be created using specify_cost_components()",
      call. = FALSE
    )
  }

  if (!is.list(base_costs) || is.null(names(base_costs))) {
    stop(
      "base_costs must be a named list",
      call. = FALSE
    )
  }

  if (!is.null(discount_rate)) {
    if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
      stop("discount_rate must be a single numeric value or NULL", call. = FALSE)
    }
    if (discount_rate < 0) {
      stop("discount_rate must be non-negative", call. = FALSE)
    }
  }

  # Check that all base_cost_names exist in base_costs
  missing_costs <- setdiff(cost_components$base_cost_name, names(base_costs))
  if (length(missing_costs) > 0) {
    stop(
      "Missing base costs: ",
      paste(missing_costs, collapse = ", "),
      "\nAvailable base costs: ",
      paste(names(base_costs), collapse = ", "),
      call. = FALSE
    )
  }

  # Check if edge_properties needed and provided
  needs_duration_lookup <- any(!is.na(cost_components$duration_unit) &
                                 is.na(cost_components$duration_value))
  needs_quantity_lookup <- any(!is.na(cost_components$quantity_value) &
                                 is.na(cost_components$quantity_value) == FALSE &
                                 is.numeric(cost_components$quantity_value) == FALSE) ||
                            any(is.na(cost_components$quantity_value) &
                                 !is.na(cost_components$quantity_value) == TRUE &
                                 cost_components$quantity_value != cost_components$quantity_value)

  # Simpler check: need edge lookup if any duration_value or quantity_value is truly NA
  # (not just checking the column type)
  needs_edge_lookup <- FALSE
  for (i in seq_len(nrow(cost_components))) {
    if (!is.na(cost_components$duration_unit[i]) && is.na(cost_components$duration_value[i])) {
      needs_edge_lookup <- TRUE
      break
    }
    if (is.na(cost_components$duration_unit[i]) && is.na(cost_components$quantity_value[i])) {
      needs_edge_lookup <- TRUE
      break
    }
  }

  if (needs_edge_lookup && is.null(edge_properties)) {
    stop(
      "edge_properties required: some cost components have NA duration_value or quantity_value",
      call. = FALSE
    )
  }

  # Validate edge_properties structure if provided
  if (!is.null(edge_properties)) {
    if (!all(c("from_node", "to_node") %in% names(edge_properties))) {
      stop(
        "edge_properties must contain 'from_node' and 'to_node' columns",
        call. = FALSE
      )
    }

    # Check that required duration_unit columns exist in edge_properties
    needed_duration_units <- unique(cost_components$duration_unit[
      !is.na(cost_components$duration_unit) &
      is.na(cost_components$duration_value)
    ])

    if (length(needed_duration_units) > 0) {
      missing_duration_cols <- setdiff(needed_duration_units, names(edge_properties))
      if (length(missing_duration_cols) > 0) {
        stop(
          "Missing columns in edge_properties for duration lookup: ",
          paste(missing_duration_cols, collapse = ", "),
          "\nRequired because some components use these duration_unit values with NA duration_value.",
          call. = FALSE
        )
      }
    }

    # Check if quantity column needed
    needs_quantity_col <- any(is.na(cost_components$duration_unit) &
                               is.na(cost_components$quantity_value))

    if (needs_quantity_col && !"quantity" %in% names(edge_properties)) {
      stop(
        "Missing 'quantity' column in edge_properties. ",
        "Required because some components have NA quantity_value.",
        call. = FALSE
      )
    }
  }

  # Compute time offsets for edges if edge_properties is provided
  # This ensures correct sequential discounting along paths in the decision tree
  if (!is.null(edge_properties)) {
    edge_properties <- compute_edge_offsets(edge_properties)
  }

  # Initialize results list
  composite_costs <- list()

  # Get unique cost labels
  cost_labels <- unique(cost_components$cost_label)

  # Process each cost label
  for (label in cost_labels) {

    # Get all components for this cost label
    label_components <- cost_components[cost_components$cost_label == label, ]

    # Initialize total cost for this label
    total_cost <- 0

    # Process each component
    for (i in seq_len(nrow(label_components))) {
      component <- label_components[i, ]

      # Get base cost value
      base_cost_value <- base_costs[[component$base_cost_name]]

      # Determine if duration-based or quantity-based
      # Key: duration_unit presence determines the cost type
      is_duration <- !is.na(component$duration_unit)
      is_quantity <- is.na(component$duration_unit)  # Quantity-based when duration_unit is NA

      if (is_duration) {
        # DURATION-BASED COST
        # -----------------

        # Get duration value
        if (!is.na(component$duration_value)) {
          # Use explicit duration value
          duration_in_units <- component$duration_value
        } else {
          # Lookup from edge_properties using duration_unit as column name
          edge_match <- edge_properties$from_node == component$edge_from &
                        edge_properties$to_node == component$edge_to

          if (!any(edge_match)) {
            stop(
              sprintf(
                "No edge property found for edge %d -> %d (cost_label: %s, base_cost: %s)",
                component$edge_from,
                component$edge_to,
                component$cost_label,
                component$base_cost_name
              ),
              call. = FALSE
            )
          }

          if (sum(edge_match) > 1) {
            stop(
              sprintf(
                "Multiple edge properties found for edge %d -> %d",
                component$edge_from,
                component$edge_to
              ),
              call. = FALSE
            )
          }

          duration_col <- component$duration_unit
          duration_in_units <- edge_properties[[duration_col]][edge_match]

          if (is.na(duration_in_units)) {
            stop(
              sprintf(
                "NA value in edge_properties$%s for edge %d -> %d",
                duration_col,
                component$edge_from,
                component$edge_to
              ),
              call. = FALSE
            )
          }
        }

        # Convert duration to years
        years_duration <- convert_duration_to_years(
          duration_in_units,
          component$duration_unit
        )

        # Apply discounting if discount_rate provided
        if (!is.null(discount_rate)) {
          # Get time offset for this edge
          if (!is.null(edge_properties)) {
            edge_match <- edge_properties$from_node == component$edge_from &
                          edge_properties$to_node == component$edge_to
            time_offset <- edge_properties$time_offset[edge_match]
          } else {
            # No edge_properties, use default offset = 1
            time_offset <- 1
          }

          component_cost <- apply_discount(
            base_cost_value,
            years_duration,
            discount_rate,
            time_offset
          )
        } else {
          # No discounting
          component_cost <- base_cost_value * years_duration
        }

      } else if (is_quantity) {
        # QUANTITY-BASED COST
        # ------------------

        # Get quantity value
        if (!is.na(component$quantity_value)) {
          # Use explicit quantity value
          quantity <- component$quantity_value
        } else {
          # Lookup from edge_properties$quantity
          edge_match <- edge_properties$from_node == component$edge_from &
                        edge_properties$to_node == component$edge_to

          if (!any(edge_match)) {
            stop(
              sprintf(
                "No edge property found for edge %d -> %d (cost_label: %s, base_cost: %s)",
                component$edge_from,
                component$edge_to,
                component$cost_label,
                component$base_cost_name
              ),
              call. = FALSE
            )
          }

          if (sum(edge_match) > 1) {
            stop(
              sprintf(
                "Multiple edge properties found for edge %d -> %d",
                component$edge_from,
                component$edge_to
              ),
              call. = FALSE
            )
          }

          quantity <- edge_properties$quantity[edge_match]

          if (is.na(quantity)) {
            stop(
              sprintf(
                "NA value in edge_properties$quantity for edge %d -> %d",
                component$edge_from,
                component$edge_to
              ),
              call. = FALSE
            )
          }
        }

        # Simple multiplication (no discounting for quantities)
        component_cost <- base_cost_value * quantity

      } else {
        # This should never happen due to validation in specify_cost_components
        stop(
          sprintf(
            "Component has neither duration_unit nor quantity_value set (cost_label: %s, edge %d -> %d)",
            component$cost_label,
            component$edge_from,
            component$edge_to
          ),
          call. = FALSE
        )
      }

      # Add to total
      total_cost <- total_cost + component_cost
    }

    # Store result
    composite_costs[[label]] <- total_cost
  }

  return(composite_costs)
}
