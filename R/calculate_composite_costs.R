#' Calculate Composite Costs from Cost Components
#'
#' @description
#' Calculates composite costs from specified cost components, base costs, and
#' edge properties. This function handles cost multipliers (duration, quantity),
#' optional time discounting, and aggregation of multiple cost components into
#' final composite cost values.
#'
#' @param cost_components A cost_components object created by
#'   \code{\link{specify_cost_components}}
#' @param base_costs A named list of primitive cost values. Names must match the
#'   \code{base_cost_name} values in \code{cost_components}
#' @param edge_properties A data frame with columns for edge properties. Must
#'   include \code{from_node} and \code{to_node} columns, plus columns matching
#'   the \code{multiplier_type} values used in \code{cost_components} (e.g.,
#'   "duration", "quantity"). Only required if \code{cost_components} uses
#'   NA multiplier_value entries.
#' @param discount_rate Optional numeric discount rate as a decimal (e.g., 0.03
#'   for 3%). If NULL (default), no discounting is applied. When provided,
#'   discounting is automatically applied to components with
#'   \code{multiplier_type = "duration"}
#'
#' @details
#' The function processes cost components in the following steps:
#' \enumerate{
#'   \item If \code{edge_properties} is provided, compute cumulative time offsets for
#'     each edge based on its position in the decision tree (using node 1 as the
#'     root at time 0). This ensures costs are discounted correctly based on when
#'     they occur in the tree's timeline.
#'   \item For each cost component, retrieve the base cost value from \code{base_costs}
#'   \item Determine the multiplier value:
#'     \itemize{
#'       \item If \code{multiplier_value} is not NA, use that value
#'       \item If \code{multiplier_value} is NA, lookup from \code{edge_properties}
#'         using \code{multiplier_type} as the column name and matching
#'         \code{edge_from} and \code{edge_to}
#'       \item If \code{multiplier_type = "none"}, use multiplier = 1
#'     }
#'   \item Apply discounting if:
#'     \itemize{
#'       \item \code{discount_rate} is provided (not NULL)
#'       \item AND \code{multiplier_type = "duration"}
#'       \item Discounting uses the edge's time offset to ensure costs are discounted
#'         based on their cumulative time from the root node
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
#' # Define edge properties
#' edge_props <- tibble::tribble(
#'   ~from_node, ~to_node, ~duration,
#'   7, 10, 1,
#'   10, 19, 4
#' )
#'
#' # Define cost components
#' cost_comps <- tibble::tribble(
#'   ~cost_label, ~base_cost_name, ~multiplier_type, ~multiplier_value, ~edge_from, ~edge_to,
#'   "c_biol_2_success", "c_biologic", "quantity", 1, 4, 7,
#'   "c_biol_2_success", "c_biologic", "quantity", 1, 7, 10,
#'   "c_biol_2_success", "c_fail_year", "duration", NA, 7, 10,
#'   "c_biol_2_success", "c_success_year", "duration", NA, 10, 19
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
#' }
#'
#' @seealso
#'   \code{\link{specify_cost_components}},
#'   \code{\link{apply_discount}},
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
  needs_edge_lookup <- any(is.na(cost_components$multiplier_value) &
                            cost_components$multiplier_type != "none")

  if (needs_edge_lookup && is.null(edge_properties)) {
    stop(
      "edge_properties required: some cost components have NA multiplier_value",
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

    # Check that required multiplier_type columns exist in edge_properties
    needed_types <- unique(cost_components$multiplier_type[
      is.na(cost_components$multiplier_value) &
      cost_components$multiplier_type != "none"
    ])

    missing_types <- setdiff(needed_types, names(edge_properties))
    if (length(missing_types) > 0) {
      stop(
        "Missing columns in edge_properties for multiplier lookup: ",
        paste(missing_types, collapse = ", "),
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

      # Determine multiplier value
      if (component$multiplier_type == "none") {
        multiplier <- 1
      } else if (!is.na(component$multiplier_value)) {
        multiplier <- component$multiplier_value
      } else {
        # Lookup from edge_properties
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

        multiplier_col <- component$multiplier_type
        multiplier <- edge_properties[[multiplier_col]][edge_match]

        if (is.na(multiplier)) {
          stop(
            sprintf(
              "NA value in edge_properties$%s for edge %d -> %d",
              multiplier_col,
              component$edge_from,
              component$edge_to
            ),
            call. = FALSE
          )
        }
      }

      # Calculate component cost
      if (component$multiplier_type == "duration" && !is.null(discount_rate)) {
        # Apply discounting for duration-based costs with time offset
        # Determine time offset for this edge
        if (!is.null(edge_properties)) {
          # Look up the time offset for this specific edge
          edge_match <- edge_properties$from_node == component$edge_from &
                        edge_properties$to_node == component$edge_to
          time_offset <- edge_properties$time_offset[edge_match]
        } else {
          # No edge_properties provided, use default offset=1 (old behavior)
          time_offset <- 1
        }

        component_cost <- apply_discount(base_cost_value, multiplier, discount_rate, time_offset)
      } else {
        # Simple multiplication (no discounting)
        component_cost <- base_cost_value * multiplier
      }

      # Add to total
      total_cost <- total_cost + component_cost
    }

    # Store result
    composite_costs[[label]] <- total_cost
  }

  return(composite_costs)
}