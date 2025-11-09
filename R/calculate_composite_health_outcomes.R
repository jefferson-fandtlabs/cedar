#' Calculate Composite Health Outcomes
#'
#' @description
#' Calculates composite health outcomes by combining base outcome values with
#' durations from edge properties. Supports different outcome types including
#' utility-to-QALY conversion, life years, and direct QALY values.
#'
#' @param outcome_components A health_outcome_components object created by
#'   \code{\link{specify_health_outcome_components}}. Defines how to calculate
#'   each composite health outcome from base outcome values.
#' @param base_outcomes A named list of base health outcome values. Names must
#'   match the base_outcome_name values in outcome_components. Values are numeric
#'   and represent:
#'   \itemize{
#'     \item For "utility_to_qaly": Health utility values (typically 0-1)
#'     \item For "life_years": Life year multipliers (typically 1, but can be
#'       adjusted for quality or other factors)
#'     \item For "qaly_direct": Pre-calculated QALY values
#'   }
#' @param edge_properties Optional data frame or tibble containing edge-specific
#'   properties including duration values. Must contain columns: from_node, to_node,
#'   plus columns matching the duration_unit values used in outcome_components
#'   (e.g., "days", "months", "years"). Only needed if outcome_components has
#'   NA values in duration_value column.
#'
#' @return A named list where each name is an outcome_label and each value is
#'   the calculated composite health outcome value. For QALY calculations, values
#'   are in QALYs. For life years, values are in years.
#'
#' @details
#' The calculation process:
#' \enumerate{
#'   \item For each outcome_label in outcome_components:
#'   \itemize{
#'     \item Retrieve all components associated with that outcome_label
#'     \item For each component:
#'     \itemize{
#'       \item Get base outcome value from base_outcomes
#'       \item Determine duration value (from component's duration_value or
#'         edge_properties lookup)
#'       \item Convert duration to years using \code{\link{convert_duration_to_years}}
#'       \item Calculate component outcome based on outcome_type:
#'       \itemize{
#'         \item \strong{utility_to_qaly}: base_outcome * years_duration = QALYs
#'         \item \strong{life_years}: base_outcome * years_duration = life years
#'         \item \strong{qaly_direct}: base_outcome (duration not used)
#'       }
#'       \item Sum all components for the outcome_label
#'     }
#'   }
#'   \item Return named list of outcome_label -> total_outcome
#' }
#'
#' \strong{Outcome Type Details:}
#'
#' \emph{utility_to_qaly:}
#' Converts health utility values to QALYs by multiplying utility by duration
#' in years. For example:
#' \itemize{
#'   \item Utility = 0.8 (health state utility)
#'   \item Duration = 2 years
#'   \item QALY = 0.8 * 2 = 1.6 QALYs
#' }
#'
#' \emph{life_years:}
#' Calculates life years by multiplying a base value (typically 1) by duration
#' in years. The base value can be adjusted for quality factors. For example:
#' \itemize{
#'   \item Base = 1.0 (full life years)
#'   \item Duration = 5 years
#'   \item Life years = 1.0 * 5 = 5 years
#' }
#'
#' \emph{qaly_direct:}
#' Uses pre-calculated QALY values directly without duration multiplication.
#' Useful when QALYs have been calculated externally or come from literature.
#'
#' \strong{Duration Handling:}
#'
#' Durations can be specified in two ways:
#' \enumerate{
#'   \item \strong{Explicit}: Set duration_value in outcome_components
#'   \item \strong{Edge lookup}: Leave duration_value as NA and provide
#'     edge_properties with columns matching duration_unit names
#' }
#'
#' All durations are converted to years using 365.25 days per year to account
#' for leap years, which is the standard in epidemiology and health economics.
#'
#' @examples
#' library(tibble)
#'
#' # Example 1: Utility to QALY conversion
#' # Define base utilities
#' base_outcomes <- list(
#'   utility_success = 0.85,
#'   utility_failure = 0.65
#' )
#'
#' # Specify outcome components with explicit durations
#' outcome_components <- tribble(
#'   ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
#'   "qaly_success", "utility_success", "utility_to_qaly", "years", 2, 4, 5,
#'   "qaly_failure", "utility_failure", "utility_to_qaly", "months", 2, 5, 6
#' ) |>
#'   specify_health_outcome_components()
#'
#' # Calculate composite outcomes
#' composite_outcomes <- calculate_composite_health_outcomes(
#'   outcome_components,
#'   base_outcomes
#' )
#'
#' # Results:
#' # qaly_success = 0.85 * 5 = 4.25 QALYs
#' # qaly_failure = 0.65 * 0.5 = 0.325 QALYs (6 months = 0.5 years)
#'
#' # Example 2: Using edge properties for duration lookup
#' edge_props <- tribble(
#'   ~from_node, ~to_node, ~years, ~months,
#'   2, 4, 5, NA,
#'   2, 5, NA, 6
#' )
#'
#' outcome_components_lookup <- tribble(
#'   ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to,
#'   "qaly_success", "utility_success", "utility_to_qaly", "years", 2, 4,
#'   "qaly_failure", "utility_failure", "utility_to_qaly", "months", 2, 5
#' ) |>
#'   specify_health_outcome_components()
#'
#' composite_outcomes_lookup <- calculate_composite_health_outcomes(
#'   outcome_components_lookup,
#'   base_outcomes,
#'   edge_props
#' )
#'
#' # Example 3: Mixed outcome types
#' mixed_base_outcomes <- list(
#'   utility_treatment = 0.80,
#'   ly_multiplier = 1.0,
#'   precalc_qaly = 2.5
#' )
#'
#' mixed_components <- tribble(
#'   ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
#'   "total_qaly", "utility_treatment", "utility_to_qaly", "years", 1, 2, 3,
#'   "total_qaly", "precalc_qaly", "qaly_direct", "years", 2, 3, NA,
#'   "life_years", "ly_multiplier", "life_years", "years", 1, 2, 10
#' ) |>
#'   specify_health_outcome_components()
#'
#' mixed_outcomes <- calculate_composite_health_outcomes(
#'   mixed_components,
#'   mixed_base_outcomes
#' )
#'
#' # Results:
#' # total_qaly = (0.80 * 3) + 2.5 = 4.9 QALYs
#' # life_years = 1.0 * 10 = 10 years
#'
#' @seealso
#' \code{\link{specify_health_outcome_components}} for creating valid component specifications,
#' \code{\link{convert_duration_to_years}} for duration conversion details,
#' \code{\link{build_decision_tree_table}} for integration with decision trees
#'
#' @export
calculate_composite_health_outcomes <- function(outcome_components,
                                                base_outcomes,
                                                edge_properties = NULL) {

  # Input validation
  if (!inherits(outcome_components, "health_outcome_components")) {
    stop(
      "outcome_components must be created using specify_health_outcome_components()",
      call. = FALSE
    )
  }

  if (!is.list(base_outcomes)) {
    stop("base_outcomes must be a named list", call. = FALSE)
  }

  if (length(base_outcomes) == 0) {
    stop("base_outcomes cannot be empty", call. = FALSE)
  }

  if (is.null(names(base_outcomes)) || any(names(base_outcomes) == "")) {
    stop("All elements in base_outcomes must be named", call. = FALSE)
  }

  # Check that all referenced base outcomes exist
  referenced_outcomes <- unique(outcome_components$base_outcome_name)
  missing_outcomes <- setdiff(referenced_outcomes, names(base_outcomes))

  if (length(missing_outcomes) > 0) {
    stop(
      "Referenced base outcomes not found in base_outcomes: ",
      paste(missing_outcomes, collapse = ", "),
      call. = FALSE
    )
  }

  # Check if edge_properties is needed
  has_na_durations <- any(is.na(outcome_components$duration_value))

  if (has_na_durations && is.null(edge_properties)) {
    stop(
      "edge_properties must be provided when outcome_components contains NA duration_value",
      call. = FALSE
    )
  }

  # Validate edge_properties if provided
  if (!is.null(edge_properties)) {
    edge_props_df <- as.data.frame(edge_properties)

    if (!all(c("from_node", "to_node") %in% names(edge_props_df))) {
      stop(
        "edge_properties must contain 'from_node' and 'to_node' columns",
        call. = FALSE
      )
    }

    # Check that all required duration_unit columns exist in edge_properties
    if (has_na_durations) {
      na_duration_rows <- outcome_components[is.na(outcome_components$duration_value), ]
      required_duration_cols <- unique(na_duration_rows$duration_unit)
      missing_duration_cols <- setdiff(required_duration_cols, names(edge_props_df))

      if (length(missing_duration_cols) > 0) {
        stop(
          "edge_properties is missing columns needed for duration lookup: ",
          paste(missing_duration_cols, collapse = ", "),
          call. = FALSE
        )
      }
    }
  }

  # Calculate composite outcomes
  composite_outcomes <- list()
  outcome_labels <- unique(outcome_components$outcome_label)

  for (label in outcome_labels) {
    # Get all components for this outcome label
    label_components <- outcome_components[outcome_components$outcome_label == label, ]
    total_outcome <- 0

    # Process each component
    for (i in seq_len(nrow(label_components))) {
      component <- label_components[i, ]

      # Get base outcome value
      base_outcome_value <- base_outcomes[[component$base_outcome_name]]

      if (!is.numeric(base_outcome_value) || length(base_outcome_value) != 1) {
        stop(
          sprintf(
            "Base outcome '%s' must be a single numeric value, got: %s",
            component$base_outcome_name,
            paste(class(base_outcome_value), collapse = ", ")
          ),
          call. = FALSE
        )
      }

      # Determine duration value
      if (!is.na(component$duration_value)) {
        # Use explicit duration value
        duration_value <- component$duration_value
      } else {
        # Look up from edge_properties
        edge_match <- edge_props_df$from_node == component$edge_from &
                      edge_props_df$to_node == component$edge_to

        if (sum(edge_match) == 0) {
          stop(
            sprintf(
              "No matching edge found in edge_properties for %d -> %d",
              component$edge_from,
              component$edge_to
            ),
            call. = FALSE
          )
        }

        if (sum(edge_match) > 1) {
          stop(
            sprintf(
              "Multiple matching edges found in edge_properties for %d -> %d",
              component$edge_from,
              component$edge_to
            ),
            call. = FALSE
          )
        }

        duration_value <- edge_props_df[[component$duration_unit]][edge_match]

        if (is.na(duration_value)) {
          stop(
            sprintf(
              "Duration value is NA in edge_properties for edge %d -> %d, column '%s'",
              component$edge_from,
              component$edge_to,
              component$duration_unit
            ),
            call. = FALSE
          )
        }
      }

      # Calculate component outcome based on outcome_type
      if (component$outcome_type == "qaly_direct") {
        # Use base outcome directly, no duration multiplication
        component_outcome <- base_outcome_value

      } else {
        # Convert duration to years for both utility_to_qaly and life_years
        years_duration <- convert_duration_to_years(
          duration_value,
          component$duration_unit
        )

        # Both utility_to_qaly and life_years use the same formula:
        # base_outcome * years_duration
        component_outcome <- base_outcome_value * years_duration
      }

      # Add to total
      total_outcome <- total_outcome + component_outcome
    }

    # Store the composite outcome
    composite_outcomes[[label]] <- total_outcome
  }

  return(composite_outcomes)
}