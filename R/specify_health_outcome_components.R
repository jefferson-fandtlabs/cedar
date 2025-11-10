#' Specify Health Outcome Components for Composite Calculations
#'
#' @description
#' Validates and structures health outcome component data frames for use with
#' \code{\link{calculate_composite_health_outcomes}}. This function ensures that
#' health outcome specifications have all required columns, valid outcome types,
#' and valid duration units.
#'
#' @param components A data frame or tibble specifying how health outcomes should
#'   be calculated from base outcome values. Must contain the following columns:
#'   \itemize{
#'     \item outcome_label: Character. Name for the composite health outcome
#'     \item base_outcome_name: Character (optional for "life_years"). Name of the
#'       base health outcome value (must exist in base_outcomes list passed to
#'       calculate_composite_health_outcomes). Can be omitted entirely if all rows
#'       use outcome_type = "life_years", or set to NA for individual life_years rows.
#'     \item outcome_type: Character. Type of outcome calculation. Must be one of:
#'       \itemize{
#'         \item "utility_to_qaly": Health utility value that will be converted to QALY
#'           by multiplying by duration in years
#'         \item "life_years": Life years calculation. Automatically uses a base value
#'           of 1.0 (i.e., 1 year per year of duration). The base_outcome_name is not
#'           needed and will be ignored if provided.
#'         \item "qaly_direct": Pre-calculated QALY value (no conversion needed)
#'       }
#'     \item duration_unit: Character. Unit of duration. Must be one of:
#'       "hours", "days", "weeks", "months", "years"
#'     \item edge_from: Integer. Parent node number
#'     \item edge_to: Integer. Child node number
#'     \item duration_value: Numeric (optional). Duration value to use. If NA or
#'       missing, will be looked up from edge_properties using the duration_unit
#'       column name
#'   }
#'
#' @return A tibble with class "health_outcome_components" containing the
#'   validated specification
#'
#' @details
#' This function performs the following validations:
#' \itemize{
#'   \item Ensures all required columns are present (base_outcome_name is optional)
#'   \item Validates that outcome_type values are one of the allowed types
#'   \item Validates that duration_unit values are one of the allowed units
#'   \item Validates base_outcome_name handling: if the column is missing, all rows
#'     must be "life_years"; if present, NA values are only allowed for "life_years"
#'   \item Adds duration_value column if not present (initialized to NA)
#'   \item Converts the input to a tibble if it isn't already
#' }
#'
#' The outcome_type determines how the calculation is performed:
#' \itemize{
#'   \item \strong{utility_to_qaly}: The base outcome value is a health utility
#'     (typically 0-1). The function will multiply this by the duration in years
#'     to calculate QALYs. For example, a utility of 0.8 over 2 years = 1.6 QALYs.
#'   \item \strong{life_years}: Automatically uses a base value of 1.0,
#'     representing 1 year of life per year of duration. The base_outcome_name
#'     column is optional for this outcome type and will be ignored if provided.
#'     Duration is converted to years and multiplied by 1.0 to get life years.
#'   \item \strong{qaly_direct}: The base outcome value is already a QALY value
#'     and no conversion is needed. Duration is still converted to years for
#'     consistency but may not be used in calculation.
#' }
#'
#' Duration values can be specified in two ways:
#' \enumerate{
#'   \item Directly in the components data frame using the duration_value column
#'   \item Looked up from edge_properties based on the duration_unit (when
#'     duration_value is NA)
#' }
#'
#' When duration_value is NA, the calculation function will look for a column
#' in edge_properties with the same name as duration_unit (e.g., "days", "months")
#' to find the duration for each edge.
#'
#' @examples
#' library(tibble)
#'
#' # Specify health outcome components
#' outcome_components <- tribble(
#'   ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
#'   "qaly_success", "utility_success", "utility_to_qaly", "years", 2, 4, 5,
#'   "qaly_failure", "utility_failure", "utility_to_qaly", "months", 2, 5, 6,
#'   "ly_gained", "base_ly", "life_years", "weeks", 3, 6, 52
#' )
#'
#' # Validate and structure
#' validated_components <- specify_health_outcome_components(outcome_components)
#' print(validated_components)
#'
#' # Example with edge property lookup (duration_value = NA)
#' outcome_components_lookup <- tribble(
#'   ~outcome_label, ~base_outcome_name, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to,
#'   "qaly_success", "utility_success", "utility_to_qaly", "years", 2, 4,
#'   "qaly_failure", "utility_failure", "utility_to_qaly", "months", 2, 5
#' )
#'
#' validated_components_lookup <- specify_health_outcome_components(outcome_components_lookup)
#'
#' # Example with life_years - no base_outcome_name needed!
#' life_years_components <- tribble(
#'   ~outcome_label, ~outcome_type, ~duration_unit, ~edge_from, ~edge_to, ~duration_value,
#'   "survival", "life_years", "years", 1, 2, 10,
#'   "survival", "life_years", "months", 2, 3, 24
#' )
#'
#' validated_ly <- specify_health_outcome_components(life_years_components)
#' # Will calculate: 10 years + 2 years = 12 life years total
#'
#' @seealso
#' \code{\link{calculate_composite_health_outcomes}} for using the specified components,
#' \code{\link{convert_duration_to_years}} for duration conversion details
#'
#' @export
specify_health_outcome_components <- function(components) {

  # Convert to tibble if not already
  if (!inherits(components, "tbl_df")) {
    components <- tibble::as_tibble(components)
  }

  # Check for required columns (base_outcome_name is optional)
  required_cols <- c("outcome_label", "outcome_type",
                     "duration_unit", "edge_from", "edge_to")
  missing_cols <- setdiff(required_cols, names(components))

  if (length(missing_cols) > 0) {
    stop(
      "Missing required columns in health outcome components specification: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate outcome_type values
  valid_outcome_types <- c("utility_to_qaly", "life_years", "qaly_direct")
  invalid_types <- setdiff(unique(components$outcome_type), valid_outcome_types)

  if (length(invalid_types) > 0) {
    stop(
      "Invalid outcome_type values: ", paste(invalid_types, collapse = ", "), "\n",
      "Valid values are: ", paste(valid_outcome_types, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate duration_unit values
  valid_duration_units <- c("hours", "days", "weeks", "months", "years")
  invalid_units <- setdiff(unique(components$duration_unit), valid_duration_units)

  if (length(invalid_units) > 0) {
    stop(
      "Invalid duration_unit values: ", paste(invalid_units, collapse = ", "), "\n",
      "Valid values are: ", paste(valid_duration_units, collapse = ", "),
      call. = FALSE
    )
  }

  # Validate base_outcome_name column handling
  if (!"base_outcome_name" %in% names(components)) {
    # Column missing - all rows must be life_years
    non_life_years <- components[components$outcome_type != "life_years", ]
    if (nrow(non_life_years) > 0) {
      stop(
        "base_outcome_name column required when using outcome types other than 'life_years'. ",
        "Found: ", paste(unique(non_life_years$outcome_type), collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    # Column present - NA values only allowed for life_years
    non_life_years_rows <- components$outcome_type != "life_years"
    if (any(non_life_years_rows)) {
      na_in_non_life_years <- is.na(components$base_outcome_name[non_life_years_rows])
      if (any(na_in_non_life_years)) {
        stop(
          "base_outcome_name cannot be NA for outcome types 'utility_to_qaly' or 'qaly_direct'",
          call. = FALSE
        )
      }
    }
  }

  # Add duration_value column if not present
  if (!"duration_value" %in% names(components)) {
    components$duration_value <- NA_real_
  }

  # Add class for S3 method dispatch
  class(components) <- c("health_outcome_components", class(components))

  return(components)
}


#' Print Health Outcome Components
#'
#' @description
#' Custom print method for health_outcome_components objects created by
#' \code{\link{specify_health_outcome_components}}.
#'
#' @param x A health_outcome_components object
#' @param ... Additional arguments (not used)
#'
#' @return Invisibly returns the input object
#'
#' @export
print.health_outcome_components <- function(x, ...) {
  cat("Health Outcome Components Specification\n")
  cat("========================================\n\n")

  # Count unique outcome labels
  unique_labels <- unique(x$outcome_label)
  cat(sprintf("Number of composite outcomes: %d\n", length(unique_labels)))
  cat(sprintf("Total components: %d\n\n", nrow(x)))

  # Summarize by outcome type
  outcome_type_summary <- table(x$outcome_type)
  cat("Components by outcome type:\n")
  for (type in names(outcome_type_summary)) {
    cat(sprintf("  - %s: %d\n", type, outcome_type_summary[[type]]))
  }
  cat("\n")

  # Summarize by duration unit
  duration_unit_summary <- table(x$duration_unit)
  cat("Components by duration unit:\n")
  for (unit in names(duration_unit_summary)) {
    cat(sprintf("  - %s: %d\n", unit, duration_unit_summary[[unit]]))
  }
  cat("\n")

  # Show how many have explicit duration values vs lookup
  explicit_durations <- sum(!is.na(x$duration_value))
  lookup_durations <- sum(is.na(x$duration_value))
  cat("Duration value specification:\n")
  cat(sprintf("  - Explicit values: %d\n", explicit_durations))
  cat(sprintf("  - Edge property lookup: %d\n\n", lookup_durations))

  # Print the actual data
  cat("Component details:\n")
  print(tibble::as_tibble(x))

  invisible(x)
}