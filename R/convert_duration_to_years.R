#' Convert Duration to Years
#'
#' @description
#' Converts time durations from various units (hours, days, weeks, months) to years.
#' This is an internal helper function used by
#' \code{\link{calculate_composite_health_outcomes}} to standardize all durations
#' to years for QALY calculations.
#'
#' @param duration Numeric. The duration value to convert
#' @param duration_unit Character. The unit of the duration. Must be one of:
#'   "hours", "days", "weeks", "months", "years"
#'
#' @details
#' The function uses the following conversion factors:
#' \itemize{
#'   \item hours: 1 year = 8766 hours (365.25 days * 24 hours)
#'   \item days: 1 year = 365.25 days (accounts for leap years)
#'   \item weeks: 1 year = 52.17857 weeks (365.25 / 7)
#'   \item months: 1 year = 12 months
#'   \item years: 1 year = 1 year (no conversion)
#' }
#'
#' The conversion uses 365.25 days per year to account for leap years, which is
#' the standard in epidemiology and health economics.
#'
#' @return Numeric. The duration converted to years
#'
#' @examples
#' # Convert 6 months to years
#' convert_duration_to_years(6, "months")
#' # Returns: 0.5
#'
#' # Convert 90 days to years
#' convert_duration_to_years(90, "days")
#' # Returns: 0.2464066
#'
#' # Convert 2 weeks to years
#' convert_duration_to_years(2, "weeks")
#' # Returns: 0.03833
#'
#' # Years remain unchanged
#' convert_duration_to_years(5, "years")
#' # Returns: 5
#'
#' @seealso \code{\link{calculate_composite_health_outcomes}}
#'
#' @export
convert_duration_to_years <- function(duration, duration_unit) {

  # Input validation
  if (!is.numeric(duration) || length(duration) != 1) {
    stop("duration must be a single numeric value", call. = FALSE)
  }

  if (!is.character(duration_unit) || length(duration_unit) != 1) {
    stop("duration_unit must be a single character value", call. = FALSE)
  }

  if (duration < 0) {
    stop("duration must be non-negative", call. = FALSE)
  }

  # Valid duration units
  valid_units <- c("hours", "days", "weeks", "months", "years")
  if (!duration_unit %in% valid_units) {
    stop(
      sprintf(
        "Invalid duration_unit: '%s'\nValid units are: %s",
        duration_unit,
        paste(valid_units, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # Handle zero duration
  if (duration == 0) {
    return(0)
  }

  # Conversion factors (using 365.25 days per year to account for leap years)
  days_per_year <- 365.25
  hours_per_year <- days_per_year * 24
  weeks_per_year <- days_per_year / 7
  months_per_year <- 12

  # Convert to years based on unit
  years <- switch(
    duration_unit,
    hours = duration / hours_per_year,
    days = duration / days_per_year,
    weeks = duration / weeks_per_year,
    months = duration / months_per_year,
    years = duration
  )

  return(years)
}