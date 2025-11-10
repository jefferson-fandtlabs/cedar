#' Apply Time Discounting to Costs
#'
#' @description
#' Calculates the present value of a cost that occurs over multiple time periods
#' using a discount rate, starting from a specified time offset. This is an internal
#' helper function used by \code{\link{calculate_composite_costs}}.
#'
#' @param cost Numeric. The cost per time period (e.g., annual cost)
#' @param time_periods Numeric. The number of time periods over which the cost occurs
#' @param discount_rate Numeric. The discount rate as a decimal (e.g., 0.03 for 3%)
#' @param time_offset Numeric. The starting time point in the overall timeline.
#'   A value of 0 means the first period is t=0 to t=1 (undiscounted),
#'   a value of 1 means the first period is t=1 to t=2 (discounted once), etc.
#'
#' @details
#' The function calculates the present value using the standard discounting formula,
#' adjusted for the time offset:
#'
#' \deqn{PV = \sum_{t=offset}^{offset+T-1} \frac{cost}{(1 + r)^t}}
#'
#' where:
#' \itemize{
#'   \item PV = Present Value
#'   \item cost = Cost per time period
#'   \item r = Discount rate
#'   \item T = Total number of time periods
#'   \item offset = Time offset (when the cost period begins)
#'   \item t = Time period index (offset to offset+T-1)
#' }
#'
#' The time_offset parameter allows costs to be correctly discounted based on their
#' position in a decision tree. For example, if a cost occurs in years 2-3, it should
#' use time_offset=1 (starting after the first year).
#'
#' @return Numeric. The discounted present value of the cost
#'
#' @examples
#' # Calculate PV of $1000/year for 1 year starting at t=0 (undiscounted)
#' apply_discount(1000, 1, 0.03, 0)
#' # Returns: 1000 (discount factor = (1.03)^0 = 1.0)
#'
#' # Calculate PV of $1000/year for 1 year starting at t=1
#' apply_discount(1000, 1, 0.03, 1)
#' # Returns: 970.87 (discount factor = (1.03)^1 = 1.03)
#'
#' # Calculate PV of $1000/year for 3 years starting at t=2
#' apply_discount(1000, 3, 0.03, 2)
#' # Discounts years 2, 3, 4 with factors (1.03)^2, (1.03)^3, (1.03)^4
#'
#' # No discounting (0% rate), offset doesn't matter
#' apply_discount(5000, 3, 0, 0)
#' # Returns: 15000
#'
#' @seealso \code{\link{calculate_composite_costs}}
#'
#' @keywords internal
#' @export
apply_discount <- function(cost, time_periods, discount_rate, time_offset) {

  # Input validation
  if (!is.numeric(cost) || length(cost) != 1) {
    stop("cost must be a single numeric value", call. = FALSE)
  }

  if (!is.numeric(time_periods) || length(time_periods) != 1) {
    stop("time_periods must be a single numeric value", call. = FALSE)
  }

  if (!is.numeric(discount_rate) || length(discount_rate) != 1) {
    stop("discount_rate must be a single numeric value", call. = FALSE)
  }

  if (!is.numeric(time_offset) || length(time_offset) != 1) {
    stop("time_offset must be a single numeric value", call. = FALSE)
  }

  if (time_periods < 0) {
    stop("time_periods must be non-negative", call. = FALSE)
  }

  if (discount_rate < 0) {
    stop("discount_rate must be non-negative", call. = FALSE)
  }

  if (time_offset < 0) {
    stop("time_offset must be non-negative", call. = FALSE)
  }

  # Handle edge cases
  if (time_periods == 0) {
    return(0)
  }

  if (cost == 0) {
    return(0)
  }

  # Handle no discounting case for efficiency
  if (discount_rate == 0) {
    return(cost * time_periods)
  }

  # Calculate discounted present value
  # Formula: sum(cost / (1 + discount_rate)^t) for t = offset to (offset + time_periods - 1)
  # Example: offset=0, periods=1 gives t=[0] (undiscounted)
  # Example: offset=1, periods=2 gives t=[1,2] (discounted once and twice)
  time_sequence <- seq_len(time_periods) + time_offset - 1
  discount_factors <- (1 + discount_rate)^time_sequence
  discounted_costs <- cost / discount_factors

  return(sum(discounted_costs))
}