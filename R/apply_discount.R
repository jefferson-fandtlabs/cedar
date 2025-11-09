#' Apply Time Discounting to Costs
#'
#' @description
#' Calculates the present value of a cost that occurs over multiple time periods
#' using a discount rate. This is an internal helper function used by
#' \code{\link{calculate_composite_costs}}.
#'
#' @param cost Numeric. The cost per time period (e.g., annual cost)
#' @param time_periods Numeric. The number of time periods over which the cost occurs
#' @param discount_rate Numeric. The discount rate as a decimal (e.g., 0.03 for 3%)
#'
#' @details
#' The function calculates the present value using the standard discounting formula:
#'
#' \deqn{PV = \sum_{t=1}^{T} \frac{cost}{(1 + r)^t}}
#'
#' where:
#' \itemize{
#'   \item PV = Present Value
#'   \item cost = Cost per time period
#'   \item r = Discount rate
#'   \item T = Total number of time periods
#'   \item t = Time period index (1 to T)
#' }
#'
#' @return Numeric. The discounted present value of the cost
#'
#' @examples
#' # Calculate present value of $5000/year for 3 years at 3% discount rate
#' apply_discount(5000, 3, 0.03)
#' # Returns: 14282.46
#'
#' # No discounting (0% rate)
#' apply_discount(5000, 3, 0)
#' # Returns: 15000
#'
#' # Single period (equivalent to simple discounting)
#' apply_discount(5000, 1, 0.03)
#' # Returns: 4854.369
#'
#' @seealso \code{\link{calculate_composite_costs}}
#'
#' @keywords internal
#' @export
apply_discount <- function(cost, time_periods, discount_rate) {

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

  if (time_periods < 0) {
    stop("time_periods must be non-negative", call. = FALSE)
  }

  if (discount_rate < 0) {
    stop("discount_rate must be non-negative", call. = FALSE)
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
  # Formula: sum(cost / (1 + discount_rate)^t) for t = 1 to time_periods
  time_sequence <- seq_len(time_periods)
  discount_factors <- (1 + discount_rate)^time_sequence
  discounted_costs <- cost / discount_factors

  return(sum(discounted_costs))
}