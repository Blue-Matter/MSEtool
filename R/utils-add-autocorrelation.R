#' Add Auto-Correlation to a Vector of Values
#'
#' Applies an AR(1) auto-correlation structure to a vector of independent
#' values, conditioning on a provided last observed value. The transformation
#' scales each value to preserve the marginal variance under the AR(1) process.
#'
#' @param values `numeric` vector. Independent values to be transformed with
#'   auto-correlation (e.g., log recruitment deviations).
#' @param ac `numeric` scalar. Auto-correlation coefficient, typically in
#'   \[-1, 1\].
#' @param last_value `numeric` scalar. The last observed value from the
#'   preceding time period, used to condition the first element of `values`.
#
#'
#' The AR(1) recursion applied is:
#'
#' \deqn{x_t = \rho \cdot x_{t-1} + \epsilon_t \cdot \sqrt{1 - \rho^2}}
#'
#' where \eqn{\rho} is `ac` and \eqn{\epsilon_t} are the input `values`. The
#' \eqn{\sqrt{1 - \rho^2}} scaling ensures the marginal variance of \eqn{x_t}
#' equals the variance of \eqn{\epsilon_t}.
#'
#' @return A `numeric` vector of the same length as `values` with AR(1)
#'   auto-correlation applied.
#'   
#' @seealso [GenMultiStockRecDevs()]
AddAutoCorrelation <- function(values, ac, last_value) {
  n_fill <- length(values)
  values[1] <- ac * last_value + values[1] * sqrt(1 - ac^2)
  for (i in seq_len(n_fill)[-1]) {
    values[i] <- ac * values[i - 1] + values[i] * sqrt(1 - ac^2)
  }
  values
}