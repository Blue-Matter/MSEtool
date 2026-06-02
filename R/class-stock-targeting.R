#' The `stocktargeting` S4 Class
#'
#' The `stocktargeting` object defines the properties of a fleet's ability to
#' selectively target individual stocks in a mixed-stock fishery. Targeting
#' weights scale the effective fishing effort of each fleet toward or away from
#' each stock relative to the fleet mean.
#'
#' @slot Mean `array[sim, stock, fleet]`. Mean targeting weight for each
#'   simulation, stock, and fleet. Values of 1 indicate neutral (equal)
#'   targeting across stocks.
#'
#' @slot Covariance `array[sim, stock_i, stock_j, fleet]`. Log-space residual
#'   covariance matrix of targeting weights for each fleet and simulation.
#'   Diagonal entries represent per-stock variance; off-diagonal entries
#'   represent co-variation in targeting between pairs of stocks.
#'
#' @slot AC `array[sim, stock, fleet]`. Lag-1 autocorrelation of log targeting
#'   deviations for each simulation, stock, and fleet. Currently not used
#'  but retained for future implementation.
#'
#' @slot Targeting `array[sim, stock, fleet, year]`. Realised targeting
#'   deviations in normal space. For each fleet and year, values represent each
#'   stock's share of effective fishing effort relative to the fleet mean. 
#'   Values greater than 1 indicate
#'   preferential targeting; values less than 1 indicate avoidance.
#'
#' @slot Misc `list`. Reserved for additional information or user-defined
#'   extensions. Not used internally.
#'
#' @export
setClass("stocktargeting", representation(
  Mean       = "array.null",  # [sim, stock, fleet]
  Covariance = "array.null",  # [sim, stock_i, stock_j, fleet]
  AC         = "array.null",  # [sim, stock, fleet]
  Targeting  = "array.null",  # [sim, stock, fleet, year]
  Misc       = "list"
))
