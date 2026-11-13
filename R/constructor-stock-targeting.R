#' Stock Targeting Constructor and Accessors
#'
#' Construct a [stocktargeting-class] object, which describes how a fleet's
#' fishing effort is distributed among the stocks of a multi-stock operating
#' model, or access the `StockTargeting` slot of an operating model and the
#' slots of a [stocktargeting-class] object.
#'
#' The slots are populated by [Simulate()] (see [FitStockTargeting()] and
#' [GenerateStockTargeting()]), so they rarely need to be set manually.
#'
#' @param Mean `array[sim, stock, fleet]` or `NULL`. Mean targeting weight
#'   for each simulation, stock, and fleet. When `Mean` is an [om-class],
#'   [hist-class], or [mse-class] object, `StockTargeting()` returns its
#'   `StockTargeting` slot (of the embedded `OM` for `hist` and `mse`).
#' @param Covariance `array[sim, stock_i, stock_j, fleet]` or `NULL`.
#'   Log-space residual covariance of the targeting weights.
#' @param AC `array[sim, stock, fleet]` or `NULL`. Lag-1 autocorrelation of
#'   the log targeting residuals.
#' @param Targeting `array[sim, stock, fleet, year]` or `NULL`. Realised
#'   targeting deviations.
#' @param Misc `list`. Used internally. Default `list()`.
#' @param x An [om-class] object, for `StockTargeting<-`; a
#'   [stocktargeting-class] object, for `Covariance()` and `Covariance<-`.
#' @param value A [stocktargeting-class] object, for `StockTargeting<-`; an
#'   array, for `Covariance<-`.
#'
#' @details
#' `Mean()`, `AC()`, and `Targeting()` also access the corresponding slots of
#' a [stocktargeting-class] object.
#'
#' @return
#' - `StockTargeting()` returns a [stocktargeting-class] object.
#' - `StockTargeting<-` returns the [om-class] object `x` with the
#'   `StockTargeting` slot replaced; it can only be replaced in [om-class]
#'   objects.
#' - `Covariance()` returns the `Covariance` slot, and `Covariance<-` returns
#'   `x` with the slot replaced.
#'
#' @seealso [stocktargeting-class], [GenerateStockTargeting()],
#'   [FitStockTargeting()], [StockTargetingLambda()]
#'
#' @family om
#'
#' @examples
#' st <- StockTargeting(MultiStockOM)
#'
#' @export
StockTargeting <- function(Mean       = NULL,
                           Covariance = NULL,
                           AC         = NULL,
                           Targeting  = NULL,
                           Misc       = list()) {
  if (inherits(Mean, c('om', 'hist', 'mse')))
    return(.IsHist(Mean, 'StockTargeting'))

  methods::new('stocktargeting',
               Mean       = Mean,
               Covariance = Covariance,
               AC         = AC,
               Targeting  = Targeting,
               Misc       = Misc)
}

#' @rdname StockTargeting
#' @export
`StockTargeting<-` <- function(x, value) {
  .CheckClass(value, 'stocktargeting', 'value')
  .AssignOMSlot(x, value, 'StockTargeting', 'struct')
}

#' @rdname StockTargeting
#' @export
Covariance <- function(x) .AccessSlot(x, 'Covariance')

#' @rdname StockTargeting
#' @export
`Covariance<-` <- function(x, value) .AssignSlot(x, value, 'Covariance')

# stocktargeting object with NA arrays dimensioned to `OM`
.InitStockTargeting <- function(OM) {
  nSim      <- nSim(OM)
  nStock    <- nStock(OM)
  nFleet    <- nFleet(OM)
  HistYears <- Years(OM, 'H')
  nYear     <- length(HistYears)
  
  if (nFleet < 1 || nStock < 2) 
    return(new("stocktargeting"))
  
  sim_names   <- seq_len(nSim)
  stock_names <- StockNames(OM)
  fleet_names <- FleetNames(OM)
  
  targeting <- array(NA_real_,
                     dim      = c(nSim, nStock, nFleet, nYear),
                     dimnames = list(
                       Sim   = sim_names,
                       Stock = stock_names,
                       Fleet = fleet_names,
                       Year  = HistYears
                     ))
  
  mean_arr <- array(NA_real_,
                    dim      = c(nSim, nStock, nFleet),
                    dimnames = list(
                      Sim   = sim_names,
                      Stock = stock_names,
                      Fleet = fleet_names
                    ))
  
  ac_arr <- array(NA_real_,
                  dim      = c(nSim, nStock, nFleet),
                  dimnames = list(
                    Sim   = sim_names,
                    Stock = stock_names,
                    Fleet = fleet_names
                  ))

  cov_arr <- array(NA_real_,
                   dim      = c(nSim, nStock, nStock, nFleet),
                   dimnames = list(
                     Sim     = sim_names,
                     Stock_i = stock_names,
                     Stock_j = stock_names,
                     Fleet   = fleet_names
                   ))
  
  new("stocktargeting",
      Mean       = mean_arr,
      Covariance = cov_arr,
      AC         = ac_arr,
      Targeting  = targeting)
}