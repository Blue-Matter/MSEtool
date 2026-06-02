#' Stock Targeting Constructor
#'
#' Initialises a `stocktargeting` object with `NA`-filled arrays sized to the
#' supplied operating model. All arrays are dimensioned and named but left
#' unpopulated.
#'
#' @param OM An [om-class] object. Must have at least one stock and one fleet.
#'
#' @return A `stocktargeting` object with:
#' - `Mean`: `NA` array of dimension `[nSim, nStock, nFleet]`
#' - `Covariance`: `NA` array of dimension `[nSim, nStock, nStock, nFleet]`
#' - `AC`: `NA` array of dimension `[nSim, nStock, nFleet]`
#' - `Targeting`: `NA` array of dimension `[nSim, nStock, nFleet, nHistYear]`
#' - `Misc`: empty list
#'
#' If `nFleet < 1` or `nStock < 1` an empty (default) `stocktargeting` object
#' is returned with a warning.
#'
#' @seealso [stocktargeting-class]
#' @export
StockTargeting <- function(OM) {
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