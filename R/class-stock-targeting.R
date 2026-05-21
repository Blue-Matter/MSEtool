#' Stock Targeting class
#' 
#' The `stocktargeting` object defines the properties of a fleet's ability 
#' to selectivity target individual stocks.
#'
#' @slot Mean `array[sim, stock, fleet]`. Mean targeting weight.
#'   
#' @slot Covariance `array[sim, stock, stock, fleet]`. Log residual covariance
#'   matrix of targeting weights per fleet.
#'   
#' @slot AC `array[sim, stock, fleet]`. Lag-1 auto-correlation of log deviations.
#' Currently not used.
#'   
#' @slot Targeting `array[sim, stock, fleet, year]`. Targeting
#'   deviations. Normal space. Values are mean-1 across stocks for each fleet-year, 
#'   representing each stock's share of effective fishing effort relative to 
#'   the fleet mean.
#'
#' @export
setClass("stocktargeting", representation(
  Mean            = "array.null",  # sim, stock, fleet 
  Covariance      = "array.null",  # sim, stock, stock, fleet
  AC              = "array.null",  # sim, stock, fleet
  Targeting       = "array.null",   # sim, stock, fleet, year
  Misc            = "list"
))



#' Constructor for stocktargeting
#'
#' Creates an empty (default) `stocktargeting` object with placeholder arrays
#'
#' @param OM An [om-class] object
#'
#' @return A `stocktargeting` object with:
#' - `Mean` initialised to 1
#'   - `Covariance` initialised to identity matrices
#'   - `Targeting` initialised to 1 (neutral targeting)
#'
#' @export
StockTargeting <- function(OM) {
  nSim      <- nSim(OM)
  nStock    <- nStock(OM)
  nFleet    <- nFleet(OM)
  HistYears <- Years(OM,'H')
  nYear     <- length(HistYears)
  
  if (nFleet < 1)
    return(new("stocktargeting"))
  
  targeting <- array(NA, dim = c(nSim, nStock, nFleet, nYear),
                     dimnames = list(
                       Sim   = seq_len(nSim),
                       Stock = StockNames(OM),
                       Fleet = FleetNames(OM),
                       Year  = HistYears
                     ))
  
  mean_arr  <- array(NA, dim = c(nSim, nStock, nFleet),
                     dimnames = list(
                       Sim   = seq_len(nSim),
                       Stock = StockNames(OM),
                       Fleet = FleetNames(OM)
                     ))
  
  ac_arr  <- array(NA, dim = c(nSim, nStock, nFleet),
                     dimnames = list(
                       Sim   = seq_len(nSim),
                       Stock = StockNames(OM),
                       Fleet = FleetNames(OM)
                     ))
  
  # Identity covariance per sim per fleet
  cov_arr <- array(NA, dim = c(nSim, nStock, nStock, nFleet),
                   dimnames = list(
                     Sim   = seq_len(nSim),
                     Stock = StockNames(OM),
                     Stock = StockNames(OM),
                     Fleet = FleetNames(OM)
                   ))
  
  for (sim in seq_len(nSim)) {
    for (fl in seq_len(nFleet)) {
      cov_arr[sim, , , fl] <- diag(nStock)
    }
  }
  

  new("stocktargeting",
      Mean           = mean_arr,
      Covariance     = cov_arr,
      AC             = ac_arr,
      Targeting      = targeting)
}