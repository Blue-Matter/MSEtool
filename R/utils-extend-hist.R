#' Extend Hist Object Years
#'
#' Extend all components of a `Hist` object to include
#' additional years. This function expands stock-, fleet-, observation-,
#' implementation-, and time-series components along the year dimension while
#' preserving existing values.
#' 
#' Years in the `OM` object are only extended if the `Years` dimensions 
#' for a particular array  is greater than one. 
#' 
#' Simulations are not extended in the `OM` object.
#' 
#' @param Hist A [Hist()] object.
#' @param Years Numeric vector of years to extend the historical object to.
#' 
#' @return A modified `Hist` object with all relevant components extended
#'   to include the specified years.
#' @keywords internal
ExtendHist <- function(Hist, Years, silent=FALSE, id=NULL) {
  nStock <- nStock(Hist@OM)
  nFleet <- nFleet(Hist)
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist@OM)
  Areas <- 1:nArea
  
  if (!silent && is.null(id)) 
    id <- cli::cli_progress_bar("Extending `Hist` Object")
  
  # Extend OM
  Hist@OM <- ExtendOM(Hist@OM, Years=Years, silent=silent, id=id)

  # Extend time series 
  slots <- slotNames('timeseries')
  
  for (sl in slots) {
    if (!silent) {
      cli::cli_progress_update(id=id)
    }
    default <- 0
    if (sl == 'Misc') 
      default <- NULL

    if (sl == 'Effort' || sl == 'Distribution') 
      default <- NA
    
    slot(Hist, sl) <- Extend(array=slot(Hist, sl), 
                             nSim=nSim, 
                             Years = Years,
                             default = default)
  }
  
  if (!silent) {
    cli::cli_progress_done()
  }
  
  Hist
}



