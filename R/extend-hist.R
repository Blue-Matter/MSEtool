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
  
  if (!silent && is.null(id)) {
    id <- cli::cli_progress_bar("Extending `Hist` Object")
  }
  
  for (st in 1:nStock) {
    stock <- Hist@OM@Stock[[st]]
    AgeClasses <- stock@Ages@Classes
    Hist@OM@Stock[[st]] <- Extend(array=stock, 
                                  nSim = NULL, # don't extend sims 
                                  AgeClasses = AgeClasses,
                                  Years = Years,
                                  Areas = Areas)
  
    if (!silent) {
      cli::cli_progress_update(id=id)
    }
    
    for (fl in 1:nFleet) {
      Hist@OM@Fleet[[st]][[fl]] <- Extend(array=Hist@OM@Fleet[[st]][[fl]], 
                                          nSim = NULL, # don't extend sims 
                                          AgeClasses = NULL,
                                          Years = Years,
                                          Areas = Areas)
      if (!silent) {
        cli::cli_progress_update(id=id)
      }
    }
   
  }
  
  for (i in seq_along(Hist@OM@Obs)) {
    if (!silent) {
      cli::cli_progress_update(id=id)
    }
    Hist@OM@Obs[[i]] <- Extend(Hist@OM@Obs[[i]], 
                               nSim = NULL, # don't extend sims 
                               AgeClasses = NULL,
                               Years = Years,
                               Areas = Areas)
  }
  
  for (i in seq_along(Hist@OM@Imp)) {
    if (!silent) {
      cli::cli_progress_update(id=id)
    }
    Hist@OM@Imp[[i]] <- Extend(Hist@OM@Imp[[i]], 
                               nSim = NULL,  # don't extend sims 
                               AgeClasses = NULL,
                               Years = Years,
                               Areas = Areas)
  }
  
  # Extend time series 
  slots <- slotNames('timeseries')
  
  for (sl in slots) {
    if (!silent) {
      cli::cli_progress_update(id=id)
    }
    slot(Hist, sl) <- Extend(array=slot(Hist, sl), 
                             nSim=nSim, 
                             Years = Years,
                             default = NA)
    
  }
  
  if (!silent) {
    cli::cli_progress_done()
  }
  
  Hist
}



