#' Extend OM Object Years
#'
#' Extend all components of a `OM` object to include
#' additional years. This function expands stock-, fleet-, observation-,
#' implementation-, and time-series components along the year dimension while
#' preserving existing values.
#' 
#' Years in the `OM` object are only extended if the `Years` dimensions 
#' for a particular array  is greater than one. 
#' 
#' Simulations are not extended in the `OM` object.
#' 
#' @param OM An [OM()] object.
#' 
#' @return A modified `Hist` object with all relevant components extended
#'   to include the specified years.
#' @keywords internal
ExtendOM <- function(OM, Years = NULL, nSim = NULL, silent=FALSE, id=NULL) {
  
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM)
  Areas <- 1:nArea
  
  if (is.null(Years))
    Years <- Years(OM,'H')
  
  if (!silent && is.null(id)) 
    id <- cli::cli_progress_bar("Extending `OM` Object")
  
  for (st in 1:nStock) {
    stock <- OM@Stock[[st]]
    AgeClasses <- stock@Ages@Classes
    
    for (sl in slotNames(stock)) {
      
      if (sl %in% c("Length", "Weight")) {
        
        for (sl2 in slotNames(tolower(sl))) {
          if (sl2 %in% c("ALK", "AWK"))
            next
          
          slot(slot(OM@Stock[[st]],sl),sl2) <- Extend(array=slot(slot(OM@Stock[[st]],sl),sl2), 
                                                     nSim = nSim, 
                                                     AgeClasses = AgeClasses,
                                                     Years = Years,
                                                     Areas = Areas)
          
        }
          
      } else  if (sl =='SRR') {
        for (sl2 in slotNames('srr')) {
          if (sl2 =='RecDevHist') {
            slot(OM@Stock[[st]]@SRR,sl2) <- Extend(array=slot(OM@Stock[[st]]@SRR,sl2), 
                                                   nSim = nSim, 
                                                   AgeClasses = AgeClasses,
                                                   Years = Years,
                                                   Areas = Areas)
          } else {
            slot(OM@Stock[[st]]@SRR,sl2) <- Extend(array=slot(OM@Stock[[st]]@SRR,sl2), 
                                                   nSim = nSim,
                                                   AgeClasses = AgeClasses,
                                                   Years = Years,
                                                   Areas = Areas)
          }
            
          
        
          
        }
      } else {
        slot(OM@Stock[[st]],sl) <- Extend(array=slot(OM@Stock[[st]],sl), 
                                               nSim = nSim, 
                                               AgeClasses = AgeClasses,
                                               Years = Years,
                                               Areas = Areas)
      }
    }
    
    if (!silent) 
      cli::cli_progress_update(id=id)
    
    for (fl in 1:nFleet) {
      OM@Fleet[[st]][[fl]] <- Extend(array=OM@Fleet[[st]][[fl]], 
                                     nSim = nSim, 
                                     AgeClasses = AgeClasses,
                                     Years = Years,
                                     Areas = Areas)
      if (!silent) 
        cli::cli_progress_update(id=id)
      
    }
    
    for (i in seq_along(OM@Obs)) {
      if (!silent) {
        cli::cli_progress_update(id=id)
      }
      OM@Obs[[i]] <- Extend(OM@Obs[[i]], 
                            nSim = nSim, 
                            AgeClasses = AgeClasses,
                                 Years = Years,
                                 Areas = Areas)
    }
    
    
    for (i in seq_along(OM@Imp)) {
      if (!silent) {
        cli::cli_progress_update(id=id)
      }
      OM@Imp[[i]] <- Extend(OM@Imp[[i]], 
                            nSim = nSim, 
                            AgeClasses = AgeClasses,
                            Years = Years,
                            Areas = Areas)
    }
  }
  OM@StockTargeting <- Extend(OM@StockTargeting, nSim=nSim, Years=Years)
  
  
  OM
  
  
}