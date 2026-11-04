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
.ExtendOM <- function(OM, Years = NULL, nSim = NULL, silent=FALSE, id=NULL) {
  show <- .MsgShowProgress(silent)
  
  nStock <- nStock(OM)
  nFleet <- nFleet(OM)
  nArea <- nArea(OM)
  Areas <- 1:nArea
  
  if (is.null(Years))
    Years <- Years(OM,'H')
  
  if (show && is.null(id)) 
    id <- cli::cli_progress_bar("Extending OM object")
  
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
                                                     Areas = Areas,
                                                     backfill = TRUE)
          
        }
          
      } else  if (sl =='SRR') {
        for (sl2 in slotNames('srr')) {
          if (sl2 =='RecDevHist') {
            slot(OM@Stock[[st]]@SRR,sl2) <- ExtendSims(slot(OM@Stock[[st]]@SRR,sl2), nSim)
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
                                               Areas = Areas,
                                               backfill = sl %in% c("NaturalMortality", "Maturity", "Fecundity"))
      }
    }
    
    if (show) 
      cli::cli_progress_update(id=id)
    
    for (fl in 1:nFleet) {
      OM@Fleet[[st]][[fl]] <- Extend(array=OM@Fleet[[st]][[fl]], 
                                     nSim = nSim, 
                                     AgeClasses = AgeClasses,
                                     Years = Years,
                                     Areas = Areas)
      for (sl in c("WeightFleetRetained", "WeightFleetSelected"))
        slot(OM@Fleet[[st]][[fl]], sl) <- Extend(slot(OM@Fleet[[st]][[fl]], sl),
                                                 Years = Years, backfill = TRUE)
      if (show) 
        cli::cli_progress_update(id=id)
      
    }
    
    if (st <= length(OM@Obs)) {
      if (show) {
        cli::cli_progress_update(id=id)
      }
      IndexSel <- .GetIndexSelectivity(OM@Obs[[st]])
      # Obs arrays are ReduceDims() output of full-length arrays; step-fill inverts it
      OM@Obs[[st]] <- Extend(.SetIndexSelectivity(OM@Obs[[st]]),
                             nSim = nSim,
                             AgeClasses = AgeClasses,
                             Years = Years,
                             Areas = Areas,
                             maintain_seasonal_pattern = FALSE)
      OM@Obs[[st]] <- .SetIndexSelectivity(OM@Obs[[st]], IndexSel, Years)
    }


    if (st <= length(OM@Imp)) {
      if (show) {
        cli::cli_progress_update(id=id)
      }
      OM@Imp[[st]] <- Extend(OM@Imp[[st]],
                             nSim = nSim,
                             AgeClasses = AgeClasses,
                             Years = Years,
                             Areas = Areas)
    }
  }
  OM@StockTargeting <- Extend(OM@StockTargeting, nSim=nSim, Years=Years)
  
  OM
}

# index Selectivity by fleet and type; NULL for non-obs elements
.GetIndexSelectivity <- function(ObsList) {
  lapply(ObsList, \(obs) {
    if (!methods::is(obs, 'obs')) return(NULL)
    list(CPUE = obs@CPUE@Selectivity, Survey = obs@Survey@Selectivity)
  })
}

# sets index Selectivity from `Sel` (NULL clears it)
.SetIndexSelectivity <- function(ObsList, Sel = NULL, Years = NULL) {
  for (fl in seq_along(ObsList)) {
    if (!methods::is(ObsList[[fl]], 'obs')) next
    for (type in c('CPUE', 'Survey'))
      slot(ObsList[[fl]], type)@Selectivity <- .ExtendIndexSelectivityYears(Sel[[fl]][[type]], Years)
  }
  ObsList
}

# step-fills reduced years through the last calendar year, then repeats the seasonal pattern (seasonal age classes)
.ExtendIndexSelectivityYears <- function(sel, Years) {
  if (is.list(sel)) return(lapply(sel, .ExtendIndexSelectivityYears, Years = Years))
  yr <- dimnames(sel)[['Year']]
  if (is.null(Years) || is.null(yr)) return(sel)
  Within <- Years[Years < floor(max(as.numeric(yr))) + 1]
  ExtendYears(sel, Within, maintain_seasonal_pattern = FALSE) |>
    ExtendYears(Years, maintain_seasonal_pattern = TRUE)
}
