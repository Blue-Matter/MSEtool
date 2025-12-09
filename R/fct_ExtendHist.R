# Extend Hist object for Projection Years

ExtendHist <- function(Hist) {
  Years <- Years(Hist@OM)
  nStock <- nStock(Hist@OM)
  nSim <- Hist@OM@nSim
  nArea <- nArea(Hist@OM)
  
  for (st in 1:nStock) {
    Hist@OM@Stock[[st]] <- ExtendStock(Hist@OM@Stock[[st]], nSim, Years, silent=TRUE)
    Hist@OM@Fleet[[st]] <- ExtendFleet_Hist(Hist@OM@Fleet[[st]], Years)
  }
  
  for (i in seq_along(Hist@OM@Obs)) {
    Hist@OM@Obs[[i]] <- ArrayExpand(Hist@OM@Obs[[i]], 1, nAge=NULL, Years)
  }
  
  for (i in seq_along(Hist@OM@Imp)) {
    Hist@OM@Imp[[i]] <- ArrayExpand(Hist@OM@Imp[[i]], 1, nAge=NULL, Years)
  }
  
  
  Hist <- ExtendTimeSeries(Hist, Years)
  Hist
}


ExtendFleet_Hist <- function(Fleet, Years) {
  Fleet@DiscardMortality <- ArrayExpand(Fleet@DiscardMortality, 1, NULL, Years)
  Fleet@Catchability <- ArrayExpand(Fleet@Catchability, 1, NULL, Years)
  Fleet@qArea <- ArrayExpand(Fleet@qArea, 1, NULL, Years)
  Fleet@Selectivity <- ArrayExpand(Fleet@Selectivity, 1, NULL, Years)
  Fleet@Retention <- ArrayExpand(Fleet@Retention, 1, NULL, Years)
  Fleet@DiscardMortality <- ArrayExpand(Fleet@DiscardMortality, 1, NULL, Years)
  Fleet@Distribution <- ArrayExpand(Fleet@Distribution, 1, NULL, Years)
  Fleet@WeightFleet <- ArrayExpand(Fleet@WeightFleet, 1, NULL, Years)
  Fleet@Closure <- ArrayExpand(Fleet@Closure, 1, NULL, Years)
  Fleet
}

ExtendTimeSeries <- function(Hist, Years) {
  slots <- slotNames('timeseries')
  slots <- slots[!slots=='Misc']
  for (sl in slots) {
    object <- slot(Hist, sl) 
    
    if (is.list(object)) {
      if (!length(object))
        next()
      if (is.list(object[[1]])) {
        object <- purrr::map(object, \(x) {
          ListYears <- names(x)
          AddYears <- Years[!Years %in% ListYears]
          if (length(AddYears)) {
            AddList <- MakeNamedList(AddYears, x[[length(x)]])
            x <- c(x, AddList)
          }
          x
        })
      } else {
        object <- purrr::map(object, \(x) ExtendYears(x, Years, default = tiny/2))  
      }
      
    } else {
      object <- ExtendYears(object, Years, default = tiny/2)
    }
    slot(Hist, sl) <- object
  }
  Hist
}