ExtendHist <- function(Hist) {
  
  Years <- Years(Hist@OM)
  YearsHist <- Years(Hist@OM, 'Historical')
  YearsProj <- Years[!Years %in%YearsHist]
  
  nStock <- nStock(Hist@OM)
  
  for (st in 1:nStock) {
    stock <- Hist@OM@Stock[[st]]
    nAge <- length(stock@Ages@Classes)
    for (sl in slotNames(stock)) {
      if (sl=='SRR') {
        Hist@OM@Stock[[st]]@SRR@Pars <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@Pars, 1, nAge, Years)
        Hist@OM@Stock[[st]]@SRR@R0 <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@R0, 1, nAge, Years)
        Hist@OM@Stock[[st]]@SRR@SD <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@SD, 1, nAge, Years)
        Hist@OM@Stock[[st]]@SRR@AC <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@AC, 1, nAge, Years)
        Hist@OM@Stock[[st]]@SRR@RecDevHist <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@RecDevHist, 1, nAge, YearsHist)
        Hist@OM@Stock[[st]]@SRR@RecDevProj <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@RecDevProj, 1, nAge, YearsHist)
      } else {
        slot(Hist@OM@Stock[[st]], sl) <- ArrayExpand(slot(Hist@OM@Stock[[st]], sl), 1, nAge, Years)
      }
    }
    
    fleet <- Hist@OM@Fleet[[st]]
    fleet@DiscardMortality <- ArrayExpand(fleet@DiscardMortality, 1, nAge, Years)
    fleet@Effort <- ArrayExpand(fleet@Effort, 1, nAge, YearsHist)
    fleet@Catchability <- ArrayExpand(fleet@Catchability, 1, nAge, Years)
    fleet@qArea <- ArrayExpand(fleet@qArea, 1, nAge, Years)
    fleet@Selectivity <- ArrayExpand(fleet@Selectivity, 1, nAge, Years)
    fleet@Retention <- ArrayExpand(fleet@Retention, 1, nAge, Years)
    fleet@DiscardMortality <- ArrayExpand(fleet@DiscardMortality, 1, nAge, Years)
    fleet@Distribution <- ArrayExpand(fleet@Distribution, 1, nAge, Years)
    fleet@WeightFleet <- ArrayExpand(fleet@WeightFleet, 1, nAge, Years)
    fleet@Closure <- ArrayExpand(fleet@Closure, 1, nAge, Years)
    Hist@OM@Fleet[[st]] <- fleet
  }
  
  for (i in seq_along(Hist@OM@Obs)) {
    Hist@OM@Obs[[i]] <- ArrayExpand(Hist@OM@Obs[[i]], 1, nAge, Years)
  }
  
  for (i in seq_along(Hist@OM@Imp)) {
    Hist@OM@Imp[[i]] <- ArrayExpand(Hist@OM@Imp[[i]], 1, nAge, Years)
  }
  
  slots <- slotNames('timeseries')
  slots <- slots[!slots=='Misc']
  for (sl in slots) {
    object <- slot(Hist, sl) 
    if (is.list(object)) {
      object <- purrr::map(object, \(x) ExpandYears(x, Years, default = tiny/2))
    } else {
      object <- ExpandYears(object, Years, default = tiny/2)
    }
    slot(Hist, sl) <- object
  }
  Hist
}

