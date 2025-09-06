ExtendHist <- function(Hist) {
  
  TimeSteps <- TimeSteps(Hist@OM)
  TimeStepsHist <- TimeSteps(Hist@OM, 'Historical')
  TimeStepsProj <- TimeSteps[!TimeSteps %in%TimeStepsHist]
  
  nStock <- nStock(Hist@OM)
  
  for (st in 1:nStock) {
    stock <- Hist@OM@Stock[[st]]
    nAge <- length(stock@Ages@Classes)
    for (sl in slotNames(stock)) {
      if (sl=='SRR') {
        Hist@OM@Stock[[st]]@SRR@Pars <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@Pars, 1, nAge, TimeSteps)
        Hist@OM@Stock[[st]]@SRR@R0 <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@R0, 1, nAge, TimeSteps)
        Hist@OM@Stock[[st]]@SRR@SD <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@SD, 1, nAge, TimeSteps)
        Hist@OM@Stock[[st]]@SRR@AC <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@AC, 1, nAge, TimeSteps)
        Hist@OM@Stock[[st]]@SRR@RecDevHist <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@RecDevHist, 1, nAge, TimeStepsHist)
        Hist@OM@Stock[[st]]@SRR@RecDevProj <- ArrayExpand(Hist@OM@Stock[[st]]@SRR@RecDevProj, 1, nAge, TimeStepsHist)
      } else {
        slot(Hist@OM@Stock[[st]], sl) <- ArrayExpand(slot(Hist@OM@Stock[[st]], sl), 1, nAge, TimeSteps)
      }
    }
    
    fleet <- Hist@OM@Fleet[[st]]
    fleet@FishingMortality <- ArrayExpand(fleet@FishingMortality, 1, nAge, TimeSteps)
    fleet@DiscardMortality <- ArrayExpand(fleet@DiscardMortality, 1, nAge, TimeSteps)
    fleet@Effort <- ArrayExpand(fleet@Effort, 1, nAge, TimeStepsHist)
    fleet@Effort@Catchability <- ArrayExpand(fleet@Effort@Catchability, 1, nAge, TimeSteps)
    fleet@Selectivity <- ArrayExpand(fleet@Selectivity, 1, nAge, TimeSteps)
    fleet@Retention <- ArrayExpand(fleet@Retention, 1, nAge, TimeSteps)
    fleet@DiscardMortality <- ArrayExpand(fleet@DiscardMortality, 1, nAge, TimeSteps)
    fleet@Distribution <- ArrayExpand(fleet@Distribution, 1, nAge, TimeSteps)
    fleet@WeightFleet <- ArrayExpand(fleet@WeightFleet, 1, nAge, TimeSteps)
    
    Hist@OM@Fleet[[st]] <- fleet
  }
  
  for (i in seq_along(Hist@OM@Obs)) {
    Hist@OM@Obs[[i]] <- ArrayExpand(Hist@OM@Obs[[i]], 1, nAge, TimeSteps)
  }
  
  for (i in seq_along(Hist@OM@Imp)) {
    Hist@OM@Imp[[i]] <- ArrayExpand(Hist@OM@Imp[[i]], 1, nAge, TimeSteps)
  }
  
  slots <- slotNames('timeseries')
  slots <- slots[!slots=='Misc']
  for (sl in slots) {
    object <- slot(Hist, sl) 
    if (is.list(object)) {
      object <- purrr::map(object, \(x) ExpandTimeSteps(x, TimeSteps, default = tiny/2))
    } else {
      object <- ExpandTimeSteps(object, TimeSteps, default = tiny/2)
    }
    slot(Hist, sl) <- object
  }
  Hist
}

