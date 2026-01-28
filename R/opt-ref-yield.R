

CalcRefLandings <- function(Hist, type=c('Landings', 'Removals'), silent=FALSE) {
  
  type <- match.arg(type, c('Landings', 'Removals'), several.ok=TRUE)
  
  HistYears <- Years(Hist,'H')
  ProjYears <- Years(Hist,'P')
  nSim <- Hist@OM@nSim
  AllYears <- c(HistYears, ProjYears)
  

  # List length nSim, each with a Hist object with 1 sim - extended to include all years 
  TS_slots <- slotNames('timeseries')
  ProjSim_List <- purrr::map(1:nSim, \(i) {
    ProjSim <- Subset(Hist, i)
    ProjSim@OM@Stock <- Extend(ProjSim@OM@Stock, Years=AllYears)
    ProjSim@OM@Fleet <- Extend(ProjSim@OM@Fleet, Years=AllYears)
    for (sl in TS_slots) {
      if (sl == 'Misc') 
        next()
      slot(ProjSim,sl) <- Extend(slot(ProjSim,sl), Years=AllYears) 
    }
    ProjSim
    }, .progress = list(
      type = "iterator",
      format = "Extending {.val Year} dimension to calculate Reference {.val {type}} {cli::pb_bar} {cli::pb_percent}",
      clear = TRUE))
  
 
  ProjSim <- ProjSim_List[[1]] # for debugging
  
  if (!silent) cli::cli_alert_success("Calculate Reference {.val {type}}")
  
  
  
  
}

OptRefLandings <- function(logF, ProjSim, HistYears, ProjYears, type=c('Landings', 'Removals')) {
  type <- match.arg(type, c('Landings', 'Removals'))
  
  
  
  
}


