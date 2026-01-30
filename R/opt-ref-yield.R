

CalcRefYield <- function(Hist, 
                         type=c('Landings', 'Removals'),
                         Units=c("Biomass", 'Number'),
                         silent=FALSE) {
  
  type <- match.arg(type, c('Landings', 'Removals'), several.ok=TRUE)
  Units <- match.arg(Units, c("Biomass", 'Number'))
  
  HistYears <- Years(Hist,'H')
  ProjYears <- Years(Hist,'P')
  nSim <- Hist@OM@nSim
  StockNames <- StockNames(Hist)
  nStock <- length(StockNames)
  nFleet <- nFleet(Hist)
  AllYears <- c(HistYears, ProjYears)
  
  # Extend Year dimensions to include ProjYears
  Proj <- Hist
  Proj@OM@Stock <- Extend(Proj@OM@Stock, Years=AllYears)
  Proj@OM@Fleet <- Extend(Proj@OM@Fleet, Years=AllYears)
  Proj@Misc <- Extend(Proj@Misc, Years=AllYears)
  
  for (sl in slotNames('timeseries')) {
    if (sl == 'Misc') 
      next()
    slot(Proj,sl) <- Extend(slot(Proj,sl), Years=AllYears, default=0) 
  }
  
  # List length nSim, each with a Hist object with 1 sim
  ProjSim_List <- lapply(1:nSim, function(i) SubsetSim(Proj, Sims=i))
  names(ProjSim_List) <- 1:nSim
  
  ProjSim_List[[1]]@Misc$RecDevs$Female|> dim()
  ProjSim_List[[10]]@Misc$RecDevs$Female |> dim()
  
  tt <- SubsetSim(object=Proj@Misc$RecDevs, Sims=10)
  dim(tt$Female)
  
  
  # Prepare output arrays
  RefLandings <- RefRemovals <- array(NA, 
                                      dim=c(nSim, nStock),
                                      dimnames = list(
                                        Sim=1:nSim,
                                        Stock=StockNames
                                      ))
  
  log_bounds <- log(c(1E-5, 10))

  for (t in type) {
    if (silent) {
      
      stop()
      
      RefYield <- lapply(ProjSim_List, function(ProjSim) {
        DoOpt <- optimize(OptRefYield,
                        log_bounds,
                        ProjSim = ProjSim,
                        HistYears = HistYears,
                        ProjYears = ProjYears,
                        nFleet = nFleet,
                        Units = Units,
                        type = t
        )
      
      })
      
    } else {
      RefYield <- purrr::map(ProjSim_List[1:3], \(ProjSim) {
        
        DoOpt <- optimize(OptRefYield,
                        log_bounds,
                        ProjSim = ProjSim,
                        HistYears = HistYears,
                        ProjYears = ProjYears,
                        nFleet = nFleet,
                        Units = Units,
                        type = t
        )
        
        yield <- OptRefYield(logScalar = DoOpt$minimum, 
                             ProjSim = ProjSim,
                             HistYears = HistYears,
                             ProjYears = ProjYears,
                             nFleet = nFleet,
                             Units = Units,
                             type = t,
                             opt = 2)
        
    
        yield
      }, .progress = list(
        type = "iterator",
        format = "Calculating Reference {.val {t}} {cli::pb_bar} {cli::pb_percent}",
        clear = TRUE))
    }
    List2Array(RefYield, "Sim", "Stock")
    
 
  
    
  }
  Hist
}

OptRefYield <- function(logScalar, ProjSim, HistYears, ProjYears, nFleet,
                        Units=c('Biomass', 'Number'),
                        type=c('Landings', 'Removals'),
                        opt = 1) {
  
  Units <- match.arg(Units, c('Biomass', 'Number'))
  type <- match.arg(type, c('Landings', 'Removals'))
  
  ProjYearInd <- match(ProjYears, c(HistYears, ProjYears))
  
  LastHistEffort <- ProjSim@Effort[1,ProjYearInd[1]-1,, drop=FALSE]
  LastHistEffort <- matrix(LastHistEffort, length(ProjYears), nFleet, byrow=TRUE)
  LastHistEffort <- LastHistEffort |> AddDimension('Sim', 1)
  
  ProjSim@Effort[1,ProjYearInd,] <- LastHistEffort * exp(logScalar)
  
  ProjSim_opt <- CalcFisheryDynamics(Hist = ProjSim,
                                     Years = c(tail(HistYears, ProjSim@OM@Seasons), ProjYears), 
                                     DoCalcaggF = 0,
                                     IdenticalSim = IdenticalSim )
  
  # summed over age, fleet, area
  Yield <- GetCatch(ProjSim_opt, Units, type) 
  
  lastnTS <- ProjSim@OM@Control$RefYield$lastnTS
  if (is.null(lastnTS))
    lastnTS <- 5
  
  dd <- dim(Yield)
  nTS <- dd[3]
  if (lastnTS >nTS)
    lastnTS <- nTS
  
  TSmean <- (nTS-lastnTS+1):nTS
  mean_Yield <- apply(Yield[1,,TSmean], "Stock", mean)
  obj <- -sum(mean_Yield) 
  
  if (opt==1) {
    return(obj)
  }
  # return Yields
  mean_Yield
  
}


