#' Calculate Reference Yield
#'
#' Internal function to calculate reference yields in terms of either 
#' `Landings` and/or `Removals`.
#'
#' Reference yield is calculated as the highest yield (in units of `Units`) 
#' summed across all fleets for a given fixed F policy over the entire 
#' projection period. 
#'
#' @param Hist `Hist` object containing historical fishery dynamics
#' @param type Character vector; one or both of `Landings` and `Removals`
#' @param Units Character; either `Biomass` or `Number`
#' @param silent Logical; if `TRUE`, suppress progress bars
#'
#' @return Updated `Hist` object with reference yields stored in
#'   `Hist@Reference$RefLandings` and/or `Hist@Reference$RefRemovals`
#'
#' @keywords internal
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
  
  log_bounds <- log(c(1E-5, 10))

  for (t in type) {
    if (silent) {
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
        
        yield <- OptRefYield(logScalar = DoOpt$minimum, 
                             ProjSim = ProjSim,
                             HistYears = HistYears,
                             ProjYears = ProjYears,
                             nFleet = nFleet,
                             Units = Units,
                             type = t,
                             opt = 2)
        
        
        yield
      
      })
      
    } else {
      RefYield <- purrr::map(ProjSim_List, \(ProjSim) {
        
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
    
    RefYield <- List2Array(RefYield, "Sim", "Stock") |> t()
    dimnames(RefYield)[['Stock']] <- StockNames
    slot(Hist@Reference, paste0("Ref",t)) <- RefYield
    
  }
  Hist
}

#' Optimize Reference Yield for a Single Simulation
#'
#' Internal helper function called by `CalcRefYield()` to optimize
#' fishing effort for a single simulation replicate.
#'
#' Scales historical effort by a log scalar, projects forward,
#' calculates total yield (summed over age, fleet, and area),
#' and returns either the negative objective function for optimization
#' or the mean yield per stock.
#'
#' @param logScalar Numeric scalar applied to scale historical effort
#' @param ProjSim Single-simulation subset of the operating model
#' @param HistYears Numeric vector of historical years
#' @param ProjYears Numeric vector of projection years
#' @param nFleet Integer number of fleets
#' @param Units Character; either `Biomass` or `Number`
#' @param type Character; either `Landings` or `Removals`
#' @param opt Integer; if 1, return objective for optimization; if 2, return yields
#'
#' @return Numeric; either negative sum of mean yields (opt=1) or vector of mean yields per stock (opt=2)
#'
#' @keywords internal
OptRefYield <- function(logScalar, 
                        ProjSim, 
                        HistYears, 
                        ProjYears, 
                        nFleet,
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
                                     IdenticalSim = FALSE)
  
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


