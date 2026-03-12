# TODO - make parallel version

#' Calculate Reference Yield
#'
#' Internal function to calculate reference yields in terms of either 
#' `Landings` and/or `Removals`.
#'
#' Reference yield is calculated as the highest yield (in units of `Units`) 
#' summed across all fleets for a given fixed F policy over the entire 
#' projection period. 
#'
#' @param Hist `hist` class object containing historical fishery dynamics
#' @param type Character vector; one or both of `Landings` and `Removals`
#' @param Units Character; either `Biomass` or `Number`
#' @param silent Logical; if `TRUE`, suppress progress bars
#'
#' @return Updated `Hist` object with reference yields stored in
#'   `Hist@Reference$RefLandings` and/or `Hist@Reference$RefRemovals`
#'
#' @name calc-ref-yield
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
  AllYears <- c(HistYears, ProjYears)
  nSim <- Hist@OM@nSim
  StockNames <- StockNames(Hist)
  nStock <- length(StockNames)
  nFleet <- nFleet(Hist)
  
  # Extend Year dimensions to include ProjYears
  Proj <- Hist
  Proj@OM@Stock <- Extend(Proj@OM@Stock, Years=AllYears)
  Proj@OM@Fleet <- Extend(Proj@OM@Fleet, Years=AllYears)
  Proj@Misc <- Extend(Proj@Misc, Years=AllYears)
  
  for (sl in slotNames('timeseries')) {
    slot(Proj,sl) <- Extend(slot(Proj,sl), Years=AllYears, default=0) 
  }
  
  ProjYearInd <- match(ProjYears, AllYears)
  LastHistEffort <- Proj@Effort[, ProjYearInd[1]-1, , drop = FALSE]
  
  # List length nSim, each with a Hist object with 1 sim
  
  for (t in type) {
    RefYield <- vector("list", nSim)
    
    if (!silent) 
      cli::cli_progress_bar(format = "Calculating Reference {.val {t}} {cli::pb_bar} {cli::pb_percent}",  total = nSim)
    
    for (sim in seq_len(nSim)) {
      baseEffort <- LastHistEffort[sim,, , drop = FALSE]
      RefYield[[sim]] <- vector("numeric", nStock)
      # Optimize F scalar for this sim
      DoOpt <- optimize(f = function(logScalar) {
        OptRefYield(logScalar,
                    Proj = Proj,
                    sim = sim,
                    HistYears = HistYears,
                    ProjYears = ProjYears,
                    ProjYearInd = ProjYearInd,
                    nFleet = nFleet,
                    Units = Units,
                    type = t,
                    baseEffort = baseEffort,
                    debug = 0,
                    opt = 1)
      }, interval = log(c(1e-5, 10)))
      
      # Get final yield using optimized scalar
      RefYield[[sim]] <- OptRefYield(DoOpt$minimum,
                                     Proj = Proj,
                                     sim = sim,
                                     HistYears = HistYears,
                                     ProjYears = ProjYears,
                                     ProjYearInd = ProjYearInd,
                                     nFleet = nFleet,
                                     Units = Units,
                                     type = t,
                                     baseEffort = baseEffort,
                                     debug = 0,
                                     opt = 2)
      
      if (!silent) cli::cli_progress_update()
      
    } # end sim loop 
    
    if (!silent) cli::cli_progress_done()
    
    # Convert list of vectors to array sim × stock
    RefYield <- List2Array(RefYield, "Sim", "Stock")[1,,, drop=FALSE] |> abind::adrop(1) |> t()
    dimnames(RefYield)[['Stock']] <- StockNames
    slot(Hist@Reference, t) <- RefYield
    
  } # end type=c('Landings', 'Removals') loop
  
  if (!silent) cli::cli_alert_success("Calculated Reference {.val {type}}")
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
#' @param Proj `hist` class object containing historical fishery dynamics
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
                        Proj,
                        sim,
                        HistYears, 
                        ProjYears, 
                        ProjYearInd,
                        nFleet,
                        Units,
                        type,
                        baseEffort,
                        debug = 0,
                        opt = 1) {
  
  # Scale historical effort
  scaledEffort <- baseEffort * exp(logScalar)
  Proj@Effort[sim, ProjYearInd, ] <- scaledEffort
  
  # Run fishery dynamics for this sim only
  ProjSim_opt <- CalcFisheryDynamics(Hist = Proj,
                                     Sims = sim,
                                     Years = c(utils::tail(HistYears, Proj@OM@Seasons), ProjYears),
                                     debug = debug)
  
  # Get total catch summed over age, fleet, area
  Yield <- GetCatch(ProjSim_opt, Units, type)
  
  # Take mean over last few years
  lastnTS <- Proj@OM@Control$RefYield$lastnTS %||% 5
  lastnTS <- min(lastnTS, dim(Yield)[3])
  TSmean <- (dim(Yield)[3]-lastnTS+1):dim(Yield)[3]
  
  # Compute mean yield by stock (over years)
  mean_Yield <- rowMeans(Yield[sim, , TSmean, drop = FALSE], dims = 2)
  
  if (opt == 1) return(-sum(mean_Yield))
  mean_Yield
}

