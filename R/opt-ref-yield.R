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

