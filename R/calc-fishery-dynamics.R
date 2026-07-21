#' Calculate Fishery Dynamics
#'
#' Internal wrapper around C++ `CalcFisheryDynamics_`
#'
#' @param Hist A [hist-class] object.
#' @param Years Numeric vector of years to calculate dynamics over. Defaults to
#'   historical years from `Hist@OM`.
#' @param Sims Integer vector of simulation indices to compute. Defaults to all
#'   simulations.
#' @param DoCalcCatch Integer flag; `1` (default) calculates catch, `0` skips.
#' @param DoCalcSpawnProduction Integer flag; `1` (default) calculates global
#'   spawning biomass and spawning production, `0` skips.
#' @param DoCalcRecruitment Integer flag; `1` (default) calculates recruitment
#'   and distributes recruits over areas, `0` skips.
#' @param DoCalcNumberNext Integer flag; `1` (default) calculates numbers at
#'   the beginning of the next time step, `0` skips.
#' @param DoCalcBiomass Integer flag; `1` (default) calculates biomass for the
#'   current time step, `0` skips.
#' @param DoCalcOverallF Integer flag; `1` (default) calculates overall fishing
#'   mortality, `0` skips.
#' @param IdenticalSim Logical; if `TRUE`, computes only simulation 1 and
#'   broadcasts results to all simulations via `.CopyFirstSim`.
#' @param debug Integer flag passed to C++; `1` enables verbose debug output.
#' @param clone Integer or `NULL`; controls whether the internal `Hist` object
#'   is deep-cloned before modification. `1` = always clone (safe, slower),
#'   `0` = no clone (faster, mutates `HistIn` directly). If `NULL`, uses
#'   `Hist@OM@Control$Clone` if set, otherwise defaults to `0`.
#'
#' @return The `Hist` object with updated fishery dynamics slots.
#' @keywords internal
.CalcFisheryDynamics <- function(Hist, 
                                Years=NULL,
                                Sims=NULL,
                                DoCalcCatch=1,
                                DoCalcSpawnProduction=1,
                                DoCalcRecruitment=1,
                                DoCalcNumberNext=1,
                                DoCalcBiomass=1,
                                DoCalcOverallF=1,
                                IdenticalSim=FALSE,
                                debug=0,
                                clone=NULL) {
  
  nStock   <- nStock(Hist)
  nFleet   <- nFleet(Hist)
  nArea    <- nArea(Hist)
  nSim     <- nSim(Hist)
  AllYears <- Years(Hist@OM)
  if (!is.null(Hist@OM@Control$Clone)) {
    clone <- Hist@OM@Control$Clone
  } else if (is.null(clone)) {
    clone <- 0L
  }
  
  if (is.null(Years)) 
    Years <- Years(Hist@OM,'H')
  
  if (is.null(Sims)) 
    Sims <- seq_len(nSim)
  
  if (IdenticalSim) { 
    # Compute for sim 1 only, then broadcast to all sims
    Hist_1 <- CalcFisheryDynamics_(HistIn=Hist, 
                                   Years=Years,
                                   AllYears=AllYears,
                                   Sims=1,
                                   nSim=nSim,
                                   nStock=nStock,
                                   nFleet=nFleet,
                                   nArea=nArea,
                                   DoCalcCatch=DoCalcCatch,
                                   DoCalcSpawnProduction=DoCalcSpawnProduction,
                                   DoCalcRecruitment=DoCalcRecruitment,
                                   DoCalcNumberNext=DoCalcNumberNext,
                                   DoCalcBiomass=DoCalcBiomass,
                                   DoCalcOverallF=DoCalcOverallF,
                                   debug=debug,
                                   clone=1L)
    
    ts_slots <- slotNames('timeseries')
    for (sl in ts_slots) {
      if (sl == 'Misc') next
      slot(Hist, sl) <- .CopyFirstSim(x=slot(Hist_1, sl))
    }
    return(Hist)
  }
  
  # Full sim-dependent calculation
  CalcFisheryDynamics_(HistIn=Hist,
                       Years=Years,
                       AllYears=AllYears,
                       Sims=Sims,
                       nSim=nSim,
                       nStock=nStock,
                       nFleet=nFleet,
                       nArea=nArea,
                       DoCalcCatch=DoCalcCatch,
                       DoCalcSpawnProduction=DoCalcSpawnProduction,
                       DoCalcRecruitment=DoCalcRecruitment,
                       DoCalcNumberNext=DoCalcNumberNext,
                       DoCalcBiomass=DoCalcBiomass,
                       DoCalcOverallF=DoCalcOverallF,
                       debug=debug,
                       clone=clone)
}
