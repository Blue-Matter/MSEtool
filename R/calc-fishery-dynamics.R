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
#' @param DoCalcTransition Integer flag; `1` (default) applies age-dependent
#'   reclassification of numbers-at-age between stocks (e.g. `Herm`), `0`
#'   skips. Has no effect unless `Hist@OM@Herm` is set.
#' @param DoCalcBiomass Integer flag; `1` (default) calculates biomass for the
#'   current time step, `0` skips.
#' @param DoCalcOverallF Integer flag; `1` (default) calculates overall fishing
#'   mortality, `0` skips.
#' @param DoBackCalcEffort Integer flag; `1` overwrites `Hist@Effort` with the
#'   effort implied by the F actually realised, `0` (default) leaves the
#'   requested effort. Unlike the other flags this defaults to *off*: the effort
#'   solvers call this function thousands of times while probing, and rewriting
#'   effort underneath them would corrupt their finite differences whenever the
#'   `maxF` clamp binds. Enable it only on the calls that advance the
#'   simulation, via `.BackCalcEffortFlag()`.
#' @param IdenticalSim Logical; if `TRUE`, computes only simulation 1 and
#'   broadcasts results to all simulations via `.CopyFirstSim`.
#' @param debug Integer flag passed to C++; `1` enables verbose debug output.
#' @param InPlace Logical. `FALSE` (default) returns new copies of every slot
#'   the engine writes, so `Hist` and any other object sharing its arrays are
#'   unchanged. `TRUE` writes the output slots (biomass, catch and fishing
#'   mortality) into `Hist`'s own arrays, and is only safe when no other
#'   object shares those arrays. The state slots (`Number`, `Effort`,
#'   `Distribution`, `SProduction`) are always copied.
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
                                DoCalcTransition=1,
                                DoCalcBiomass=1,
                                DoCalcOverallF=1,
                                DoBackCalcEffort=0,
                                IdenticalSim=FALSE,
                                debug=0,
                                InPlace=FALSE) {
  
  nStock   <- nStock(Hist)
  nFleet   <- nFleet(Hist)
  nArea    <- nArea(Hist)
  nSim     <- nSim(Hist)
  AllYears <- Years(Hist@OM)
  
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
                                   DoCalcTransition=DoCalcTransition,
                                   DoCalcBiomass=DoCalcBiomass,
                                   DoCalcOverallF=DoCalcOverallF,
                                   DoBackCalcEffort=DoBackCalcEffort,
                                   debug=debug)
    
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
                       DoCalcTransition=DoCalcTransition,
                       DoCalcBiomass=DoCalcBiomass,
                       DoCalcOverallF=DoCalcOverallF,
                       DoBackCalcEffort=DoBackCalcEffort,
                       debug=debug,
                       InPlace=as.integer(InPlace))
}

# Slots written by the engine that `InPlace = TRUE` overwrites; must match OutputSlots in calc_fishery_dynamics.cpp
.DynamicsOutputSlots <- c(
  "Biomass", "SBiomass", "Interactions", "Landings", "Discards",
  "InteractAtAge", "LandingsAtAge", "DiscardsAtAge",
  "FInteract", "FDead", "FRetain", "FInteractArea", "FDeadArea", "FRetainArea"
)

# Gives `Hist` its own copy of the output slots so later InPlace calls cannot change any other object
.OwnDynamicsSlots <- function(Hist) {
  for (sl in .DynamicsOutputSlots)
    methods::slot(Hist, sl) <- rlang::duplicate(methods::slot(Hist, sl), shallow = FALSE)
  Hist
}

.BackCalcEffortFlag <- function(Hist) {
  ctl <- Hist@OM@Control$BackCalcEffort
  if (is.null(ctl)) return(1L)
  as.integer(isTRUE(ctl))
}
