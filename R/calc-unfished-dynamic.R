#' Calculate Dynamic Unfished Population Dynamics
#'
#' Calculate dynamic unfished population quantities for all [Stock()]
#' objects in an operating model. Quantities include unfished number-at-age,
#' total biomass, spawning biomass, and spawning production, evaluated dynamically
#' over historical and projection years.
#'
#'
#' @param Hist A [Hist()] object containing historical operating model data.
#' @param IdenticalHist Logical; if `TRUE`, assumes all simulations are identical
#'   and calculates only a single simulation. If `NULL` (default), determined
#'   automatically via `IdenticalSims()`.
#' @param silent Logical; if `TRUE`, suppress messages during calculation.
#'
#' @return A [PopDynamics] object containing dynamic unfished
#'   `Number`, `Biomass`, `SBiomass`, and `SProduction` arrays with dimensions
#'   `Sim × Stock × Year`.
#'
#' @seealso [CalcUnfished_Equilibrium()]
#'
#' @export

CalcUnfished_Dynamic <- function(Hist, IdenticalHist=NULL, silent=FALSE) {
  
  if (inherits(Hist,'om')) {
    Hist <- Hist(OM, silent=TRUE)
  }
  
  if (EmptyObject(Hist@Unfished@Equilibrium )) {
    Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(Hist@OM)
  }
  
  Hist <- CalcDynamicInitial(Hist)
  
  if (is.null(Hist@Misc$SAVE)) {
    Hist <- PrepHistMisc(Hist) 
  }
 
  Hist_Copy <- Hist 
  nStock <- nStock(Hist)
  nFleet <- nFleet(Hist)
  nArea <- nArea(Hist)
  
  for (st in 1:nStock) {
    for (fl in 1:nFleet) {
      Hist_Copy@OM@Fleet[[st]][[fl]]@Catchability@Efficiency[] <- tiny
    }
  }
  
  HistYears <- Years(Hist, 'H')
  AllYears <- Years(Hist)
  
  if (is.null(IdenticalHist)) {
    IdenticalHist <- IdenticalSims(Hist@OM, ignore='SRR')
  }

  out <- new("popdynamics")
  
  if (IdenticalHist) {
    # run for just sim 1
    Hist_Copy_1 <- Subset(Hist_Copy, 1)
    
    unfished <- CalcFisheryDynamics_(Hist_Copy_1, 
                                       Years=HistYears,
                                       AllYears=AllYears,
                                       nSim=1,
                                       nStock,
                                       nFleet,
                                       nArea,
                                       DoCalcCatch=0,
                                       DoCalcaggF=0)
  } else {
    unfished <- CalcFisheryDynamics_(Hist_Copy, 
                                     Years=HistYears,
                                     AllYears=AllYears,
                                     nSim=Hist@OM@nSim,
                                     nStock,
                                     nFleet,
                                     nArea,
                                     DoCalcCatch=0,
                                     DoCalcaggF=0)
  
  }
  for (sl in slotNames(out)) {
    slot(out, sl) <- slot(unfished, sl)
  }
  
  if (!silent) {
    cli::cli_alert_success("Calculated Dynamic Unfished Conditions")
  }
  
  out
}