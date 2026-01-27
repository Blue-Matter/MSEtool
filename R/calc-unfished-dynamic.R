#' Calculate Dynamic Unfished
#' 
#' Calculates the dynamic unfished number, biomass, spawning biomass, and 
#' spawning production for all [Stock()] objects in an [OM()]
#' 
#' @param OM A [Hist] or an [OM()] object
#' 
#' @return A [PopDynamics] object with dynamic unfished Number, Biomass, SBiomass, and SProduction
#' @export
CalcUnfished_Dynamic <- function(Hist) {
  
  if (inherits(OM,'om')) {
    Hist <- OM2Hist(OM, silent=TRUE)
    Hist@Unfished@Equilibrium <- CalcUnfished_Equilibrium(OM)
    Hist <- CalcDynamicInitial(Hist)
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
  unfished <- CalcFisheryDynamics_(Hist_Copy, 
                                   Years=HistYears,
                                   AllYears=AllYears,
                                   nSim=Hist@OM@nSim,
                                   nStock,
                                   nFleet,
                                   nArea,
                                   DoCalcCatch=0,
                                   DoCalcaggF=0)
  
  
  out <- new("popdynamics")
  for (sl in slotNames(out)) {
    slot(out, sl) <- slot(unfished, sl)
  }
  out
  
}