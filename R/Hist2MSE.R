Hist2MSE <- function(Hist, MPs) {
  MSE <- new('mse')
  MSE@OM <- Hist@OM
  MSE@Unfished <- Hist@Unfished
  MSE@RefPoints <- Hist@RefPoints
  

  slots <- slotNames(MSE@Hist)
  for (sl in slots)  {
    slot(MSE@Hist, sl) <- slot(Hist, sl)
  }
  
  MPNames <- MPs
  MSE@MPs <- lapply(MPs, get)
  names(MSE@MPs) <- MPNames
  attributes(MSE@MPs)$complete <- rep(FALSE, length(MPNames))
  
  MSE@Number <- ListArraySimAgeTimeAreaMP(Hist@OM, "Projection", MPNames)
  MSE@Biomass <- ArraySimStockTimeMP(Hist@OM, "Projection", MPs=MPNames)
  
  MSE@SBiomass <- MSE@SProduction <- MSE@Biomass
  
  MSE@Removals <- ArraySimStockTimeFleetMP(Hist@OM, "Projection", MPs=MPNames)
  
  MSE@Landings <-  MSE@Removals
  
  MSE@Effort <- ArraySimStockTimeFleetMP(Hist@OM, "Projection", MPs=MPNames)
  
  MSE@FDeadAtAge <- ListArraySimAgeTimeFleetMP(Hist@OM, "Projection", MPNames)
  MSE@FRetainAtAge <-  MSE@FDeadAtAge
  
  MSE@EffortArea <- ListArraySimTimeFleetAreaMP(Hist@OM, "Projection", MPNames)
  
  MSE@FDeadAtAgeArea <- ListArraySimAgeTimeFleetAreaMP(Hist@OM, "Projection", MPNames)
  MSE@FRetainAtAgeArea <- MSE@FDeadAtAgeArea
  
  MSE
}