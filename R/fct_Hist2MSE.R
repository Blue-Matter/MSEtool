Hist2MSE <- function(Hist, MPs) {
  MSE <- new('mse')
  MSE@OM <- Hist@OM
  MSE@Unfished <- Hist@Unfished
  MSE@Reference <- Hist@Reference
  
  
  slots <- slotNames(MSE@Hist)
  for (sl in slots)  {
    slot(MSE@Hist, sl) <- slot(Hist, sl)
  }
  
  MPNames <- MPs
  MSE@MPs <- lapply(MPs, get)
  names(MSE@MPs) <- MPNames
  # attributes(MSE@MPs)$complete <- rep(FALSE, length(MPNames))
  
  MSE@Number <- ListArraySimAgeTimeAreaMP(Hist@OM, "Projection", MPNames)
  MSE@Biomass <- ArraySimStockTimeMP(Hist@OM, "Projection", MPs=MPNames)
  
  MSE@SBiomass <- MSE@SProduction <- MSE@Biomass
  
  MSE@Landings <- ArraySimStockTimeFleetMP(Hist@OM, 'Projection', MPNames)
  MSE@Discards <-  MSE@Landings 
  
  MSE@LandingsAtAge <- ListArraySimAgeTimeFleetAreaMP(Hist@OM, 'Projection', MPNames)
  MSE@DiscardsAtAge <-  MSE@LandingsAtAge
  
  MSE@Effort <- ArraySimStockTimeFleetMP(Hist@OM, "Projection", MPs=MPNames)
  
  MSE@FDead <- MSE@FRetain <- ListArraySimAgeTimeFleetMP(Hist@OM, "Projection", MPNames)
  MSE@Distribution <- ListArraySimTimeFleetAreaMP(Hist@OM, "Projection", MPNames)
  
  MSE@FDeadArea <- ListArraySimAgeTimeFleetAreaMP(Hist@OM, "Projection", MPNames)
  MSE@FRetainArea <- MSE@FDeadArea
  
  MSE
}