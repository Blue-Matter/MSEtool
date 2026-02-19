UpdateMSEObject <- function(MSE, Proj, MPName, mp, YearsHist, YearsProj,
                            StockNames, FleetNames) {
  
  YearsAll <- c(YearsHist, YearsProj)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    MSE@Number[[st]][,,,,mp] <- Proj@Number[[st]] |> SubsetYear(YearsProj)
  
    MSE@InteractAtAge[[st]][,,,,,mp] <- Proj@InteractAtAge[[st]] |> SubsetYear(YearsProj)
    MSE@LandingsAtAge[[st]][,,,,,mp] <- Proj@LandingsAtAge[[st]] |> SubsetYear(YearsProj)
    MSE@DiscardsAtAge[[st]][,,,,,mp] <- Proj@DiscardsAtAge[[st]] |> SubsetYear(YearsProj)
    
    # MSE@LandingsAtSize[[st]][,,,,,mp] <- Proj@LandingsAtSize[[st]] |> SubsetYear(YearsProj)
    # MSE@DiscardsAtSize[[st]][,,,,,mp] <- Proj@LandingsAtSize[[st]] |> SubsetYear(YearsProj)
    
    MSE@FInteractArea[[st]][,,,,,mp] <- Proj@FInteractArea[[st]] |> SubsetYear(YearsProj)
    MSE@FDeadArea[[st]][,,,,,mp] <- Proj@FDeadArea[[st]] |> SubsetYear(YearsProj)
    MSE@FRetainArea[[st]][,,,,,mp] <- Proj@FRetainArea[[st]] |> SubsetYear(YearsProj)
    
  }
  
  MSE@Biomass[,,,mp] <- Proj@Biomass |> SubsetYear(YearsProj)
  MSE@SBiomass[,,,mp] <- Proj@SBiomass |> SubsetYear(YearsProj)
  MSE@SProduction[,,,mp] <- Proj@SProduction |> SubsetYear(YearsProj)
  
  MSE@Interactions[,,,,mp] <- Proj@Interactions |> SubsetYear(YearsProj)
  MSE@Landings[,,,,mp] <- Proj@Landings |> SubsetYear(YearsProj)
  MSE@Discards[,,,,mp] <- Proj@Discards |> SubsetYear(YearsProj)
  
  MSE@Effort[,,,mp] <- Proj@Effort |> SubsetYear(YearsProj)
  MSE@Distribution[,,,,mp] <- Proj@Distribution |> SubsetYear(YearsProj)
  
  MSE@FInteract[,,,,mp] <- Proj@FInteract |> SubsetYear(YearsProj)
  MSE@FDead[,,,,mp] <- Proj@FDead |> SubsetYear(YearsProj)
  MSE@FRetain[,,,,mp] <- Proj@FRetain |> SubsetYear(YearsProj)
  
  # Misc 
  # keep MPAdvice 
  if (is.null(MSE@Misc$Advice)) 
    MSE@Misc$Advice <- list()
  
  MSE@Misc$Advice[[MPName]] <- Proj@Misc$MPAdvice

  
  # TODO 
  # MSE@Misc$Failed[[MPName]] <- lapply(SimList_MP, slot, 'Misc') |>   lapply("[[", "Failed") |> 
  #   unlist() |> 
  #   as.numeric()

  MSE <- AddPPD(MSE, Proj, MPName, YearsProj)    
  # TODO 

  # MSE <- MSE |> 
  #   KeepRetention(Proj, mp) |>
  #   KeepSelectivity(Proj, mp) |> 
  #   KeepDiscardMortality(Proj, mp) |>
  #   AddPPD(Proj, mp) |>
  #   ProcessLogMSE(Proj, mp, MP)
  
  MSE
  
  
  
}

AddPPD <- function(MSE, Proj, MPName, YearsProj) {
  PPD <- Proj@Data |> SubsetYear(YearsProj)
  MSE@PPD[[MPName]] <- PPD
  MSE
}