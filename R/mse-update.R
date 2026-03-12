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

  MSE <- AddPPD(MSE, Proj, MPName, YearsHist, YearsProj)    
  
  MSE <- RecordSelRetDisc(MSE, Proj, MPName, YearsProj)
  
  MSE
}


#' Record Selectivity, Retention, and Discard Mortality
#' Records changes in selectivity, retention, and discard mortality
#'   for each fleet and MP relative to the original OM, storing only slots
#'   where values differ. Returns \code{MSE} unchanged for a given slot if all
#'   fleet/area comparisons are identical.
#' @param MSE An MSE object.
#' @param Proj A projection object containing updated fleet parameters.
#' @param MPName Character. Name of the management procedure.
#' @param YearsProj Integer vector of projection year indices to subset.
#' @return The `MSE` object with `MSE@Misc[["Selectivity"]]`,
#'    `MSE@Misc[["Retention"]]`, and `MSE@Misc[["DiscardMortality"]]`
#'   updated for ``MPName`.
#' @keywords internal
RecordSelRetDisc <- function(MSE, Proj, MPName, YearsProj) {
  for (slotname in c('Selectivity', 'Retention', 'DiscardMortality')) {
    result <- purrr::map2(Proj@OM@Fleet, MSE@OM@Fleet, \(fleetlist, origfleetlist) {
      
      purrr::map2(fleetlist, origfleetlist,  \(fleet, origfleet) {
        
        obj      <- slot(fleet, slotname)
        obj_orig <- slot(origfleet, slotname)
        
        MeanAtAge    <- obj@MeanAtAge |> SubsetYear(YearsProj) |> ExtendSims(nSim=MSE@OM@nSim)
        MeanAtLength <- obj@MeanAtLength |> SubsetYear(YearsProj) |> ExtendSims(nSim=MSE@OM@nSim)
        MeanAtAge_orig    <- obj_orig@MeanAtAge |> SubsetYear(YearsProj) |> ExtendSims(nSim=MSE@OM@nSim)
        MeanAtLength_orig <- obj_orig@MeanAtLength |> SubsetYear(YearsProj) |> ExtendSims(nSim=MSE@OM@nSim)
        
        out <- list()
        if (!prod(MeanAtAge == MeanAtAge_orig)) out$MeanAtAge <- MeanAtAge
        
        if (all(dim(MeanAtLength) == dim(MeanAtLength_orig))) {
          if (!prod(MeanAtLength == MeanAtLength_orig)) out$MeanAtLength <- MeanAtLength  
        } else {
          # different number of classes 
          out$MeanAtLength <- MeanAtLength
        }
        if (!length(out)) return(NULL)
        out
      })
    })
    # Only record if at least one fleet/area has non-NULL differences
    all_null <- all(purrr::map_lgl(purrr::list_flatten(result), is.null))
    if (!all_null)
      MSE@Misc[[slotname]][[MPName]] <- result
  }
  MSE
}

AddPPD <- function(MSE, Proj, MPName, YearsHist, YearsProj) {
  
  PPD <- Proj@Data |> 
    AddYearDimnames(Years=c(YearsHist, YearsProj)) |>
    AddFleetDimnames(FleetNames = FleetNames(Proj)) |>
    SubsetYear(Years=YearsProj)
  
  MSE@PPD[[MPName]] <- PPD
  MSE
}
