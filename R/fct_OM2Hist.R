

# Initialize a `Hist` object for a given `OM`
OM2Hist <- function(OM, silent=FALSE) {
  
  if (!silent) {
    id <- cli::cli_progress_bar("Initializing `Hist` Object")  
  }
    
  # Populate if needed 
  OM <- PopulateOM(OM, silent=silent) 
  
  Hist <- new('hist')
  Hist@OM <- OM
  HistYears <- Years(OM, 'Historical')
  nYears <- length(HistYears)
  nSim <- OM@nSim
  

  # Stock - expand all arrays to include all Sims and all Years
  # TODO - this could be improved later by keeping all arrays at the minimum
  #        size and updating the code to match Sim/Year
  Hist@OM@Stock <- purrr::map(OM@Stock, \(Stock) {
    Stock <- ExtendStock(Stock, nSim, HistYears, silent, id)
    Stock@SRR@SPFrom <- match(Stock@SRR@SPFrom, StockNames(OM))
    Stock
  })
  

  # Fleet
  # Extend Fleet arrays to include all Sims and historical years
  AgeClassList <- purrr::map(Hist@OM@Stock, \(Stock) Stock@Ages@Classes)
  Hist@OM@Fleet <- purrr::map2(Hist@OM@Fleet, AgeClassList, \(FleetList, AgeClasses)
                               ExtendFleet(FleetList, AgeClasses, nSim, HistYears, silent, id)
  )
  
  # Time Series 
  
  
  # UP TO HERE
  # ---------------------- DEBUG ----------------------
  
  
  
  
  
  stop()
  # -------------------- END DEBUG --------------------
  
  
  
  Hist@Number <- ListArraySimAgeTimeArea(OM, 'Historical') 
  Hist@Biomass <- ListArraySimAgeTime(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> 
    aperm(c('Sim', 'Stock', 'Year'))
  Hist@SBiomass <-  Hist@SProduction <- Hist@Biomass 
  
  Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetArea(OM, 'Historical')
  
  Hist@Effort <- ListArraySimAgeTimeFleet(OM, 'Historical') |> lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year', 'Fleet'))
  
  # Add Effort from OM
  for (st in 1:nStock(OM)) {
    Hist@Effort[,st,,] <- Hist@OM@Fleet[[st]]@Effort@Value
  }
  
  Hist@Distribution <- ListArraySimAgeTimeFleetArea(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE)
  
  # Add Distribution from OM
  for (st in 1:nStock(OM)) {
    Hist@Distribution[[st]] <- Hist@OM@Fleet[[st]]@Effort@Distribution[,1:nYears,,,drop=FALSE] 
  }
  
  Hist@FDead <-  Hist@FRetain <- ListArraySimAgeTimeFleet(OM, 'Historical') 
  Hist@FDeadArea <-  Hist@FRetainArea  <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  
  Hist <- ProcessMiscOM(Hist)
  
  if (!silent) 
    cli::cli_progress_done()
  
  Hist
}
  


 