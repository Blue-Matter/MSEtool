

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
  
  # Create Time Series Arrays
  Hist <- InitializeTimeSeries(Hist)
  
  # Add values included in Misc
  # These values won't be over-written by the model 
  # TODO - new feature not used or testedd
  Hist <- FillFromMisc(Hist)
  
  if (!silent) {
    cli::cli_progress_done()
  }
    
  
  Hist
}
  

InitializeTimeSeries <- function(Hist)  {
  OM <- Hist@OM
  
  # List of Stocks - Number by Sim, Age, Year, and Area
  Hist@Number <- ListArraySimAgeTimeArea(OM, 'Historical') 
  
  # Arrays: Sim, Stock, Year
  Hist@Biomass <- ListArraySimAgeTime(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> 
    aperm(c('Sim', 'Stock', 'Year'))
  Hist@SBiomass <-  Hist@SProduction <- Hist@Biomass 
  
  # Landings and Discards by Age and Size 
  # List of Stocks - array Sim, Age, Year, Fleet, Area
  Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  # List of Stocks - list of Fleets - array Sim, Class, Year, Area
  Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetArea(OM, 'Historical')
  
  # Historical Fishing Effort - Total
  # Sim, Stock, Year, Fleet
  Hist@Effort <- ListArraySimAgeTimeFleet(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year', 'Fleet'))
  
  # Add Effort from OM
  for (st in 1:nStock(OM)) {
    for (fl in 1:nFleet(OM)) {
      Hist@Effort[,st,,fl] <- Hist@OM@Fleet[[st]][[fl]]@Effort@Effort  
    }
  }
  
  # Effort Distribution over Areas - effort by area
  # Sim, Stock, Year, Fleet, Area
  Hist@Distribution <- ListArraySimAgeTimeFleetArea(OM, 'Historical') |> 
    purrr::map(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year', 'Fleet', 'Area'))
  
  # Add Distribution from OM
  for (st in 1:nStock(OM)) {
    for (fl in 1:nFleet(OM)) {
      Hist@Distribution[,st,,fl,] <- Hist@OM@Fleet[[st]][[fl]]@Effort@Distribution[,1:nYears,,drop=FALSE] 
    }
  }
  
  # Fishing Mortality - Dead and Retain 
  # Overall
  # List of Stocks - array Sim, Age, Year, Fleet
  Hist@FDead <- Hist@FRetain <- ListArraySimAgeTimeFleet(OM, 'Historical') 
  
  # Within Area
  # List of Stocks - array Sim, Age, Year, Fleet, Area 
  Hist@FDeadArea <- Hist@FRetainArea <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  Hist
}

 