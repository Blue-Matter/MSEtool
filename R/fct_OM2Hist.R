
OM2Hist <- function(OM, silent=FALSE, id=NULL) {
  
  if (!silent) 
    id <- cli::cli_progress_bar("Initializing `Hist` Object")  
  
  OM <- PopulateOM(OM, silent=silent)
  
  Hist <- new('hist')
  Hist@OM <- OM
  Years <- Years(OM, 'Historical')
  nArea <- nArea(OM)
  nYears <- length(Years)
  nSim <- OM@nSim
  
  # Stock
  Hist@OM@Stock <- purrr::map(OM@Stock, \(Stock) {
    Stock <- ExtendStock(Stock, nSim, Years, silent, id)
    Stock@SRR@SPFrom <- match(Stock@SRR@SPFrom, StockNames(OM))
    Stock
  })
                              
  # Fleet
  AgeClassList <- purrr::map(Hist@OM@Stock, \(Stock) 
                             Stock@Ages@Classes)
  
  Hist@OM@Fleet <- purrr::map2(Hist@OM@Fleet, AgeClassList, \(FleetList, AgeClasses)
                               ExtendFleet(FleetList, AgeClasses, nSim, Years, nArea, silent, id)
  )
  
  # Time Series 
  Hist@Number <- ListArraySimAgeTimeArea(OM, 'Historical') 
  Hist@Biomass <- ListArraySimAgeTime(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> 
    aperm(c('Sim', 'Stock', 'Year'))
  Hist@SBiomass <-  Hist@SProduction <- Hist@Biomass 
  Hist@Landings <- Hist@Discards <- ArraySimStockTimeFleetMP(OM,'Historical',1) |> DropDimension('MP')
  
  Hist@LandingsAtAge <- Hist@DiscardsAtAge <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  
  Hist@LandingsAtSize <- Hist@DiscardsAtSize <- ListArraySimClassTimeFleetArea(OM, 'Historical')
  
  Hist@Effort <- ListArraySimAgeTimeFleet(OM, 'Historical') |> lapply(DropDimension, 'Age', FALSE) |>
    List2Array('Stock') |> aperm(c('Sim', 'Stock', 'Year', 'Fleet'))
  
  # Add Effort from OM
  for (st in 1:nStock(OM)) {
    Hist@Effort[,st,,] <- Hist@OM@Fleet[[st]]@Effort
  }
  
  Hist@Distribution <- ListArraySimAgeTimeFleetArea(OM, 'Historical') |> 
    lapply(DropDimension, 'Age', FALSE)
  
  # Add Distribution from OM
  for (st in 1:nStock(OM)) {
    Hist@Distribution[[st]] <- Hist@OM@Fleet[[st]]@Distribution[,1:nYears,,,drop=FALSE] 
  }
  
  Hist@FDead <-  Hist@FRetain <- ListArraySimAgeTimeFleet(OM, 'Historical') 
  Hist@FDeadArea <-  Hist@FRetainArea  <- ListArraySimAgeTimeFleetArea(OM, 'Historical')
  
  Hist <- ProcessMiscOM(Hist)
  
  if (!silent) 
    cli::cli_progress_done()
  
  Hist
}
  


 