

## ---- OM -----
#' @describeIn Populate Populate an [OM()] object
#' @param seed Seed for the random number generator
#' @export
PopulateOM <- function(OM, silent=FALSE) {
  CheckClass(OM)
  if (CheckDigest(OM) | EmptyObject(OM))
    return(OM)
  
  OM@Stock  <- PopulateStockList(OM, silent)
  OM@Fleet <- PopulateFleetList(OM, silent)
  
  OM <- OM |>
    PopulateObs() |>
    # CheckCatchFrac() |> # TODO - auto-populate CatchFrac if OM@Data exists
    # CheckAllocation() |>
    PopulateComplexes() |> 
    UpdateSPFrom() |> # TODO
    ShareParameters() |>   # share parameters for two-sex stocks TODO
    StartMessages()
  
  #Imp
  
  # Complexes
  
  # OM@Data
  

  SetDigest(OM)
  
}

PopulateStockList <- function(OM, silent=FALSE) {
  nStocks <- nStock(OM)
  StockList <- vector('list', nStocks)
  names(StockList) <- paste('Stock', 1:nStocks)
  class(StockList) <- 'StockList'
  
  for (st in 1:nStocks) {
    if (isS4(OM@Stock)) {
      stock <- OM@Stock 
    } else {
      stock <- OM@Stock[[st]]
    }
    stock@nSim <- OM@nSim
    stock@nYear <- OM@nYear
    stock@pYear <- OM@pYear
    stock@CurrentYear <- OM@CurrentYear
    StockList[[st]] <- PopulateStock(stock, 
                                     seed=OM@Seed, 
                                     silent=silent)
    names(StockList)[st] <- stock@Name
  }
  StockList
}

PopulateFleetList <- function(OM, silent=FALSE) {
  StockList <- OM@Stock
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  FleetList <- vector('list', nStocks)
  class(FleetList) <- 'StockFleetList'
  names(FleetList) <- paste('Stock', 1:nStocks)
  
  for (st in 1:nStocks) {
    names(FleetList)[st] <- names(StockList)[st]
    FleetList[[st]] <- list()
    class(FleetList[[st]]) <- 'FleetList'
    
    for (fl in 1:nFleets) {
      if (isS4(OM@Fleet)) {
        fleet <- OM@Fleet
      } else if (inherits(OM@Fleet, 'FleetList')) {
        fleet <- OM@Fleet[[fl]]
      } else {
        fleet <- OM@Fleet[[st]][[fl]]
      }
      
      fleet@nSim <- OM@nSim
      fleet@nYear <- OM@nYear
      fleet@pYear <- OM@pYear
      fleet@CurrentYear <- OM@CurrentYear
      stock <- StockList[[st]]
      fleet@TimeUnits <- stock@Ages@Units
      fleet@TimeStepsPerYear <- TSperYear(stock@TimeUnits)
      fleet@TimeSteps <- CalcTimeSteps(stock@nYear, 
                                       stock@pYear, 
                                       stock@CurrentYear, 
                                       stock@TimeUnits)
      
      FleetList[[st]][[fl]] <- PopulateFleet(Fleet=fleet, 
                                             Ages=Ages(StockList[[st]]),
                                             Length=Length(StockList[[st]]),
                                             Weight=Weight(StockList[[st]]),
                                             RelativeSize=StockList[[st]]@Spatial@RelativeSize,
                                             seed=OM@Seed,
                                             silent=silent)
      
      names(FleetList[[st]])[fl] <- FleetList[[st]][[fl]]@Name
      
    }
  }
  FleetList
}


PopulateComplexes <- function(OM) {
  if (length(OM@Complexes)>0)
    return(OM)
  
  # TODO validation for Complexes
  
  if (nStock(OM) ==1) {
    OM@Complexes <- MakeNamedList(StockNames(OM), 1)
    return(OM)
  }
  
  if (length(OM@Data)>0) {
    if (length(OM@Data)==1) {
      OM@Complexes <- MakeNamedList(names(OM@Data), 1:nStock(OM))
      return(OM)
    }
    
    if (length(OM@Data)==nStock(OM)) {
      OM@Complexes <- list()
      for (i in seq_along(OM@Stock)) {
        OM@Complexes[[i]] <- i
      }
      names(OM@Complexes) <- StockNames(OM)
      return(OM)
    }
  }
  OM 
}
