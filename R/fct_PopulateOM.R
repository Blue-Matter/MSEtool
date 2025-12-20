

## ---- OM -----
#' @describeIn Populate Populate an [OM()] object
#' @param seed Seed for the random number generator
#' @export
PopulateOM <- function(OM, silent=FALSE) {
  CheckClass(OM)
  # if (CheckDigest(OM) | EmptyObject(OM))
  if (EmptyObject(OM))
    return(OM)
  
  if (is.null(OM@Stock))
    cli::cli_abort(c('x'='{.var OM} must have at least one stock',
                     'i'='See {.help MSEtool::OM} and {.help MSEtool::Stock}'))
  
  if (is.null(OM@Fleet))
    cli::cli_abort(c('x'='{.var OM} must have at least one fleet',
                     'i'='See {.help MSEtool::OM} and {.help MSEtool::Fleet}'))
  
  OM@Stock  <- PopulateStockList(OM, silent)
  OM@Fleet <- PopulateFleetList(OM, silent)
  OM@Imp <- PopulateImpList(OM, silent)
  
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
  OM <- ProcessData(OM)
  
  SetDigest(OM)
}

ProcessData <- function(OM) {
  if (is.null(OM@Data))
    return(OM)
  stocknames <- StockNames(OM)
  
  if (isS4(OM@Data)) {
    if (length(stocknames) > 1) 
      stocknames <- paste(stocknames, collapes='-')
    OM@Data <- MakeNamedList(stocknames, OM@Data)
  }
  
  if (is.list(OM@Data) & is.null(names(OM@Data)))
    names(OM@Data) <- stocknames

  OM
}



PopulateStockList <- function(OM, silent=FALSE) {
  nStocks <- nStock(OM)
  StockList <- vector('list', nStocks)
  names(StockList) <- paste('Stock', 1:nStocks)
  class(StockList) <- 'StockList'
  
  for (st in 1:nStocks) {
    if (isS4(OM@Stock)) {
      Stock <- OM@Stock 
    } else {
      Stock <- OM@Stock[[st]]
    }
    StockList[[st]] <- PopulateStock(Stock, 
                                     seed=OM@Seed, 
                                     nYear=OM@nYear,
                                     pYear=OM@pYear,
                                     CurrentYear=OM@CurrentYear,
                                     nSim=OM@nSim,
                                     Seasons=OM@Seasons,
                                     seed=OM@Seed+st, 
                                     silent=silent)
    names(StockList)[st] <- Stock@Name
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
      if (is.null(OM@Fleet))
        next()
      if (isS4(OM@Fleet)) {
        Fleet <- OM@Fleet
      } else if (inherits(OM@Fleet, 'FleetList')) {
        Fleet <- OM@Fleet[[fl]]
      } else {
        if (!length(OM@Fleet[[st]]))
            next()
        Fleet <- OM@Fleet[[st]][[fl]]
      }
      
      Fleet@nSim <- OM@nSim
      Fleet@nYear <- OM@nYear
      Fleet@pYear <- OM@pYear
      Fleet@CurrentYear <- OM@CurrentYear
      Stock <- StockList[[st]]
      Fleet@Seasons <- Stock@Seasons
      Fleet@Years <- CalcYears(nYear=Stock@nYear, 
                                       pYear=Stock@pYear, 
                                       CurrentYear=Stock@CurrentYear, 
                                       Seasons= Stock@Seasons )
      
      FleetList[[st]][[fl]] <- PopulateFleet(Fleet=Fleet, 
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

PopulateImpList <- function(OM, silent=FALSE) {
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  ImpList <- MakeNamedList(StockNames(OM),
                           MakeNamedList(FleetNames(OM),
                           new('imp'))
  )
  
  if (length(OM@Imp)) {
    for (st in 1:nStocks) {
      for (fl in 1:nFleets) {
        
        if (isS4(OM@Imp)) {
          Imp <- OM@Imp 
        } else {
          Imp <- OM@Imp[[st]][[fl]]
        }
        ImpList[[st]][[fl]] <- Imp
      }
    }
  }

  ImpList
}

