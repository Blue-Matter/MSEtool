

## ---- OM -----
#' @describeIn Populate Populate an [OM()] object
#' @param seed Seed for the random number generator
#' @param force Populate object even if digest hasn't changed?
#' @export
PopulateOM <- function(OM, silent=FALSE, force=FALSE) {
  CheckClass(OM)
  
  if (EmptyObject(OM)) {
    return(OM)
  }
  
  if (CheckDigest(OM) & !force) {
    return(OM)
  }  
  
  if (is.null(OM@Stock))
    cli::cli_abort(c('x'='{.var OM} must have at least one stock',
                     'i'='See {.help MSEtool::OM} and {.help MSEtool::Stock}'))
  
  if (is.null(OM@Fleet))
    cli::cli_abort(c('x'='{.var OM} must have at least one fleet',
                     'i'='See {.help MSEtool::OM} and {.help MSEtool::Fleet}'))
  
  OM <- OM |>
    PopulateStockList(silent, force) |>
    PopulateFleetList(silent, force) |>
    PopulateImpList(silent) |> 
    PopulateComplexes() |>
    ProcessData() |> 
    PopulateObsList(silent) |>
    UpdateSPFrom() |> # TODO
    ShareParameters() |>   # share parameters for two-sex stocks TODO
    StartMessages()

    # CheckCatchFrac() |> # TODO - auto-populate CatchFrac if OM@Data exists
    # CheckAllocation() 

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


PopulateStockList <- function(OM, silent=FALSE, force=FALSE) {
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
                                     nYear=OM@nYear,
                                     pYear=OM@pYear,
                                     CurrentYear=OM@CurrentYear,
                                     nSim=OM@nSim,
                                     Seasons=OM@Seasons,
                                     seed=OM@Seed+st, 
                                     silent=silent,
                                     force=force)
    names(StockList)[st] <- Stock@Name
  }
  OM@Stock <- StockList
  OM
}

PopulateFleetList <- function(OM, silent=FALSE, force=FALSE) {
  if (is.null(OM@Fleet)) {
    return(OM)
  }
  
  StockList <- OM@Stock
  nStocks <- nStock(OM)
  nFleets <- nFleet(OM)
  FleetList <- vector('list', nStocks)
  class(FleetList) <- 'StockFleetList'
  names(FleetList) <- paste('Stock', 1:nStocks)
  
  # Prep Fleet List 
  for (st in 1:nStocks) {
    names(FleetList)[st] <- names(StockList)[st]
    FleetList[[st]] <- list()
    class(FleetList[[st]]) <- 'FleetList'
    
    if (isS4(OM@Fleet)) {
      FleetList[[st]] <- list(OM@Fleet)
      next()
    }
    
    if (inherits(OM@Fleet, 'FleetList')) {
      FleetList[[st]] <- OM@Fleet
      next()
    }
    
    
    for (fl in 1:nFleets) {
      if (length(OM@Fleet) < st) {
        if (length(OM@Fleet)>1) {
          cli::cli_abort("`OM@Fleet` must be a list length 1 or length `nStock` ({.val {nStocks}})")
        }
        FleetList[[st]][[fl]] <- OM@Fleet[[1]][[fl]]
      } else {
        FleetList[[st]][[fl]] <- OM@Fleet[[st]][[fl]]
      }
    }
  }
  
  for (st in 1:nStocks) {
    for (fl in 1:nFleets) {
      
      FleetList[[st]][[fl]] <- PopulateFleet(Fleet=FleetList[[st]][[fl]], 
                                             Stock=StockList[[st]],
                                             seed=OM@Seed+st+fl,
                                             silent=silent,
                                             force=force)
      
      names(FleetList[[st]])[fl] <- FleetList[[st]][[fl]]@Name
      
    }
  }
  OM@Fleet <- FleetList
  OM
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
          if (length(OM@Imp) < st) {
            if (length(OM@Imp)>1) {
              cli::cli_abort("`OM@Imp` must be a list length 1 or length `nStock` ({.val {nStocks}})")
            }
            Imp <- OM@Imp[[1]][[fl]]
          } else {
            Imp <- OM@Imp[[st]][[fl]]  
          }
          
        }
        ImpList[[st]][[fl]] <- Imp
      }
    }
  }

  OM@Imp <- ImpList
  OM
}

PopulateObsList <- function(OM, silent=FALSE) {
  
  Complexes <- Complexes(OM)
  ComplexNames <- names(Complexes)
  FleetNames <- FleetNames(OM)
  nFleet <- length(FleetNames)
  
  if (EmptyObject(OM@Obs)) {
    # initialize Obs object for conditioning
    if (is.null(FleetNames)) {
      return(OM)
    }
    
    OM@Obs <- MakeNamedList(
      ComplexNames,
      MakeNamedList(FleetNames, new("obs"))
    )
    return(OM)
  }
  
  # Recycles over both stocks and fleets
  if (inherits(OM@Obs, "obs")) {
    OM@Obs <- MakeNamedList(ComplexNames, MakeNamedList(FleetNames, OM@Obs))
  }
  
  if (!is.list(OM@Obs)) {
    cli::cli_abort("`OM@Obs` must be a list or an object of class `obs`")
  }
  
  # Prep Obs List
  nComplex <- length(ComplexNames)
  nFleets <- nFleet(OM)
  ObsList <- vector('list', nComplex)
  names(ObsList) <- names(Complexes)
  
  for (st in 1:nComplex) {
    ObsList[[st]] <- vector('list', nFleet)
    names(ObsList[[st]]) <- FleetNames
    if (isS4(OM@Obs)) {
      ObsList[[st]] <- list(OM@Obs)
      next()
    }
    
    if (inherits(OM@Obs[[st]], 'list')) {
      ObsList[[st]] <- OM@Obs[[st]]
      next()
    }
    
    for (fl in 1:nFleets) {
      if (length(OM@Obs) < st) {
        if (length(OM@Obs)>1) {
          cli::cli_abort("`OM@Obs` must be a list length 1 or length `nStock` ({.val {nStocks}})")
        }
        ObsList[[st]][[fl]] <- OM@Obs[[1]][[fl]]
      } else {
        ObsList[[st]][[fl]] <- OM@Obs[[st]][[fl]]
      }
    }
  }
  
  HistYears <- Years(OM, "H")
  ProjYears <- Years(OM, "P")
  
  for (st in 1:length(ObsList)) {
    for (fl in 1:length(ObsList[[1]])) {
      SetSeed(OM@Seed + st + fl)
      
      ObsList[[st]][[fl]]@Effort <- PopulateEffortObs(
        Effort = ObsList[[st]][[fl]]@Effort,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )
      
      ObsList[[st]][[fl]]@Landings <- PopulateCatchObs(
        Catch = ObsList[[st]][[fl]]@Landings,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )
      
      ObsList[[st]][[fl]]@Discards <- PopulateCatchObs(
        Catch = ObsList[[st]][[fl]]@Discards,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )
      
      ObsList[[st]][[fl]]@CPUE <- PopulateIndexObs(
        Index = ObsList[[st]][[fl]]@CPUE,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )
      
      ObsList[[st]][[fl]]@Survey <- PopulateIndexObs(
        Index = ObsList[[st]][[fl]]@Survey,
        nSim = OM@nSim,
        HistYears,
        ProjYears
      )
      
      # OM@Obs[[st]][[fl]]@CAA
      
      # OM@Obs[[st]][[fl]]@CAL
    }
  }
  
  OM@Obs <- ObsList
  OM
  
}


