
# Convert SimList back to Hist
SimList2Hist <- function(Hist, SimList, Years=NULL) {
  
  if (is.null(Years))
    Years <- Years(Hist@OM, "Historical")
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  CatchFrac <- purrr::map(SimList, \(HistSim) {
    HistSim@OM@CatchFrac
  }) |> ReverseList()
  
  Hist@OM@CatchFrac <- purrr::map(CatchFrac, \(Stock) {
    catchfrac <- List2Array(Stock, 'Sim', 'Fleet') |> aperm(c('Sim', 'Fleet'))
    dimnames(catchfrac)[[2]] <- FleetNames
    catchfrac
  }) 
  
  
  # RefPointsMSY
  Hist <- SimListRefPointsMSY(Hist, SimList)
  
  # RefPointsPR
  Hist <- SimListRefPointsPR(Hist, SimList)
  
  # RefLandings
  Hist@Reference@RefLandings <- purrr::map(SimList, \(HistSim) HistSim@Reference@RefLandings) |> 
    List2Array('Sim') |> aperm(c('Sim', 'Stock'))
  
  # RefRemovals
  Hist@Reference@RefRemovals <- purrr::map(SimList, \(HistSim) HistSim@Reference@RefRemovals) |> 
    List2Array('Sim') |> aperm(c('Sim', 'Stock'))
  
  # Stock
  Hist <- SimListStock(Hist, SimList)
  
  # Fleet 
  Hist <- SimListFleet(Hist, SimList)
  
  # Obs 
  Hist <- SimListObs(Hist, SimList)
  
  # imp 
  Hist <- SimListImp(Hist, SimList)
  
  # Time Series
  Hist <- SimListTimeSeries(Hist, SimList, Years)
  
  # Data
  HistData <- purrr::map(SimList, \(HistSim)
                         HistSim@Data) 
  CheckHash <- purrr::map(HistData, \(data) digest::digest(data, 'spookyhash')) |> unlist()
  if (all(CheckHash==CheckHash[1])) {
    Hist@Data <- list()
    Hist@Data[[1]] <- HistData[[1]]
    names(Hist@Data) <- 1
  } else {
    Hist@Data <- HistData
  }
  
  # Check for Depletion Optimization 
  # TODO - warning message or re-sample 
  OptDepletionRatio <- CheckDepletionOpt(SimList, Years) 
  Hist@Log$OptDepletionRatio <- OptDepletionRatio
  
  Hist
}


SimListRefPointsMSY <- function(Hist, SimList) {
  if (!EmptyObject(Hist@Reference@MSY))
    return(Hist)
  
  slots <- slotNames(Hist@Reference@MSY)
  for (slot in slots) {
    slot(Hist@Reference@MSY, slot) <- purrr::map(SimList, \(HistSim) 
                                                slot(HistSim@Reference@MSY, slot)) |>
      List2Array('Sim') |>
      aperm(c('Sim', 'Stock', 'Year'))
  }
  
  Hist
}

SimListRefPointsPR <- function(Hist, SimList) {
  
  Hist@Reference@SPR0 <- purrr::map(SimList, \(HistSim) HistSim@Reference@SPR0) |>
    List2Array('Sim') |>
    aperm(c('Sim', 'Stock', 'Year'))
  
  Hist
}


SimListStock <- function(Hist, SimList) {
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    Stock <- Hist@OM@Stock[[st]] 
    StockList <- purrr::map(SimList, \(x) x@OM@Stock[[st]])
    Stock <- StockList2SimArray(Stock, StockList, "Length")
    Stock <- StockList2SimArray(Stock, StockList, "Weight")
    Stock <- StockList2SimArray(Stock, StockList, "NaturalMortality")
    Stock <- StockList2SimArray(Stock, StockList, "Maturity")
    Stock <- StockList2SimArray(Stock, StockList, "Fecundity")
    Stock <- StockList2SimArray(Stock, StockList, "SRR")
    Hist@OM@Stock[[st]] <- Stock
  }
  Hist
}

SimListFleet <- function(Hist, SimList) {
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    Fleet <- Hist@OM@Fleet[[st]]
    FleetList <- purrr::map(SimList, \(x) x@OM@Fleet[[st]])
    
    Fleet@Catchability <- purrr::map(FleetList, \(x) x@Catchability) |> 
      List2Array('Sim') |>
      aperm(c('Sim','Year', 'Fleet'))
    
    Fleet@qArea <- purrr::map(FleetList, \(x) x@qArea) |> 
      List2Array('Sim') |>
      aperm(c('Sim','Year', 'Fleet', 'Area'))
    
    Fleet@Selectivity@MeanAtAge <- purrr::map(FleetList, \(x) x@Selectivity@MeanAtAge) |>
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'Year', 'Fleet'))
    
    Fleet@Retention@MeanAtAge <- purrr::map(FleetList, \(x) x@Retention@MeanAtAge) |>
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'Year', 'Fleet'))
    
    Fleet@DiscardMortality@MeanAtAge <- purrr::map(FleetList, \(x) x@DiscardMortality@MeanAtAge) |>
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'Year', 'Fleet'))
    
    Hist@OM@Fleet[[st]] <- Fleet
  }
  Hist
}

ObsList2SimArray <- function(Obs, ObsList, fl, slot='Catch') {
  
  if (!inherits(Obs[[fl]], 'obs')) # TODO - Convert Obs
    return(Obs)
  
  Slots <- slotNames(slot(Obs[[fl]], slot))
  
  for (sl in Slots) {
    Val <- purrr::map(ObsList, \(obs) 
                      slot(slot(obs[[fl]], slot), sl)
    )
    
    if (inherits(Val[[1]], 'NULL'))
      next()
    
    if (inherits(Val[[1]], 'array')) {
      Val <- List2Array(Val, 'Sim') 
      dnames <- names(dimnames(Val)) 
      if (length(dnames)==3) {
        Val <- aperm(Val, c('Sim', 'Age', 'Year'))
      }
      if (length(dnames)==2) {
        Val <- aperm(Val, c('Sim', 'Year'))
      }
      
    } else if (inherits(Val[[1]], 'numeric') | inherits(Val[[1]], 'integer')) {
      if (length(Val[[1]])==1) {
        Val <- List2Array(Val, 'Sim')[1,]
        Val <- array(Val, length(Val), dimnames=list(Sim=names(Val)))
      } else {
        Val <- Val[[1]] # TODO this probably doesn't apply for all cases
      }
    } else if (inherits(Val[[1]], 'character')) {
      if (length(Val[[1]])<1)
        next()
      Val <- List2Array(Val, 'Sim')[1,]
      
    } else if (inherits(Val[[1]], 'list')) {
      Val <- Val |> ReverseList() |> purrr::map(List2Array, 'Sim', pos=1)
    } else {
      stop("!!!")
    }
    slot(slot(Obs[[fl]], slot), sl) <- Val
  }
  Obs[[fl]]
}

SimListObs <- function(Hist, SimList) {
  
  nStock <- length(Hist@OM@Obs)
  
  for (st in 1:nStock) {
    Obs <- Hist@OM@Obs[[st]]
    ObsList <- purrr::map(SimList, \(x) x@OM@Obs[[st]])
    for (fl in 1:length(ObsList[[1]])) {
      Obs[[fl]] <- ObsList2SimArray(Obs, ObsList, fl, slot='Landings')
      Obs[[fl]] <- ObsList2SimArray(Obs, ObsList, fl, slot='Discards')
      Obs[[fl]] <- ObsList2SimArray(Obs, ObsList, fl, slot='Survey')
      Obs[[fl]] <- ObsList2SimArray(Obs, ObsList, fl, slot='CPUE')
      # Obs <- ObsList2SimArray(Obs, ObsList, fl, slot='CAA')
      # Obs <- ObsList2SimArray(Obs, ObsList, fl, slot='CAL')
      
      Hist@OM@Obs[[st]][[fl]] <- Obs[[fl]]
    }
  }
  Hist
}

SimListImp <- function(Hist, SimList) {
  Hist
  
}


StockList2SimArray <- function(Stock, StockList, slot="Length") {
  if (EmptyObject(slot(Stock, slot))) {
    slot(Stock, slot) <- new(class(slot(Stock, slot)))
    return(Stock)
  }
  
  
  nms <- slotNames(slot(Stock, slot))
  
  if ("Pars" %in% nms) {
    Pars <- slot(Stock, slot)@Pars
    if (!any(lapply(Pars, is.na) |> unlist())) {
      slot(Stock, slot)@Pars <- purrr::map(StockList, \(x) slot(x, slot)@Pars) |> 
        ReverseList() |>
        purrr::map(List2Array, 'Sim') |>
        purrr::map(aperm, c('Sim', 'Year'))
    }
  }
  
  if ("MeanAtAge" %in% nms)
    slot(Stock, slot)@MeanAtAge <- purrr::map(StockList, \(x) slot(x, slot)@MeanAtAge) |> 
    List2Array('Sim') |>
    aperm(c('Sim', 'Age', 'Year'))
  
  if ("CVatAge" %in% nms)
    slot(Stock, slot)@CVatAge <- purrr::map(StockList, \(x) slot(x, slot)@CVatAge) |> 
    List2Array('Sim') |>
    aperm(c('Sim', 'Age', 'Year'))
  
  if ("MeanAtLength" %in% nms)
    slot(Stock, slot)@MeanAtLength <- purrr::map(StockList, \(x) slot(x, slot)@MeanAtLength) |> 
    List2Array('Sim') |>
    aperm(c('Sim', 'Class', 'Year'))
  
  Stock
}



SimListTimeSeries <- function(Hist, SimList, Years= NULL) {
  if (is.null(Years))
    Years <- Years(Hist@OM, "Historical")
  
  StockNames <- StockNames(Hist@OM)
  FleetNames <- as.vector(Hist@OM@Fleet[[1]]@Name)
  nStock <- length(StockNames)
  
  for (st in 1:nStock) {
    Hist@Number[[st]] <- purrr::map(SimList, \(HistSim) {
      AddDimNames(HistSim@Number[[st]],  c("Age", "Year", "Area"), 
                  Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                  Years=Years)
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Area'))
  }
  
  Hist@Biomass <- purrr::map(SimList, \(HistSim)
                             AddDimNames(HistSim@Biomass, c('Stock', 'Year'), Years=Years,
                                         values=list(StockNames))
  ) |> List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "Year"))
  
  Hist@SBiomass <- purrr::map(SimList, \(HistSim)
                              AddDimNames(HistSim@SBiomass, c('Stock', 'Year'), Years=Years,
                                          values=list(StockNames))
  ) |> List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "Year"))
  
  Hist@SProduction <- purrr::map(SimList, \(HistSim)
                                 AddDimNames(HistSim@SProduction, c('Stock', 'Year'), Years=Years,
                                             values=list(StockNames))
  ) |> List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "Year"))
  
  Hist@Landings <- purrr::map(SimList, \(HistSim) HistSim@Landings) |> 
    List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "Year", "Fleet"))
  
  Hist@Discards <- purrr::map(SimList, \(HistSim) HistSim@Discards) |> 
    List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "Year", "Fleet"))

  
  for (st in 1:nStock) {
    Hist@LandingsAtAge[[st]] <- purrr::map(SimList, \(HistSim) {
      List2Array(HistSim@LandingsAtAge[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"), 
                    Years=Years,
                    Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Fleet', 'Area'))
    
    Hist@DiscardsAtAge[[st]] <- purrr::map(SimList, \(HistSim) {
      List2Array(HistSim@DiscardsAtAge[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"), 
                    Years=Years,
                    Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Fleet', 'Area'))
  }
  
  Hist@Effort <- purrr::map(SimList, \(HistSim)
                            AddDimNames(HistSim@Effort, c('Stock', 'Year', 'Fleet'), 
                                        Years=Years,
                                        values=c(list(StockNames), list(NA), list(FleetNames)))) |> 
    List2Array("Sim") |>
    aperm(c("Sim", 'Stock', "Year", "Fleet"))
  
  Hist@Distribution[[st]] <- purrr::map(SimList, \(HistSim) 
                                        AddDimNames(HistSim@Distribution[[st]], c('Year', 'Fleet', "Area"), Years=Years,
                                                    values=c(list(NA), list(FleetNames), list(NA)))) |> 
    List2Array("Sim") |>
    aperm(c("Sim", "Year", "Fleet", 'Area'))
  
  
  for (st in 1:nStock) {
    Hist@FDeadArea[[st]] <- purrr::map(SimList, \(HistSim) {
      List2Array(HistSim@FDeadArea[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"), 
                    Years=Years,
                    Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Fleet', 'Area'))
    
    Hist@FDead[[st]] <- purrr::map(SimList, \(HistSim) {
      HistSim@FDead[[st]] |>
        AddDimNames(c("Age", "Year", "Fleet"), 
                    Years=Years,
                    Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                    values=c(list(NA), list(NA), list(FleetNames)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Fleet'))
    
    
    Hist@FRetainArea[[st]] <- purrr::map(SimList, \(HistSim) {
      List2Array(HistSim@FRetainArea[[st]]) |>
        AddDimNames(c("Age", "Fleet", "Area", "Year"), 
                    Years=Years,
                    Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                    values=c(list(NA), list(FleetNames), list(NA), list(NA)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Fleet', 'Area'))
    
    Hist@FRetain[[st]] <- purrr::map(SimList, \(HistSim) {
      HistSim@FRetain[[st]] |>
        AddDimNames(c("Age", "Year", "Fleet"), 
                    Years=Years,
                    Ages=HistSim@OM@Stock[[st]]@Ages@Classes,
                    values=c(list(NA), list(NA), list(FleetNames)))
    }) |>
      List2Array("Sim") |>
      aperm(c("Sim", 'Age', 'Year', 'Fleet'))
    
    
    
  }
  
  Hist
}

