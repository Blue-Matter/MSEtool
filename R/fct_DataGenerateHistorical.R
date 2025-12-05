
# HistSim <- HistSimList$`1`
# TODO - check for identical sims 


# Hist@Data:
# - list of length `nSim` (or length 1) then
# - list of length `nComplex`

GenerateHistoricalData <- function(SimList, HistYears) {
  
  SimList <- purrr::map(SimList, \(HistSim)
                        GenerateHistoricalData_Sim(HistSim, HistYears),
                        .progress = list(
                          type = "iterator",
                          format = "Generating Historical Data {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))
  
  class(SimList) <- 'simlist'
  SimList
  
}

GenerateHistoricalData_Sim <- function(HistSim, HistYears) {
  OM <- HistSim@OM
  Complexes <- HistSim@OM@Complexes
  HistSim@Data <- MakeNamedList(names(Complexes), new('data'))
  
  if (!is.null(HistSim@OM@Data)) {
    HistSim@Data <- HistSim@OM@Data
  }
    
  nStockData <- length(HistSim@Data)
  
  for (i in 1:nStockData) {
    stocks <- Complexes[[i]]
    Data <- HistSim@Data[[i]]
    Data@nArea <- nArea(HistSim@OM) 
    
    if (is.null(Data@Name)) 
      Data@Name <- purrr::map(HistSim@OM@Stock[stocks], slot, 'Name') |> unlist() |> paste(collapse='-')
    
    if (is.null(Data@Years)) 
      Data@Years <- HistYears
    
    if (is.null(Data@Seasons)) 
      Data@Seasons <- HistSim@OM@Seasons
  
    if (is.null(Data@YearLH)) 
      Data@YearLH <- Data@Years[length(Data@Years)]
   
    Data <- GenerateHistoricalData_Effort(Data, HistSim, HistYears, i, stocks)
    Data <- GenerateHistoricalData_Catch(Data, HistSim, HistYears, i, stocks, 'Landings')
    Data <- GenerateHistoricalData_Catch(Data, HistSim, HistYears, i, stocks, 'Discards')
    HistSim@Data[[i]] <- Data
    
    HistSim <- GenerateHistoricalData_Index(HistSim, HistYears, i, stocks)
    HistSim <- GenerateHistoricalData_Index(HistSim, HistYears, i, stocks, 'Survey')
  }
  HistSim
}

GenerateHistoricalData_Effort <- function(Data, HistSim, HistYears, i, stocks) {
  if (!EmptyObject(Data@Effort))
    return(Data)
  
  nTS <- length(HistYears)
  FleetNames <- HistSim@OM@Fleet[[1]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  EffortData <- new('effortdata')
  EffortData@Name <- FleetNames
  
  EffortData@Value <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(Year=HistYears,
                                         Fleet=FleetNames))
  EffortData@CV <- EffortData@Value 
  EffortData@CV[] <- 0.2
  
  EffortData@Value[] <- HistSim@Effort[stocks,,,drop=FALSE] |> apply(2:3, mean, na.rm=TRUE)

  for (fl in 1:nFleet) {
    EffortObs <- HistSim@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(EffortObs)) {
      next()
    }
    
    EffortData@Value[,fl] <- EffortData@Value[,fl] * 
      EffortObs@Bias * 
      ArraySubsetYear(EffortObs@Error, HistYears)
    
  }
  Data@Effort <- EffortData
  Data
}
  
GenerateHistoricalData_Catch <- function(Data, HistSim, HistYears, i, 
                                         stocks, type=c('Landings', 'Discards')) {
  type <- match.arg(type)
  if (!EmptyObject(slot(Data, type))) 
    return(Data)
  
  nTS <- length(HistYears)
  FleetNames <- HistSim@OM@Fleet[[1]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  CatchData <- new('catchdata')
  CatchData@Name <- FleetNames
  CatchData@Type <- type
  CatchData@Value <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(Year=HistYears,
                                         Fleet=FleetNames))
  CatchData@CV <- CatchData@Value 
  CatchData@CV[] <- 0.2
  
  Catch  <- purrr::map(slot(HistSim, type)[stocks], \(catch) 
                       catch |> List2Array() |> apply(c(2,4), sum) |> t()
  ) |> List2Array('Stock') |>
    apply(1:2, sum)
  dimnames(Catch) <- list(Year=HistYears, 
                          Fleet=FleetNames)
  
  
  for (fl in 1:nFleet) {
    Obs <- HistSim@OM@Obs[[i]][[fl]]
   
    obs <- slot(Obs, type)
    if (EmptyObject(obs)) {
      next()
    }
    
    CatchData@Value[,fl] <- Catch[,fl] * ArraySubsetYear(obs@Error, HistYears) * obs@Bias
    
    NA_TS <- which(!HistYears %in% obs@Years)
    if (length(NA_TS)>0) {
      CatchData@Value[NA_TS,fl] <- NA
    }
    # CatchData@Type[fl] <- Obs@Catch@Type
  }
  
  CatchData@Units <- 'Biomass'
  
  slot(Data, type) <- CatchData
  
  Data
}
  
GenerateHistoricalData_Index <- function(HistSim, HistYears, i, stocks, 
                                         type=c('CPUE', 'Survey')) {
  
  type <- match.arg(type)
  
  Data <- HistSim@Data[[i]]

  if (!EmptyObject(slot(Data, type))) 
    return(HistSim)
  
  nTS <- length(HistYears)
  
  ObsObjectList <- purrr::map(HistSim@OM@Obs[[i]], \(obs) slot(obs,type))
  FleetNames <- names(ObsObjectList)
  TypeFleets <- purrr::map(ObsObjectList, \(obs) !is.null(obs@Error)) |> unlist() |> which()
  
  if (!length(TypeFleets))
    return(HistSim)
  
  ObsObjectList <- ObsObjectList[TypeFleets]
  FleetNames <- FleetNames[TypeFleets]
  nFleet <- length(FleetNames)
  
  IndexData <- new('indicesdata')
  IndexData@Name <- FleetNames
  IndexData@Value <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(Year=HistYears,
                                         Fleet=FleetNames))
  
  IndexData@CV <-  IndexData@Value
  IndexData@CV[] <- 0.2
  IndexData@Units <- rep('Biomass', nFleet)
  
  SimulatedNumberList <- purrr::map(HistSim@Number[stocks], \(stock) {
    stock |> AddDimNames(c('Age', 'Year', 'Area'),HistYears) |>
      apply(c('Age', 'Year'), sum)
  })
  
  for (fl in 1:nFleet) {
    IndexObs <- slot(HistSim@OM@Obs[[i]][[FleetNames[fl]]],type)
    SelectivityAtAge <- IndexObs@Selectivity
    SelectivityAtAgeList <- MakeNamedList(StockNames(HistSim@OM)[stocks])
    
    if (is.character(SelectivityAtAge)) {
      if (SelectivityAtAge == 'Biomass') {
        for (st in seq_along(stocks)) {
          SelectivityAtAgeList[[st]] <- matrix(1,nAge(HistSim@OM, stocks[st]), 1) |>
            AddDimNames(c('Age', 'Year'), HistYears)
        }
      } else if (SelectivityAtAge == 'SBiomass') {
        for (st in seq_along(stocks)) {
          SelectivityAtAgeList[[st]] <- HistSim@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge |> ArrayReduceDims()
        }
        
      } else if (SelectivityAtAge == 'Obs') {
        SelectivityAtAgeList <- IndexObs@Selectivity
      }
    } else {
      SelectivityAtAgeList <- purrr::map(HistSim@OM@Fleet[stocks], \(stock) {
        stock@Selectivity@MeanAtAge[,,fl] |>
          ArraySubsetYear(HistYears) |> 
          ArrayReduceDims()
      }) 
    }
    
    Units <- IndexData@Units[fl]
    SimNumberSelectedList <- purrr::map2(SimulatedNumberList, SelectivityAtAgeList, ArrayMultiply)
    
    if (Units=='Biomass') {
      WeightAtAgeList <- purrr::map(HistSim@OM@Stock[stocks], \(stock) stock@Weight@MeanAtAge |>
                                      ArraySubsetYear(HistYears)) 
      
      SimulatedIndex <- purrr::map2(SimNumberSelectedList, WeightAtAgeList, ArrayMultiply) |>
        purrr::map(apply, 'Year', sum) |>
        List2Array('Stock', 'Year') |>
        AddDimNames(c('Year', 'Stock'), HistYears) |> 
        apply(c('Year'), sum) 
      
    } else if (Units=='Number') {
      SimulatedIndex <- SimNumberSelectedList |>
        purrr::map(apply, 'Year', sum) |>
        List2Array('Stock', 'Year') |>
        AddDimNames(c('Year', 'Stock'), HistYears) |> 
        apply(c('Year'), sum) 
      
    } else {
      cli::cli_abort('Not done yet!', .internal=TRUE)
    }

    SimulatedIndexError <- SimulatedIndex *  ArraySubsetYear(IndexObs@Error, HistYears)
    StIndex <- SimulatedIndexError/mean(SimulatedIndexError, na.rm=TRUE)
    IndexData@Value[,fl] <- StIndex
    NonNAInd <- which(!is.na(StIndex))
    IndexObs@q <- mean(StIndex, na.rm=TRUE)/mean(SimulatedIndex[NonNAInd], na.rm=TRUE)
    
    # Ref value 
    if (length(IndexObs@Ref)) {
      # TODO - index ref value if units != Biomass
      if (!is.null(HistSim@RefPointsMSY@BMSY) && length(HistSim@RefPointsMSY@BMSY)) {
        adjust <- mean(SimulatedIndex/apply(HistSim@Biomass[i,,drop=FALSE], 2, mean, na.rm=TRUE), na.rm=TRUE)
        IndexData@Ref <- array(mean(HistSim@RefPointsMSY@BMSY[i,], na.rm=TRUE) *  adjust *IndexObs@q)  
      }
    }
    
    slot(HistSim@OM@Obs[[i]][[FleetNames[fl]]],type) <- IndexObs
  } # end of fleet loop
  slot(HistSim@Data[[i]],type) <- IndexData
  HistSim
}
  
  