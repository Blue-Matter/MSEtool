
# HistSim <- HistSimList$`1`


GenerateHistoricalData <- function(HistSim, HistTimeSteps) {
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
    if (is.null(Data@Name)) {
      Data@Name <- purrr::map(HistSim@OM@Stock[stocks], slot, 'Name') |> unlist() |> paste(collapse='-')
    }
    if (is.null(Data@TimeSteps)) {
      Data@TimeSteps <- HistTimeSteps
    }
    if (is.null(Data@TimeStepsPerYear)) {
      Data@TimeStepsPerYear <- HistSim@OM@TimeStepsPerYear
    }
    
    if (is.null(Data@TimeStepLH)) {
      Data@TimeStepLH <- Data@TimeSteps[length(Data@TimeSteps)]
    }
    Data@TimeUnits <-  HistSim@OM@Stock[[stocks[1]]]@Ages@Units
    
    Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, i, stocks, 'Landings')
    Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, i, stocks, 'Discards')
    HistSim@Data[[i]] <- Data
    
    HistSim <- GenerateHistoricalData_Index(HistSim, HistTimeSteps, i, stocks)
    HistSim <- GenerateHistoricalData_Index(HistSim, HistTimeSteps, i, stocks, 'Survey')
  }
  HistSim
  
}

  
GenerateHistoricalData_Catch <- function(Data, HistSim, HistTimeSteps, i, 
                                         stocks, type=c('Landings', 'Discards')) {
  type <- match.arg(type)
  if (!EmptyObject(slot(Data, type))) 
    return(Data)
  
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[1]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  CatchData <- new('catchdata')
  CatchData@Name <- FleetNames
  CatchData@Type <- type
  CatchData@Value <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(TimeStep=HistTimeSteps,
                                         Fleet=FleetNames))
  CatchData@CV <- CatchData@Value 
  CatchData@CV[] <- 0.2
  
  Catch  <- purrr::map(slot(HistSim, type)[stocks], \(catch) 
                       catch |> List2Array() |> apply(c(2,4), sum) |> t()
  ) |> List2Array('Stock') |>
    apply(1:2, sum)
  dimnames(Catch) <- list(TimeStep=HistTimeSteps, 
                          Fleet=FleetNames)
  
  
  for (fl in 1:nFleet) {
    Obs <- HistSim@OM@Obs[[i]][[fl]]
   
    obs <- slot(Obs, type)
    if (EmptyObject(obs)) {
      next()
    }
    
    CatchData@Value[,fl] <- Catch[,fl] * ArraySubsetTimeStep(obs@Error, HistTimeSteps) * obs@Bias
    
    NA_TS <- which(!HistTimeSteps %in% obs@TimeSteps)
    if (length(NA_TS)>0) {
      CatchData@Value[NA_TS,fl] <- NA
    }
    # CatchData@Type[fl] <- Obs@Catch@Type
  }
  
  CatchData@Units <- 'Biomass'
  
  slot(Data, type) <- CatchData
  
  Data
}
  
GenerateHistoricalData_Index <- function(HistSim, HistTimeSteps, i, stocks, 
                                         type=c('CPUE', 'Survey')) {
  
  type <- match.arg(type)
  Data <- HistSim@Data[[i]]

  if (!EmptyObject(slot(Data, type))) 
    return(HistSim)
  
  nTS <- length(HistTimeSteps)
  
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
                           dimnames=list(TimeStep=HistTimeSteps,
                                         Fleet=FleetNames))
  
  IndexData@CV <-  IndexData@Value
  IndexData@CV[] <- 0.2
  IndexData@Units <- rep('Biomass', nFleet)

  SimulatedNumberList <- purrr::map(HistSim@Number[stocks], \(stock) {
    stock |> AddDimNames(c('Age', 'TimeStep', 'Area'),HistTimeSteps) |>
      apply(c('Age', 'TimeStep'), sum)
  })
  
  for (fl in 1:nFleet) {
    IndexObs <- slot(HistSim@OM@Obs[[i]][[FleetNames[fl]]],type)
    SelectivityAtAge <- IndexObs@Selectivity
    SelectivityAtAgeList <- MakeNamedList(StockNames(HistSim@OM)[stocks])
    
    if (is.character(SelectivityAtAge)) {
      if (SelectivityAtAge == 'Biomass') {
        for (st in seq_along(stocks)) {
          SelectivityAtAgeList[[st]] <- matrix(1,nAge(HistSim@OM, stocks[st]), 1) |>
            AddDimNames(c('Age', 'TimeStep'), HistTimeSteps)
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
          ArraySubsetTimeStep(HistTimeSteps) |> 
          ArrayReduceDims()
      }) 
    }
    
    Units <- IndexData@Units[fl]
    SimNumberSelectedList <- purrr::map2(SimulatedNumberList, SelectivityAtAgeList, ArrayMultiply)
    
    if (Units=='Biomass') {
      WeightAtAgeList <- purrr::map(HistSim@OM@Stock[stocks], \(stock) stock@Weight@MeanAtAge |>
                                      ArraySubsetTimeStep(HistTimeSteps)) 
      
      SimulatedIndex <- purrr::map2(SimNumberSelectedList, WeightAtAgeList, ArrayMultiply) |>
        purrr::map(apply, 'TimeStep', sum) |>
        List2Array('Stock', 'TimeStep') |>
        AddDimNames(c('TimeStep', 'Stock'), HistTimeSteps) |> 
        apply(c('TimeStep'), sum) 
      
    } else if (Units=='Number') {
      SimulatedIndex <- SimNumberSelectedList |>
        purrr::map(apply, 'TimeStep', sum) |>
        List2Array('Stock', 'TimeStep') |>
        AddDimNames(c('TimeStep', 'Stock'), HistTimeSteps) |> 
        apply(c('TimeStep'), sum) 
      
    } else {
      cli::cli_abort('Not done yet!', .internal=TRUE)
    }

    SimulatedIndexError <- SimulatedIndex *  ArraySubsetTimeStep(IndexObs@Error, HistTimeSteps)
    StIndex <- SimulatedIndexError/mean(SimulatedIndexError, na.rm=TRUE)
    IndexData@Value[,fl] <- StIndex
    NonNAInd <- which(!is.na(StIndex))
    IndexObs@q <- mean(StIndex, na.rm=TRUE)/mean(SimulatedIndex[NonNAInd], na.rm=TRUE)
    slot(HistSim@OM@Obs[[i]][[FleetNames[fl]]],type) <- IndexObs
  }
  slot(HistSim@Data[[i]],type) <- IndexData
  HistSim
}
  
  