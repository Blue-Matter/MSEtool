
# HistSim <- HistSimList$`1`


GenerateHistoricalData <- function(HistSim, HistTimeSteps) {
  OM <- HistSim@OM
  Complexes <- Complexes(HistSim@OM)
  HistSim@Data <- MakeNamedList(names(Complexes), new('data'))
  
  
  if (!is.null(HistSim@OM@Data)) {
    HistSim@Data <- HistSim@OM@Data
  }
    
  nStockData <- length(HistSim@Data)
  
  for (i in 1:nStockData) {
    stocks <- Complexes[[i]]
    HistSim@Data[[i]] <- GenerateHistoricalDataStock(i, HistSim, HistTimeSteps, stocks)
  }
  
  HistSim
  
}

GenerateHistoricalDataStock <- function(i, HistSim, HistTimeSteps, stocks) {
  Data <- HistSim@Data[[i]]

  if (is.null(Data@Name)) {
    Data@Name <- purrr::map(HistSim@OM@Stock[stocks], slot, 'Name') |> unlist() |> paste(collapse='-')
  }
  
  if (is.null(Data@TimeSteps)) {
    Data@TimeSteps <- HistTimeSteps
  }
  
  if (is.null(Data@TimeStepLH)) {
    Data@TimeStepLH <- Data@TimeSteps[length(Data@TimeSteps)]
  }

  Data@TimeUnits <-  HistSim@OM@Stock[[stocks[1]]]@Ages@Units

  Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, i, stocks)
  
 

  Data@Landings
  Data@Discards
  
  
  Data@CPUE
  Data@Survey
  
  
  Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, st, 'Landings')
  
  # TODO
  Data <- GenerateHistoricalData_Index(Data, HistSim, HistTimeSteps, st)
  Data <- GenerateHistoricalData_Index(Data, HistSim, HistTimeSteps, st, 'Survey')

  # Data@Index
  # Data@CAA
  # Data@CAL
  
  Data
}
  
  
GenerateHistoricalData_Catch <- function(Data, HistSim, HistTimeSteps, i, stocks, type=c('Landings', 'Discards')) {
  type <- match.arg(type)
  if (!EmptyObject(slot(Data, type))) 
    return(Data)
  
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[1]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  CatchData <- new('catchdata')
  CatchData@Type <- type
  CatchData@Value <- array(NA, dim=c(nTS, nFleet),
                           dimnames=list(TimeStep=HistTimeSteps,
                                         Fleet=FleetNames))
  CatchData@CV <- CatchData@Value 
  CatchData@CV[] <- 0.2
  
  if (type=='Removals') {
    Catch <- apply(List2Array(HistSim@Removals[[st]]), c(2,4), sum) |> t()
  } else {
    Catch <- apply(List2Array(HistSim@Landings[[st]]), c(2,4), sum) |> t()
  }
  dimnames(Catch) <- list(TimeStep=HistTimeSteps, 
                          Fleet=FleetNames)
  
  
  for (fl in 1:nFleet) {
    Obs <- HistSim@OM@Obs[[st]][[fl]]
    if (EmptyObject(Obs)) {
      next()
    }
    if (!inherits(Obs, 'obs')) # TODO
      next()
    
    obs <- slot(Obs, type)
    CatchData@Value <-  Catch[,fl] * ArraySubsetTimeStep(obs@Error, HistTimeSteps) * obs@Bias
    
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
  
GenerateHistoricalData_Index <- function(Data, HistSim, HistTimeSteps, st, type=c('CPUE', 'Survey')) {
  # TODO add ability to generate additional indices using info in Obs 
  type <- match.arg(type)
  if (!EmptyObject(slot(Data, type))) 
    return(Data)
  
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[st]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  Data
}
  
  