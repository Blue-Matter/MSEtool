
# HistSim <- HistSimList$`1`


GenerateHistoricalData <- function(HistSim, HistTimeSteps) {
  OM <- HistSim@OM
  HistSim@Data <- MakeNamedList(StockNames(OM), new('data'))
  nStock <- nStock(OM)
  
  if (!is.null(HistSim@OM@Data)) {
    HistSim@Data <- HistSim@OM@Data
  }
    
  nStockData <- length(HistSim@Data)
  for (stock in 1:nStockData) {
    HistSim@Data[[stock]] <- GenerateHistoricalDataStock(stock, HistSim, HistTimeSteps)
  }
  
  HistSim
}

GenerateHistoricalDataStock <- function(stock, HistSim, HistTimeSteps) {
  Data <- HistSim@Data[[stock]]
  # TODO - don't overwrite if already existing
  Data@Name <- HistSim@OM@Stock[[stock]]@Name
  Data@TimeSteps <- HistTimeSteps
  Data@TimeStepLH <- Data@TimeSteps[length(Data@TimeSteps)]
  Data@TimeUnits <- HistSim@OM@Stock[[stock]]@Ages@Units
  
  Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, stock)
  Data <- GenerateHistoricalData_Index(Data, HistSim, HistTimeSteps, stock)

  # Data@Index
  # Data@CAA
  # Data@CAL
  
  Data
}
  
  
GenerateHistoricalData_Catch <- function(Data, HistSim, HistTimeSteps, stock) {
  if (!EmptyObject(Data@Catch)) 
    return(Data)
  
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[stock]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  Data@Catch <- new('catch')
  Data@Catch@Value <- array(NA, dim=c(nTS, nFleet),
                               dimnames=list(TimeStep=HistTimeSteps,
                                             Fleet=FleetNames))
  
  Data@Catch@CV <- Data@Catch@Value 
  Data@Catch@CV[] <- 0.2
  
  Removals <- apply(List2Array(HistSim@Removals[[stock]]), c(2,4), sum) |> t()
  dimnames(Removals) <- list(TimeStep=HistTimeSteps, 
                             Fleet=FleetNames)
  
  Landings <- apply(List2Array(HistSim@Landings[[stock]]), c(2,4), sum) |> t()
  dimnames(Landings) <- list(TimeStep=HistTimeSteps, 
                             Fleet=FleetNames)


  for (fl in 1:nFleet) {
    Obs <- HistSim@OM@Obs[[stock]][[fl]]
    if (!inherits(Obs, 'obs')) # TODO
      next()
    if (Obs@Catch@Type == 'Removals') {
      Data@Catch@Value[,fl] <- Removals[,fl] * ArraySubsetTimeStep(Obs@Catch@Error, HistTimeSteps) * Obs@Catch@Bias[1:length(HistTimeSteps)]
    } else {
      Data@Catch@Value[,fl] <- Landings[,fl] * ArraySubsetTimeStep(Obs@Catch@Error, HistTimeSteps) * Obs@Catch@Bias[1:length(HistTimeSteps)]
    }
    
    NA_TS <- which(!HistTimeSteps %in% Obs@Catch@TimeSteps)
    if (length(NA_TS)>0) {
      Data@Catch@Value[NA_TS,fl] <- NA
    }
    Data@Catch@Type[fl] <- Obs@Catch@Type
  }
  
  Data@Catch@Units <- 'Biomass'
  Data
}
  
GenerateHistoricalData_Index <- function(Data, HistSim, HistTimeSteps, stock) {
  # TODO add ability to generate additional indices using info in Obs 
  if (!EmptyObject(Data@Index)) 
    return(Data)
  
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[stock]]@Name |> as.character()
  nFleet <- length(FleetNames)
  
  Data
}
  
  
  