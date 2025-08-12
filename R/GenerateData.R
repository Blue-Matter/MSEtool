
# HistSim <- HistSimList$`1`

# ----- Historical -----

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
  
  
# ---- Projection -----


GenerateProjectionData_Catch <- function(ProjSim, DataTimeStep, TimeStepsAll, st=1) {
  
  # TODO needs to have a stock index in ProjSim@OM@Data at some point
  # TODO catch units in numbers
  Removals <- ProjSim@Removals[[st]][[as.character(DataTimeStep)]] |>
    apply(2, sum) # sum over ages and areas
  Landings <- ProjSim@Landings[[st]][[as.character(DataTimeStep)]] |>
    apply(2, sum) # sum over age and areas
  
  Catch <- ProjSim@Data[[st]]@Catch
  if (EmptyObject(Catch))
    return(ProjSim)
  
  Value <- Catch@Value
  CV <- Catch@CV
  
  if (DataTimeStep %in% dimnames(Value)[[1]]) {
    # data already exists
    return(ProjSim)
  }
  
  TSIndex <- match(DataTimeStep, TimeStepsAll)
  
  nFleet <- ncol(Value)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(TimeStep=DataTimeStep,
                                    Fleet=Catch@Name))
  NewCV <- array(NA, dim=c(1, nFleet),
                 dimnames = list(TimeStep=DataTimeStep,
                                 Fleet=Catch@Name))
  
  # loop over fleets 
  for (fl in 1:nFleet) {
    Obs <- ProjSim@OM@Obs[[st]][[fl]]
    if (!inherits(Obs, 'obs')) # TODO - convert Obs
      return(ProjSim)
    # check if real data exists
    
    # Catch 
    if (!is.null(ProjSim@OM@Data[[st]]) && nrow(ProjSim@OM@Data[[st]]@Catch@Value)>=TSIndex) {
      NewValue[,fl] <- ProjSim@OM@Data[[st]]@Catch@Value[TSIndex,fl]
    } else {
      if (Catch@Units[fl] != 'Biomass')
        cli::cli_abort('Currently catch can only be in units of Biomass', .internal=TRUE)
      
      error <- ArraySubsetTimeStep(Obs@Catch@Error, DataTimeStep)
      bias <- ArraySubsetTimeStep(Obs@Catch@Bias, DataTimeStep)
      
      
      
      if (Catch@Type[fl] == 'Removals') {
        NewValue[,fl] <- Removals[fl] * error * bias
      } else {
        NewValue[,fl] <- Landings[fl] *  error * bias
      }
    }
    
    # CV 
    if (nrow(ProjSim@OM@Data[[st]]@Catch@CV)>=TSIndex) {
      NewCV[,fl] <- ProjSim@OM@Data[[st]]@Catch@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetTimeStep(Catch@CV, DataTimeStep)[fl]
    }
  }
  
  Catch@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  Catch@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  ProjSim@Data[[st]]@Catch <- Catch
  ProjSim
}

GenerateProjectionData_Index <- function(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, st=1) {
  
  # TODO hyperstability Beta not functional yet - ignored
  
  Index <- ProjSim@Data[[st]]@Index
  if (EmptyObject(Index))
    return(ProjSim)
  Value <- Index@Value
  CV <- Index@CV
  
  if (DataTimeStep %in% dimnames(Value)[[1]]) {
    # data already exists
    return(ProjSim)
  }
  
  TSIndex <- match(DataTimeStep, TimeStepsAll)
  
  nFleet <- ncol(Value)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(TimeStep=DataTimeStep,
                                    Fleet=Index@Name))
  NewCV <- array(NA, dim=c(1, nFleet),
                 dimnames = list(TimeStep=DataTimeStep,
                                 Fleet=Index@Name))
  
  FleetIndex <- match(Index@Name, names(ProjSim@OM@Obs[[st]]))
  
  # loop over fleets 
  for (fl in 1:nFleet) {
    Obs <- ProjSim@OM@Obs[[st]][[FleetIndex[fl]]]
    
    # TODO - make this an option
    # currently doesn't simulate index if last two data points were NAs
    if (all(!is.finite(tail(Value[,fl],2)))) 
      next()
    
    # Index 
    if (nrow(ProjSim@OM@Data[[st]]@Index@Value)>=TSIndex) {
      # check if real data exists
      NewValue[,fl] <- ProjSim@OM@Data[[st]]@Index@Value[TSIndex,fl]
    } else {
      if (Index@Timing[fl]!=0)
        cli::cli_alert_warning('`Index@Timing` not working yet. Calculating from beginning of time step')
      
      if (Index@Selectivity[fl]=='Obs') {
        SelectAtAge <- ArraySubsetTimeStep(Obs@Index@Selectivity, DataTimeStep)
      } else {
        cli::cli_abort('Not done yet!', .internal=TRUE)
      }
      
      NumberAtAge <- ProjSim@Number[[st]][,TSIndex,, drop=FALSE] |> apply(1:2, sum) 
      SimulatedValue <- ArrayMultiply(NumberAtAge, SelectAtAge)
      
      if (Index@Units[fl] == 'Biomass') {
        SimulatedValue <- SimulatedValue * ProjSim@OM@Stock[[st]]@Weight@MeanAtAge[,TSIndex]
      }
      SimulatedValue <- apply(SimulatedValue, 2, sum)
      
      NewValue[,fl] <- SimulatedValue * Obs@Index@q*Obs@Index@Error[TSIndex]
    }
    
    # CV 
    if (nrow(ProjSim@OM@Data[[st]]@Index@CV)>=TSIndex) {
      NewCV[,fl] <- ProjSim@OM@Data[[st]]@Index@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- Index@CV[TSIndex-1,fl]
    }
  }
  Index@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  Index@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  ProjSim@Data[[st]]@Index <- Index
  ProjSim
}


GenerateProjectionData <- function(ProjSim, TimeStep, TimeStepsHist, TimeStepsProj) {
  
  st <- 1 # TODO multiple stocks
  
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj)
  TSIndex <- match(TimeStep, TimeStepsAll) - 1
  DataTimeStep <- TimeStepsAll[TSIndex]
  
  if (max(ProjSim@Data[[st]]@TimeSteps) == DataTimeStep)
    return(ProjSim)
  
  ProjSim@Data[[st]]@TimeSteps <- TimeStepsAll[1:TSIndex]
  
  ProjSim <- GenerateProjectionData_Catch(ProjSim, DataTimeStep, TimeStepsAll, st)
  ProjSim <- GenerateProjectionData_Index(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, st)
  
  # TODO 
  # - CAL
  # - CAA
  # - Life history
  
  ProjSim
}


  