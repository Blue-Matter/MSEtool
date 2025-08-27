
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
  for (st in 1:nStockData) {
    HistSim@Data[[st]] <- GenerateHistoricalDataStock(st, HistSim, HistTimeSteps)
  }
  
  HistSim
}

GenerateHistoricalDataStock <- function(st, HistSim, HistTimeSteps) {
  Data <- HistSim@Data[[st]]
  # TODO - don't overwrite if already existing
  Data@Name <- HistSim@OM@Stock[[st]]@Name
  Data@TimeSteps <- HistTimeSteps
  Data@TimeStepLH <- Data@TimeSteps[length(Data@TimeSteps)]
  Data@TimeUnits <- HistSim@OM@Stock[[st]]@Ages@Units
  
  Data@Removals
  Data@Landings
  Data@CPUE
  Data@Survey
  
  Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, st)
  Data <- GenerateHistoricalData_Catch(Data, HistSim, HistTimeSteps, st, 'Landings')
  
  # TODO
  Data <- GenerateHistoricalData_Index(Data, HistSim, HistTimeSteps, st)
  Data <- GenerateHistoricalData_Index(Data, HistSim, HistTimeSteps, st, 'Survey')

  # Data@Index
  # Data@CAA
  # Data@CAL
  
  Data
}
  
  
GenerateHistoricalData_Catch <- function(Data, HistSim, HistTimeSteps, st, type=c('Removals', 'Landings')) {
  type <- match.arg(type)
  if (!EmptyObject(slot(Data, type))) 
    return(Data)
  

  
  nTS <- length(HistTimeSteps)
  FleetNames <- HistSim@OM@Fleet[[st]]@Name |> as.character()
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
  
  
# ---- Projection -----


GenerateProjectionData_Catch <- function(ProjSim, DataTimeStep, TimeStepsAll, st=1, type=c('Removals', 'Landings')) {
  type <- match.arg(type)
  
  SimCatch <- slot(ProjSim, type)[[st]][[as.character(DataTimeStep)]] |>
    apply(2, sum) # sum over age and areas
  
  # TODO needs to have a stock index in ProjSim@OM@Data at some point
  # TODO catch units in numbers
  
  Catch <- slot(ProjSim@Data[[st]], type)
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
    Obs <- slot(ProjSim@OM@Obs[[st]][[fl]], type)
    
    if (length(Obs@Error)<1)
      next()

 
    # Catch 
    if (!is.null(ProjSim@OM@Data[[st]]) && nrow(slot(ProjSim@OM@Data[[st]],type)@Value)>=TSIndex) {
      NewValue[,fl] <- slot(ProjSim@OM@Data[[st]],type)@Value[TSIndex,fl]
    } else {
      if (Catch@Units[fl] != 'Biomass')
        cli::cli_abort('Currently catch can only be in units of Biomass', .internal=TRUE)
      
      error <- ArraySubsetTimeStep(Obs@Error, DataTimeStep)
      bias <- ArraySubsetTimeStep(Obs@Bias, DataTimeStep)
    
      NewValue[,fl] <- SimCatch[fl] * error * bias
    }
    
    # CV 
    if (nrow(slot(ProjSim@OM@Data[[st]],type)@CV)>=TSIndex) {
      NewCV[,fl] <- slot(ProjSim@OM@Data[[st]],type)@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetTimeStep(Catch@CV, DataTimeStep)[fl]
    }
  }
  
  Catch@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  Catch@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  slot(ProjSim@Data[[st]],type) <- Catch
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
    if (length(Obs@Index@Error)<1)
      next()
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
  
  ProjSim <- GenerateProjectionData_Catch(ProjSim, DataTimeStep, TimeStepsAll, st, type='Removals')
  ProjSim <- GenerateProjectionData_Catch(ProjSim, DataTimeStep, TimeStepsAll, st, type='Landings')
  # ProjSim <- GenerateProjectionData_Index(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, st, type=c('CPUE', 'Survey'))
  
  # TODO 
  # - CAL
  # - CAA
  # - Life history
  
  ProjSim
}


  