
#' @export
ProjectionTimeStep <- function(Data) {
  length(Data@TimeSteps[Data@TimeSteps>Data@TimeStepLH])+1
}




GenerateProjectionData_Catch <- function(ProjSim, DataTimeStep, TimeStepsAll, st=1) {
  
  # TODO needs to have a stock index in ProjSim@OM@Data at some point
  # TODO catch units in numbers
  Removals <- ProjSim@Removals[[st]][[as.character(DataTimeStep)]] |>
    apply(2, sum) # sum over ages and areas
  Landings <- ProjSim@Landings[[st]][[as.character(DataTimeStep)]] |>
    apply(2, sum) # sum over age and areas
  
  Catch <- ProjSim@Data[[st]]@Catch
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
    # check if real data exists
    
    # Catch 
    if (nrow(ProjSim@OM@Data@Catch@Value)>=TSIndex) {
      NewValue[,fl] <- ProjSim@OM@Data@Catch@Value[TSIndex,fl]
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
    if (nrow(ProjSim@OM@Data@Catch@CV)>=TSIndex) {
      NewCV[,fl] <- ProjSim@OM@Data@Catch@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- Catch@CV[TSIndex-1,fl]
    }
  }
  Catch@Value <- abind::abind(Value, NewValue, along=1)
  Catch@CV <- abind::abind(CV, NewCV, along=1)
  ProjSim@Data[[st]]@Catch <- Catch
  ProjSim
}

GenerateProjectionData_Index <- function(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, st=1) {
  
  # TODO hyperstability Beta not functional yet - ignored

  Index <- ProjSim@Data[[st]]@Index
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
    if (nrow(ProjSim@OM@Data@Index@Value)>=TSIndex) {
      # check if real data exists
      NewValue[,fl] <- ProjSim@OM@Data@Index@Value[TSIndex,fl]
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
      
      NewValue[,fl] <- SimulatedValue * ProjSim@OM@Obs[[st]][[FleetIndex[fl]]]@Index@q*Obs@Index@Error[TSIndex]
    }
    
    # CV 
    if (nrow(ProjSim@OM@Data@Index@CV)>=TSIndex) {
      NewCV[,fl] <- ProjSim@OM@Data@Index@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- Index@CV[TSIndex-1,fl]
    }
  }
  Index@Value <- abind::abind(Value, NewValue, along=1)
  Index@CV <- abind::abind(CV, NewCV, along=1)
  ProjSim@Data[[st]]@Index <- Index
  ProjSim
}


GenerateProjectionData <- function(ProjSim, TimeStep, TimeStepsHist, TimeStepsProj) {
  
  st <- 1 # TODO multiple stocks
  
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj)
  
  # TODO make DataLag an option 
  DataLag <- 1 # this function is run at the beginning of implementation time step
  # so DataLag = 1 is the data from the time step (year) before the MP is implemented
  
  DataTimeStep <- TimeStep-DataLag
  
  ProjSim@Data[[st]]@TimeSteps <- TimeStepsAll[1:(match(TimeStep, TimeStepsAll)-DataLag)]
  
  ProjSim <- GenerateProjectionData_Catch(ProjSim, DataTimeStep, TimeStepsAll, st)
  ProjSim <- GenerateProjectionData_Index(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, st)
  
  # TODO 
  # - CAL
  # - CAA
  # - Life history

  ProjSim
}


#' Projects a single MP from the output of `Simulate`
ProjectMP <- function(ProjSim, MP, TimeStepsHist, TimeStepsProj, ManagementTimeSteps) {
  # tictoc::tic("Project TimeSteps")

  for (TimeStep in TimeStepsProj) {
    
    # Generate Data up to TimeStep - 1
    # TODO - add option for Data Lag 
    ProjSim <- GenerateProjectionData(ProjSim, TimeStep, TimeStepsHist, TimeStepsProj)

    
    # --- Update `ProjSim` with MP Advice ----
    # tictoc::tic("Apply MP")
    
    ProjSim <- ApplyMPInternal(ProjSim, 
                               MP, 
                               TimeStep, 
                               TimeStepsHist,
                               TimeStepsProj,
                               ManagementTimeSteps)
    
    # tictoc::toc()
    
    # --- Simulate Pop Dynamics for this Time Step ----
    # tictoc::tic("Update Dynamics")
 
    
    ProjSim <- SimulateDynamics_(ProjSim, TimeStep)
    # tictoc::toc()
    
  } 
  # tictoc::toc()
  ProjSim
}

