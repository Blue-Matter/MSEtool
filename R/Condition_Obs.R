ConditionObs <- function(HistSim, HistTimeSteps) {
  
  if (nStock(HistSim@OM)>1)
    cli::cli_abort('`ConditionObs` currently only working for one stock', .internal=TRUE)
  
  FisheryData <- HistSim@OM@Data
  
  if (is.null(FisheryData)) 
    return(HistSim)

  StockNames <- StockNames(HistSim@OM)
  FleetNames <- dimnames(FisheryData@Catch@Value)[[2]] # TODO function to grab from `data` and add FleetNames to `data`
  

  # FisheryData # Life History 
  
  # TODO - grab from OM so not to over-write - do this earlier
  HistSim@OM@Obs <- MakeNamedList(StockNames(HistSim@OM)) |>
    purrr::map(\(obs) 
               MakeNamedList(FleetNames, new('obs'))
    )
  

  HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistTimeSteps)
  HistSim <- ConditionObs_Index(HistSim, FisheryData, HistTimeSteps)
  
  # FisheryData@CAA
  # 
  # FisheryData@CAL
  
  HistSim
  
}

ConditionObs_Catch <- function(HistSim, FisheryData, HistTimeSteps, st=1) {
  
  nTS <- length(HistTimeSteps)
  
  ObservedCatch <- FisheryData@Catch@Value |>
    ArraySubsetTimeStep(TimeSteps=HistTimeSteps)
  
  nFleet <- ncol(ObservedCatch)
  
  SimulatedRemovals <- HistSim@Removals[[st]] |>
    List2Array() |> 
    AddDimNames(c('Age', 'Fleet', 'Area', 'TimeStep'), HistTimeSteps) |>
    apply(c('TimeStep', 'Fleet'), sum)
  
  SimulatedLandings <- HistSim@Landings[[st]] |>
    List2Array() |> 
    AddDimNames(c('Age', 'Fleet', 'Area', 'TimeStep'), HistTimeSteps) |>
    apply(c('TimeStep', 'Fleet'), sum)
  
 
  
  for (fl in 1:nFleet) {
    CatchObs <- HistSim@OM@Obs[[st]][[fl]]@Catch
    CatchObs@Type <- FisheryData@Catch@Type[fl]
    
    if (CatchObs@Type =='Removals') {
      SimValue <- SimulatedRemovals[,fl]
    } else {
      SimValue <- SimulatedLandings[,fl]
    }
    
    bias <- mean(SimValue/ObservedCatch[,fl], na.rm=TRUE) |> round(2)
    
    CatchObs@Bias <- array(bias, dim=c(nTS), dimnames = list(TimeStep=HistTimeSteps))
    
    CatchError <- (ObservedCatch[,fl]/(SimValue*CatchObs@Bias)) |> round(2)
    CatchError[!is.finite(CatchError)] <- NA
    
    if (is.null(CatchObs@TimeSteps)) 
      CatchObs@TimeSteps <- HistTimeSteps # TODO - currently calculates obs error from all historical time-steps
    
    CatchErrorCondition <- CatchError[as.character(CatchObs@TimeSteps)]
    CatchErrorCondition <- CatchErrorCondition/mean(CatchErrorCondition, na.rm=TRUE)
    CatchObs@CV <- as.numeric(CatchErrorCondition) |> sd(na.rm=TRUE)
    CatchObs@Error <- CatchError
    HistSim@OM@Obs[[st]][[fl]]@Catch <- CatchObs
    
  }
  HistSim
}

CalcIndexResiduals <- function(ObservedIndex, SimulatedIndex, beta=1) {
  if (any(ObservedIndex<0, na.rm=TRUE))
    cli::cli_abort('`Data@Index` cannot have negative values. Standardize to positive values with mean 1')
  
  # standardize index and biomass to mean 1
  StObservedIndex <- ObservedIndex/mean(ObservedIndex, na.rm=TRUE)
  notnas <- !is.na(StObservedIndex)
  StSimulatedIndex <- SimulatedIndex/mean(SimulatedIndex[notnas], na.rm=TRUE)
  
  LogStObservedIndex <- log(StObservedIndex)
  LogStSimulatedIndex <- log(StSimulatedIndex)
  
  Residuals <- LogStObservedIndex - LogStSimulatedIndex
  out <- list(Residuals=Residuals, beta=beta)
}

ConditionObs_Index <- function(HistSim, FisheryData, HistTimeSteps, st=1) {
  
  nTS <- length(HistTimeSteps)
  
  ObservedIndices <- FisheryData@Index@Value |>
    ArraySubsetTimeStep(TimeSteps=HistTimeSteps)
  
  # TODO - total biomass, spawning biomass, vulnerable biomass
  nFleet <- ncol(ObservedIndices)
  
  SimNumber <- HistSim@Number[[st]] |> 
    apply(1:2, sum) |> # sum over areas 
    AddDimNames(c('Age', 'TimeStep'), HistTimeSteps)
  
  for (fl in 1:nFleet) {
    IndexObs <- HistSim@OM@Obs[[st]][[fl]]@Index
    
    ObservedIndex <- ObservedIndices[,fl]
    Units <- FisheryData@Index@Units[fl]
    
    SelectivityAtAge <- FisheryData@Index@Selectivity[[fl]]
    SimNumberSelected <- ArrayMultiply(SimNumber, SelectivityAtAge)
    
    if (Units=='Biomass') {
      WeightAtAge <- ArraySubsetTimeStep(HistSim@OM@Stock[[st]]@Weight@MeanAtAge, HistTimeSteps)
      SimulatedIndex <- ArrayMultiply(WeightAtAge, SimNumberSelected) |> apply('TimeStep', sum)
    } else if (Units=='Numbers') {
      SimulatedIndex <- SimNumberSelected |> apply('TimeStep', sum)
    } else {
      cli::cli_abort('Not done yet!', .internal=TRUE)
    }

    NonNAInd <- which(!is.na(ObservedIndex))
    SimulatedIndex <- SimulatedIndex/mean(SimulatedIndex[NonNAInd], na.rm=TRUE) * mean(ObservedIndex, na.rm=TRUE)
    ResidualsBeta <- CalcIndexResiduals(ObservedIndex, SimulatedIndex, beta=1)  # TODO  doesn't fit beta parameter for now; always assumes beta = 1
    IndexObs@Beta <- ResidualsBeta$beta
    IndexObs@Error <- ResidualsBeta$Residuals
    
     # UP TO HERE 
    # TODO -
    # calculate AC, SD, of resids
    # report index fits
    # generate index obs for projection
    # generate catch obs for projection

    # generate data for the projections ... 
    
 
    
    IndexObs@Error
    IndexObs@Beta
    IndexObs@AC
    IndexObs@TimeSteps
    IndexObs@Type
    
    
    FisheryData@Index@Units
    
    CatchObs@Type <- FisheryData@Index@Type[fl]
  
    HistSim@OM@Obs[[st]][[fl]]@Index <- IndexObs
    
  }
  
  HistSim
  
  
}