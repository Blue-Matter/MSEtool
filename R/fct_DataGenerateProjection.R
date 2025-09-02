
GenerateProjectionData <- function(ProjSim, TimeStep, TimeStepsHist, TimeStepsProj) {
  
  Complexes <- ProjSim@OM@Complexes
 
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj)
  TSIndex <- match(TimeStep, TimeStepsAll) - 1
  DataTimeStep <- TimeStepsAll[TSIndex]
  
  # loop over complexes 
  for (i in seq_along(Complexes)) {
    stocks <- Complexes[[i]]
    if (max(ProjSim@Data[[i]]@TimeSteps) == DataTimeStep)
      next()
    
    ProjSim@Data[[i]]@TimeSteps <- TimeStepsAll[1:TSIndex]
    ProjSim <- GenerateProjectionData_Catch(ProjSim, DataTimeStep, TimeStepsAll, i, stocks)
    ProjSim <- GenerateProjectionData_Catch(ProjSim, DataTimeStep, TimeStepsAll, i, stocks, type='Discards')
    
    ProjSim <- GenerateProjectionData_Index(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, i, stocks)
    ProjSim <- GenerateProjectionData_Index(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, i, stocks, 'Survey')
    
    # TODO 
    # - CAL
    # - CAA
    # - Life history
    
  }
  
  ProjSim
}


GenerateProjectionData_Catch <- function(ProjSim, DataTimeStep, TimeStepsAll, i, 
                                         stocks, type=c('Landings', 'Discards')) {
  type <- match.arg(type)
  
  DataCatch <- slot(ProjSim@Data[[i]], type)
  if (EmptyObject(DataCatch))
    return(ProjSim)
  
  FleetNames <- DataCatch@Name
  SimCatch <- purrr::map(slot(ProjSim, type)[stocks], \(catch) 
                         catch[[as.character(DataTimeStep)]] |>  apply(2:3, sum) |> t()
  ) |> List2Array('Stock') |>  apply(1:2, sum)
  dimnames(SimCatch) <- list(TimeStep=DataTimeStep, 
                          Fleet=FleetNames)
  
 
  Value <- DataCatch@Value
  CV <- DataCatch@CV
  
  if (DataTimeStep %in% dimnames(Value)[[1]]) {
    # data already exists
    return(ProjSim)
  }
  
  TSIndex <- match(DataTimeStep, TimeStepsAll)
  
  nFleet <- ncol(Value)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(TimeStep=DataTimeStep,
                                    Fleet=DataCatch@Name))
  NewCV <- NewValue
  
  # loop over fleets 
  for (fl in 1:nFleet) {
    Obs <- slot(ProjSim@OM@Obs[[i]][[fl]], type)
    
    if (length(Obs@Error)<1)
      next()
  
    # Catch 
    if (!is.null(ProjSim@OM@Data[[i]]) && nrow(slot(ProjSim@OM@Data[[i]],type)@Value)>=TSIndex) {
      NewValue[,fl] <- slot(ProjSim@OM@Data[[i]],type)@Value[TSIndex,fl]
    } else {
      if (DataCatch@Units[fl] != 'Biomass')
        cli::cli_alert_warning('Currently Landings & Discards data can only be in units of Biomass. Use `FleetWeight=1`')
      
      error <- ArraySubsetTimeStep(Obs@Error, DataTimeStep)
      bias <- ArraySubsetTimeStep(Obs@Bias, DataTimeStep)
      NewValue[,fl] <- SimCatch[fl] * error * bias
    }
    
    # CV 
    if (nrow(slot(ProjSim@OM@Data[[i]],type)@CV)>=TSIndex) {
      NewCV[,fl] <- slot(ProjSim@OM@Data[[i]],type)@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetTimeStep(DataCatch@CV, DataTimeStep)[fl]
    }
  }
  
  DataCatch@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  DataCatch@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  slot(ProjSim@Data[[i]],type) <- DataCatch
  ProjSim
}

GenerateProjectionData_Index <- function(ProjSim, DataTimeStep, TimeStepsHist, TimeStepsAll, i, stocks,
                                         type=c('CPUE', 'Survey')) {
  
  # TODO hyperstability Beta not functional yet - ignored
  type <- match.arg(type)
  
  DataIndex <- slot(ProjSim@Data[[i]], type)
  if (EmptyObject(Index))
    return(ProjSim)
  Value <- DataIndex@Value
  CV <- DataIndex@CV
  
  if (DataTimeStep %in% dimnames(Value)[[1]]) {
    # data already exists
    return(ProjSim)
  }
  
  TimeStepsProj <- TimeStepsAll[!TimeStepsAll %in% TimeStepsHist]
  TSIndex <- match(DataTimeStep, TimeStepsAll)
  
  nFleet <- ncol(Value)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(TimeStep=DataTimeStep,
                                    Fleet=DataIndex@Name))
  NewCV <- NewValue
  
  FleetIndex <- match(DataIndex@Name, names(ProjSim@OM@Obs[[i]]))
  
  SimulatedNumberList <- purrr::map(ProjSim@Number[stocks], \(stock) {
    stock[,TSIndex,,drop=FALSE] |> AddDimNames(c('Age', 'TimeStep', 'Area'),DataTimeStep) |>
      apply(c('Age', 'TimeStep'), sum)
  })
  
  # loop over fleets 
  for (fl in 1:nFleet) {
    Obs <- ProjSim@OM@Obs[[i]][[FleetIndex[fl]]]
    IndexObs <- slot(Obs,type)
    if (length(IndexObs)<1)
      next()
    
    # TODO - make this an option
    # currently doesn't simulate index if last five data points were NAs
    if (all(!is.finite(tail(Value[,fl],5)))) 
      next()
    
    # Index 
    if (nrow(slot(ProjSim@OM@Data[[i]],type)@Value)>=TSIndex) {
      # check if real data exists
      NewValue[,fl] <- slot(ProjSim@OM@Data[[i]],type)@Value[TSIndex,fl]
    } else {
      if (DataIndex@Timing[fl]!=0)
        cli::cli_alert_warning('`Index@Timing` not working yet. Calculating from beginning of time step')
      
      SelectivityAtAge <- DataIndex@Selectivity[fl]
      SelectivityAtAgeList <- MakeNamedList(StockNames(ProjSim@OM)[stocks])
      
      if (is.character(SelectivityAtAge)) {
        if (SelectivityAtAge == 'Biomass') {
          for (st in seq_along(stocks)) {
            SelectivityAtAgeList[[st]] <- matrix(1,nAge(ProjSim@OM, stocks[st]), 1) |>
              AddDimNames(c('Age', 'TimeStep'), DataTimeStep)
          }
        } else if (SelectivityAtAge == 'SBiomass') {
          for (st in seq_along(stocks)) {
            SelectivityAtAgeList[[st]] <- ProjSim@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge |> 
              ArraySubsetTimeStep(DataTimeStep) 
          }
        } else if (SelectivityAtAge == 'Obs') {
          SelectivityAtAgeList <- purrr::map(IndexObs@Selectivity, \(selectivity)
                                             ArraySubsetTimeStep(selectivity, DataTimeStep) )
                                             
        }
      } else {
        SelectivityAtAgeList <- purrr::map(ProjSim@OM@Fleet[stocks], \(stock) {
          stock@Selectivity@MeanAtAge[,,fl] |>
            ArraySubsetTimeStep(DataTimeStep) 
        }) 
      }
      
      SimNumberSelectedList <- purrr::map2(SimulatedNumberList, SelectivityAtAgeList, ArrayMultiply)
      
      if (DataIndex@Units[fl] == 'Biomass') {
        WeightAtAgeList <- purrr::map(HistSim@OM@Stock[stocks], \(stock) stock@Weight@MeanAtAge |>
                                        ArraySubsetTimeStep(DataTimeStep)) 
        
        SimulatedIndex <- purrr::map2(SimNumberSelectedList, WeightAtAgeList, ArrayMultiply) |>
          purrr::map(apply, 'TimeStep', sum) |>
          List2Array('Stock', 'TimeStep') |>
          AddDimNames(c('TimeStep', 'Stock'), DataTimeStep) |> 
          apply(c('TimeStep'), sum)

      } else if (DataIndex@Units[fl] == 'Number') {
        SimulatedIndex <- SimNumberSelectedList |>
          purrr::map(apply, 'TimeStep', sum) |>
          List2Array('Stock', 'TimeStep') |>
          AddDimNames(c('TimeStep', 'Stock'), DataTimeStep) |> 
          apply(c('TimeStep'), sum) 
      } else {
        cli::cli_abort('Only `Biomass` and `Number` supported for `Units` in `Data@CPUE` and `Data@Survey`', .internal=TRUE)
      }
      
      NewValue[,fl] <- SimulatedIndex * IndexObs@q*IndexObs@Error[TSIndex]
    }
    
    # CV 
    if (nrow(slot(ProjSim@OM@Data[[i]],type)@CV)>=TSIndex) {
      NewCV[,fl] <- slot(ProjSim@OM@Data[[i]],type)@CV[TSIndex,fl]
    } else {
      previouscv <- DataIndex@CV[,fl]
      previouscv <- previouscv[!is.na(previouscv)] |> tail(1) |> as.numeric()
      NewCV[,fl] <- previouscv
    }
  }
  DataIndex@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  DataIndex@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  slot(ProjSim@Data[[i]],type) <- DataIndex
  ProjSim
}


