
GenerateProjectionData <- function(ProjSim, Year, YearsHist, YearsProj) {
  
  Complexes <- ProjSim@OM@Complexes
 
  YearsAll <- c(YearsHist, YearsProj)
  TSIndex <- match(Year, YearsAll) - 1
  DataYear <- YearsAll[TSIndex]
  
  # loop over complexes 
  for (i in seq_along(Complexes)) {
    stocks <- Complexes[[i]]
    if (max(ProjSim@Data[[i]]@Years) >= DataYear)
      next()
    
    ProjSim@Data[[i]]@Years <- YearsAll[1:TSIndex]
    
    ProjSim <- GenerateProjectionData_Effort(ProjSim, DataYear, 
                                            YearsAll, i, stocks)
    
    ProjSim <- GenerateProjectionData_Catch(ProjSim, DataYear, 
                                            YearsAll, i, stocks)
    
    ProjSim <- GenerateProjectionData_Catch(ProjSim, DataYear, 
                                            YearsAll, i, stocks, type='Discards')
    
    ProjSim <- GenerateProjectionData_Index(ProjSim, DataYear, 
                                            YearsHist, YearsAll, i, stocks)
    
    ProjSim <- GenerateProjectionData_Index(ProjSim, DataYear, 
                                            YearsHist, YearsAll, i, stocks, 'Survey')
    
    # TODO 
    # - CAL
    # - CAA
    # - Life history
    
  }
  
  ProjSim
}

GenerateProjectionData_Effort <- function(ProjSim, DataYear, YearsAll, i, stocks) {
  EffortData <- ProjSim@Data[[i]]@Effort
  if (EmptyObject(EffortData))
    return(ProjSim)
  
  FleetNames <- EffortData@Name
  
  TSIndex <- match(DataYear, YearsAll)
  
  SimEffort <- ProjSim@Effort[stocks,TSIndex,,drop=FALSE] |> apply(2:3, mean, na.rm=TRUE)
  dimnames(SimEffort) <- list(Year=DataYear, 
                             Fleet=FleetNames)
  
  Value <- EffortData@Value
  CV <- EffortData@CV
  
  if (DataYear %in% dimnames(Value)[[1]]) 
    return(ProjSim)
  
  nFleet <- length(FleetNames)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(Year=DataYear,
                                    Fleet=FleetNames))
  NewCV <- NewValue
  
  for (fl in 1:nFleet) {
    EffortObs <- ProjSim@OM@Obs[[i]][[fl]]@Effort
    if (EmptyObject(EffortObs)) {
      next()
    }
    if (length(EffortObs@Error)<1)
      next()
    
    if (!is.null(ProjSim@OM@Data[[i]]) && nrow(ProjSim@OM@Data[[i]]@Effort@Value)>=TSIndex) {
      NewValue[,fl] <- ProjSim@OM@Data[[i]]@Effort@Value[TSIndex,fl]
    } else {
      error <- ArraySubsetYear(EffortObs@Error, DataYear)
      bias <- ArraySubsetYear(EffortObs@Bias, DataYear)
      NewValue[,fl] <- SimEffort[fl] * error * bias
    }
    
    # CV 
    if (!is.null(ProjSim@OM@Data[[i]]) &&  nrow(ProjSim@OM@Data[[i]]@Effort@CV)>=TSIndex) {
      NewCV[,fl] <- ProjSim@OM@Data[[i]]@Effort@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetYear(EffortData@CV, DataYear)[fl]
    }
  }
  
  EffortData@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  EffortData@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  
  ProjSim@Data[[i]]@Effort <- EffortData
  ProjSim
}

GenerateProjectionData_Catch <- function(ProjSim, DataYear, YearsAll, i, 
                                         stocks, type=c('Landings', 'Discards')) {
  type <- match.arg(type)
  
  DataCatch <- slot(ProjSim@Data[[i]], type)
  if (EmptyObject(DataCatch))
    return(ProjSim)
  
  FleetNames <- DataCatch@Name
  SimCatch <- purrr::map(slot(ProjSim, type)[stocks], \(catch) 
                         catch[[as.character(DataYear)]] |> apply(2, sum) 
  ) |> List2Array('Stock', 'Fleet') |> rowSums() |> t()
  dimnames(SimCatch) <- list(Year=DataYear, 
                          Fleet=FleetNames)
  
 
  Value <- DataCatch@Value
  CV <- DataCatch@CV
  
  if (DataYear %in% dimnames(Value)[[1]]) 
    return(ProjSim)
  
  TSIndex <- match(DataYear, YearsAll)
  
  nFleet <- length(FleetNames)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(Year=DataYear,
                                    Fleet=FleetNames))
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
      
      error <- ArraySubsetYear(Obs@Error, DataYear)
      bias <- ArraySubsetYear(Obs@Bias, DataYear)
      NewValue[,fl] <- SimCatch[fl] * error * bias
    }
    
    # CV 
    if (!is.null(ProjSim@OM@Data[[i]]) &&  nrow(slot(ProjSim@OM@Data[[i]],type)@CV)>=TSIndex) {
      NewCV[,fl] <- slot(ProjSim@OM@Data[[i]],type)@CV[TSIndex,fl]
    } else {
      NewCV[,fl] <- SubsetYear(DataCatch@CV, DataYear)[fl]
    }
  }
  
  DataCatch@Value <- abind::abind(Value, NewValue, along=1, use.dnns=TRUE)
  DataCatch@CV <- abind::abind(CV, NewCV, along=1, use.dnns=TRUE)
  slot(ProjSim@Data[[i]],type) <- DataCatch
  ProjSim
}

GenerateProjectionData_Index <- function(ProjSim, DataYear, YearsHist, YearsAll, i, stocks,
                                         type=c('CPUE', 'Survey')) {
  
  # TODO hyperstability Beta not functional yet - ignored
  type <- match.arg(type)
  
  DataIndex <- slot(ProjSim@Data[[i]], type)
  if (EmptyObject(DataIndex))
    return(ProjSim)
  Value <- DataIndex@Value
  CV <- DataIndex@CV
  
  if (DataYear %in% dimnames(Value)[[1]]) {
    # data already exists
    return(ProjSim)
  }
  
  YearsProj <- YearsAll[!YearsAll %in% YearsHist]
  TSIndex <- match(DataYear, YearsAll)
  
  nFleet <- ncol(Value)
  NewValue <- array(NA, dim=c(1, nFleet),
                    dimnames = list(Year=DataYear,
                                    Fleet=DataIndex@Name))
  NewCV <- NewValue
  
  FleetIndex <- match(DataIndex@Name, names(ProjSim@OM@Obs[[i]]))
  
  SimulatedNumberList <- purrr::map(ProjSim@Number[stocks], \(stock) {
    stock[,TSIndex,,drop=FALSE] |> AddDimNames(c('Age', 'Year', 'Area'),DataYear) |>
      apply(c('Age', 'Year'), sum)
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
    if (!is.null(ProjSim@OM@Data[[i]]) && nrow(slot(ProjSim@OM@Data[[i]],type)@Value)>=TSIndex) {
      # check if real data exists
      NewValue[,fl] <- slot(ProjSim@OM@Data[[i]],type)@Value[TSIndex,fl]
    } else {
      if (!is.na(DataIndex@Timing[fl]) && DataIndex@Timing[fl]!=0)
        cli::cli_alert_warning('`Index@Timing` not working yet. Calculating from beginning of time step')
      
      SelectivityAtAge <- DataIndex@Selectivity[fl]
      SelectivityAtAgeList <- MakeNamedList(StockNames(ProjSim@OM)[stocks])
      
      if (is.character(SelectivityAtAge)) {
        if (SelectivityAtAge == 'Biomass') {
          for (st in seq_along(stocks)) {
            SelectivityAtAgeList[[st]] <- matrix(1,nAge(ProjSim@OM, stocks[st]), 1) |>
              AddDimNames(c('Age', 'Year'), DataYear)
          }
        } else if (SelectivityAtAge == 'SBiomass') {
          for (st in seq_along(stocks)) {
            SelectivityAtAgeList[[st]] <- ProjSim@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge |> 
              ArraySubsetYear(DataYear) 
          }
        } else if (SelectivityAtAge == 'Obs') {
          SelectivityAtAgeList <- purrr::map(IndexObs@Selectivity, \(selectivity)
                                             ArraySubsetYear(selectivity, DataYear) )
                                             
        }
      } else {
        SelectivityAtAgeList <- purrr::map(ProjSim@OM@Fleet[stocks], \(stock) {
          stock@Selectivity@MeanAtAge[,,fl] |>
            ArraySubsetYear(DataYear) 
        }) 
      }
      
      SimNumberSelectedList <- purrr::map2(SimulatedNumberList, SelectivityAtAgeList, ArrayMultiply)
      
      if (DataIndex@Units[fl] == 'Biomass') {
        WeightAtAgeList <- purrr::map(ProjSim@OM@Stock[stocks], \(stock) stock@Weight@MeanAtAge |>
                                        ArraySubsetYear(DataYear)) 
        
        SimulatedIndex <- purrr::map2(SimNumberSelectedList, WeightAtAgeList, ArrayMultiply) |>
          purrr::map(apply, 'Year', sum) |>
          List2Array('Stock', 'Year') |>
          AddDimNames(c('Year', 'Stock'), DataYear) |> 
          apply(c('Year'), sum)

      } else if (DataIndex@Units[fl] == 'Number') {
        SimulatedIndex <- SimNumberSelectedList |>
          purrr::map(apply, 'Year', sum) |>
          List2Array('Stock', 'Year') |>
          AddDimNames(c('Year', 'Stock'), DataYear) |> 
          apply(c('Year'), sum) 
      } else {
        cli::cli_abort('Only `Biomass` and `Number` supported for `Units` in `Data@CPUE` and `Data@Survey`', .internal=TRUE)
      }
      
      NewValue[,fl] <- SimulatedIndex * IndexObs@q*IndexObs@Error[TSIndex]
    }
    
    # CV 
    if (!is.null(ProjSim@OM@Data[[i]]) &&  nrow(slot(ProjSim@OM@Data[[i]],type)@CV)>=TSIndex) {
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


