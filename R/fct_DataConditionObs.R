# HistSimHistSim <- HistSimList$`1`

ConditionObs <- function(HistSim, HistTimeSteps, ProjectionTimeSteps) {
  
  FisheryDataList <- HistSim@OM@Data
  nData <- length(FisheryDataList)
  
  if (nData<1)
    return(HistSim)
  
  if (nData>1) {
    cli::cli_alert_warning('Observation Error Conditioning is currently not supported for OMs with more than one set of real fishery data. `Obs` will not be calculated!')
    return(HistSim)
  }
    
  Complexes <- HistSim@OM@Complexes

  # FisheryData # Life History
  for (i in seq_along(FisheryDataList)) {
    FisheryData <- FisheryDataList[[i]]
    stocks <- Complexes[[i]]
    HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, stocks, i)
    HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, stocks, i, 'Discards')
    
    HistSim <- ConditionObs_Index(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, stocks, i)
    HistSim <- ConditionObs_Index(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps,  stocks, i, 'Survey')
  }

  # FisheryData@CAA
  
  # FisheryData@CAL
  
  HistSim
  
}

# ----- Catch ----

ConditionObs_Catch <- function(HistSim, FisheryData, HistTimeSteps,
                               ProjectionTimeSteps, stocks, i, type=c('Landings', 'Discards')) {
  
  type <- match.arg(type)
  nHistTS <- length(HistTimeSteps)
  nProjTS <- length(ProjectionTimeSteps)
  
  ObservedCatch <- slot(FisheryData, type)@Value |>
    ArraySubsetTimeStep(TimeSteps=HistTimeSteps)
  
  nFleet <- ncol(ObservedCatch)
  
  catchList <- slot(HistSim, type)[stocks]
  
  SimulatedCatch <- purrr::map(catchList, \(catch) {
    List2Array(catch) |>
    AddDimNames(c('Age', 'Fleet', 'Area', 'TimeStep'), HistTimeSteps) |>
      apply(c('TimeStep', 'Fleet'), sum)
  }) |> 
    List2Array('Stock') |> 
    apply(c('TimeStep', 'Fleet'), sum)
  
  for (fl in 1:nFleet) {
    CatchObs <- slot(HistSim@OM@Obs[[i]][[fl]], type)
    CatchObs@Type <- slot(FisheryData,type)@Type[fl]
    
    if (is.null(CatchObs@TimeSteps)) 
      CatchObs@TimeSteps <- HistTimeSteps
    
    SimValue <- SimulatedCatch[,fl]
    SimValue[SimValue<0] <- 1E-15
    
    # Bias 
    Bias <- ArraySubsetTimeStep(ObservedCatch[,fl], CatchObs@TimeSteps)/ArraySubsetTimeStep(SimValue, CatchObs@TimeSteps)
    Bias[Bias<0.001] <- NA
    Bias <- Bias[is.finite(Bias)]
    BiasMean <- mean(Bias, na.rm=TRUE) 
    CatchObs@Bias <- array(BiasMean, dim=nHistTS, dimnames = list(TimeStep=HistTimeSteps))
    
    # Error
    CatchErrorHist <- (ObservedCatch[,fl]/(SimValue*CatchObs@Bias))
    CatchErrorHist[!is.finite(CatchErrorHist)] <- NA
  
    CatchErrorHistCondition <- CatchErrorHist[as.character(CatchObs@TimeSteps)]
    CatchErrorHistCondition[CatchErrorHistCondition<0.001] <- NA
    CatchErrorHistCondition <- CatchErrorHistCondition/mean(CatchErrorHistCondition, na.rm=TRUE)
    CatchObs@CV <- as.numeric(CatchErrorHistCondition) |> sd(na.rm=TRUE) 
    SD <- CatchObs@CV 
    if (!is.finite(SD))
      SD <- 1E-6
    
    CatchErrorProj <- exp(rnorm(nProjTS, -((SD^2)/2), SD))
    CatchObs@Error <-c(CatchErrorHist,CatchErrorProj) |> 
      array(dimnames = list(TimeStep=c(HistTimeSteps, ProjectionTimeSteps)))
    slot(HistSim@OM@Obs[[i]][[fl]], type) <- CatchObs
    
  }
  HistSim
}

# ---- Index ----

CalcIndexResiduals <- function(ObservedIndex, SimulatedIndex, beta=1) {
  if (any(ObservedIndex<0, na.rm=TRUE))
    cli::cli_abort('`Data@Index` cannot have negative values. Standardize to positive values with mean 1')
  
  # standardize index and biomass to mean 1
  StObservedIndex <- ObservedIndex/mean(ObservedIndex, na.rm=TRUE)
  notnas <- !is.na(StObservedIndex)
  StSimulatedIndex <- SimulatedIndex/mean(SimulatedIndex[notnas], na.rm=TRUE)
  
  LogStObservedIndex <- log(StObservedIndex)
  LogStSimulatedIndex <- log(StSimulatedIndex)
  
  LogResiduals <- LogStObservedIndex - LogStSimulatedIndex
  out <- list(LogResiduals=LogResiduals, beta=beta)
}

CalcResidualStats <- function(LogResiduals) {
  non.nas <- which(!is.na(LogResiduals))
  Residual.Groups <- split(non.nas, cumsum(c(1, diff(non.nas) != 1)))
  
  # calculate auto-correlation for each group of contiguous residuals
  group.length <- Residual.Groups %>% lapply(length) %>% unlist() %>% as.numeric()
  group.ind <- which(group.length>1) 
  ac.group <- vector('numeric', length=length(group.ind))
  ac.group.n <- ac.group
  
  cnt <- 0
  for (n in group.ind) {
    cnt <- cnt+1
    ac.group[cnt] <- acf(LogResiduals[Residual.Groups[[n]]], plot=F)$acf[2,1,1]
    ac.group.n[cnt] <- group.length[n]
  }
  ac <- weighted.mean(ac.group, ac.group.n)
  ac[ac<0] <- 0 # https://github.com/Blue-Matter/MSEtool/issues/65
  
  sd <- sd(LogResiduals, na.rm=TRUE)
  non.na.res <- LogResiduals[!is.na(LogResiduals)]
  data.frame(AC=ac, SD=sd) # log-space residuals
}

GenerateIndexResiduals <- function(Stats, ProjectionTimeSteps) {
  sd <- Stats$SD
  ac <- Stats$AC
  ac[!is.finite(ac)] <- 0
  LastError <- Stats$LastError
  nTS <- length(ProjectionTimeSteps)
  
  if (all(is.na(sd))) {
    cli::cli_abort('Not done yet!!', .internal=TRUE)
  }
  
  mu <- -0.5 * (sd)^2 * (1 - ac)/sqrt(1 - ac^2)
  
  Residuals <- array(rnorm(nTS, mu, sd), nTS, dimnames = list(TimeStep=ProjectionTimeSteps))
  # apply a pseudo AR1 autocorrelation
  Residuals <- ApplyIndexAC(Residuals, ac, LastError)
  exp(Residuals)
}

ApplyIndexAC <- function(Residuals, ac, LastError) {
  for (ts in seq_along(Residuals)) {
    if (ts == 1) {
      Residuals[ts] <- ac * LastError + Residuals[ts] * (1-ac * ac)^0.5
    } else {
      Residuals[ts] <- ac * Residuals[ts-1] + Residuals[ts] * (1-ac * ac)^0.5
    }
  }
  Residuals
}

ConditionObs_Index <- function(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, 
                               stocks, i, type=c('CPUE', 'Survey')) {
  
  type <- match.arg(type)
  
  nTS <- length(HistTimeSteps)
  
  NameIndices <- slot(FisheryData, type)@Name
  ObservedIndices <- slot(FisheryData, type)@Value |>
    ArraySubsetTimeStep(TimeSteps=HistTimeSteps)
  
  nFleet <- ncol(ObservedIndices)
  
  SimulatedNumberList <- purrr::map(HistSim@Number[stocks], \(stock) {
    stock |> AddDimNames(c('Age', 'TimeStep', 'Area'),HistTimeSteps) |>
      apply(c('Age', 'TimeStep'), sum)
  })
  
  for (fl in 1:nFleet) {
    IndexObs <- slot(HistSim@OM@Obs[[i]][[NameIndices[fl]]],type)
    SelectivityAtAge <- slot(FisheryData, type)@Selectivity[[fl]]
    
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
  
    ObservedIndex <- ObservedIndices[,fl]
    Units <- slot(FisheryData,type)@Units[fl]
    
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
      cli::cli_abort('Only `Biomass` and `Number` supported for `Units` in `Data@CPUE` and `Data@Survey`', .internal=TRUE)
    }

    NonNAInd <- which(!is.na(ObservedIndex))
    IndexObs@q <- mean(ObservedIndex[NonNAInd], na.rm=TRUE)/mean(SimulatedIndex[NonNAInd], na.rm=TRUE)
    SimulatedIndex <- SimulatedIndex * IndexObs@q
    
    if (is.null(IndexObs@TimeSteps))
      IndexObs@TimeSteps <- HistTimeSteps[NonNAInd]
    
    TSInd <- match(IndexObs@TimeSteps, HistTimeSteps)
    
    # TODO  doesn't fit beta parameter for now; always assumes beta = 1
    # also need to account for TSInd when calculating beta
    ResidualsBeta <- CalcIndexResiduals(ObservedIndex, SimulatedIndex, beta=1)  
    
    IndexObs@Beta <- ResidualsBeta$beta
    LogResiduals <- ResidualsBeta$LogResiduals
    Stats <- CalcResidualStats(LogResiduals[TSInd])
    Stats$LastError <- LogResiduals[NonNAInd] |> tail(1)
    IndexObs@CV <- Stats$SD
    IndexObs@AC <- Stats$AC

    # TODO - CheckIndexFit - see check_Index_Fit
    
    # Generate residuals for projections
    ResidualsHistorical <- exp(LogResiduals)
    ResidualsProjection <- GenerateIndexResiduals(Stats, ProjectionTimeSteps)
    
    # TODO - option to discard indices that are NA for x timesteps before terminal historical year
    IndexObs@Error <- c(ResidualsHistorical, ResidualsProjection) |> 
      array(dimnames = list(TimeStep=c(HistTimeSteps,  ProjectionTimeSteps)))
    
    slot(HistSim@OM@Obs[[i]][[NameIndices[fl]]],type) <- IndexObs
  }
  
  HistSim
  
}

# ---- CAL ----



# ---- CAA ----