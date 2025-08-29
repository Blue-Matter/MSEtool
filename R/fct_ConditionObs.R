ConditionObs <- function(HistSim, HistTimeSteps, ProjectionTimeSteps) {
  
  # if (nStock(HistSim@OM)>1)
  #   cli::cli_abort('`ConditionObs` currently only working for one stock', .internal=TRUE)
  
  FisheryData <- HistSim@OM@Data[[1]]
  
  if (is.null(FisheryData)) 
    return(HistSim)

  # FisheryData # Life History 
  
  HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, 'Landings')
  HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, 'Removals')
  
  HistSim <- ConditionObs_Index(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, 'CPUE')
  HistSim <- ConditionObs_Index(HistSim, FisheryData, HistTimeSteps, ProjectionTimeSteps, 'Survey')
  

  # FisheryData@CAA
  
  # FisheryData@CAL
  
  HistSim
  
}

# ----- Catch ----

ConditionObs_Catch <- function(HistSim, FisheryData, HistTimeSteps,
                               ProjectionTimeSteps, type=c('Landings', 'Removals')) {
  
  type <- match.arg(type)
  
  
  # TODO multiple stocks 
  if (!inherits(FisheryData, 'data')) {
    cli::cli_abort('Currently can only handle OM@Data for a single stock', .internal=TRUE)
  }
  
  nHistTS <- length(HistTimeSteps)
  nProjTS <- length(ProjectionTimeSteps)
  
  ObservedCatch <- slot(FisheryData, type)@Value |>
    ArraySubsetTimeStep(TimeSteps=HistTimeSteps)
  
  nFleet <- ncol(ObservedCatch)
  
  SimulatedCatch <- purrr::map(slot(HistSim, type), \(stock) {
    List2Array(stock) |> AddDimNames(c('Age', 'Fleet', 'Area', 'TimeStep'), HistTimeSteps) |>
      apply(c('TimeStep', 'Fleet'), sum)
  }) |> 
    List2Array('Stock') |> 
    apply(c('TimeStep', 'Fleet'), sum)
  
  for (fl in 1:nFleet) {
    CatchObs <- slot(HistSim@OM@Obs[[1]][[fl]], type)
    CatchObs@Type <- slot(FisheryData,type)@Type[fl]
    
    SimValue <- SimulatedCatch[,fl]
    
    bias <- mean(SimValue/ObservedCatch[,fl], na.rm=TRUE) |> round(2)
    
    CatchObs@Bias <- array(bias, dim=nHistTS, dimnames = list(TimeStep=HistTimeSteps))
    
    CatchErrorHist <- (ObservedCatch[,fl]/(SimValue*CatchObs@Bias)) |> round(2)
    CatchErrorHist[!is.finite(CatchErrorHist)] <- NA
    
    if (is.null(CatchObs@TimeSteps)) 
      CatchObs@TimeSteps <- HistTimeSteps # TODO - currently calculates obs error from all historical time-steps
    
    CatchErrorHistCondition <- CatchErrorHist[as.character(CatchObs@TimeSteps)]
    CatchErrorHistCondition <- CatchErrorHistCondition/mean(CatchErrorHistCondition, na.rm=TRUE)
    CatchObs@CV <- as.numeric(CatchErrorHistCondition) |> sd(na.rm=TRUE) 
    SD <- CatchObs@CV 
    CatchErrorProj <- exp(rnorm(nProjTS, -((SD^2)/2), SD))
    CatchObs@Error <-c(CatchErrorHist,CatchErrorProj) |> 
      array(dimnames = list(TimeStep=c(HistTimeSteps, ProjectionTimeSteps)))
    slot(HistSim@OM@Obs[[1]][[fl]], type) <- CatchObs
    
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
                               type=c('CPUE', 'Survey'), st=1) {
  
  type <- match.arg(type)
  
  # TODO - condition index for specific timesteps - NSWO
  
  # TODO - fix for multiple stocks 
  
  if (!inherits(FisheryData, 'data')) {
    cli::cli_abort('Currently can only handle OM@Data for a single stock', .internal=TRUE)
  }
  
  nTS <- length(HistTimeSteps)
  
  NameIndices <- slot(FisheryData, type)@Name
  ObservedIndices <- slot(FisheryData, type)@Value |>
    ArraySubsetTimeStep(TimeSteps=HistTimeSteps)
  
  nFleet <- ncol(ObservedIndices)
  
  SimulatedNumber <- List2Array(HistSim@Number, 'Stock') |> 
    AddDimNames(c('Age', 'TimeStep', 'Area', 'Stock'),HistTimeSteps) |>
    # apply(c('Age', 'TimeStep'), sum)
    apply(c('Stock', 'Age', 'TimeStep'), sum)
  
  for (fl in 1:nFleet) {
    IndexObs <- slot(HistSim@OM@Obs[[1]][[NameIndices[fl]]],type)
    SelectivityAtAge <- slot(FisheryData, type)@Selectivity[[fl]]
    if (is.character(SelectivityAtAge)) {
      if (SelectivityAtAge == 'Biomass') {
        SelectivityAtAge <- matrix(1,nAge(HistSim@OM, 1), 1) |>
          AddDimNames(c('Age', 'TimeStep'), HistTimeSteps)
      } else if (SelectivityAtAge == 'SBiomass') {
        SelectivityAtAge <- HistSim@OM@Stock[[1]]@Maturity@MeanAtAge
      } else if (SelectivityAtAge == 'Obs') {
        SelectivityAtAge <- IndexObs@Selectivity
      }
    } else {
      SelectivityAtAge <- purrr::map(HistSim@OM@Fleet, \(stock) stock@Selectivity@MeanAtAge[,,fl]) |>
        List2Array('Stock') |> aperm(c('Stock', 'Age', 'TimeStep')) |>
        ArraySubsetTimeStep(HistTimeSteps)
    }
  
    ObservedIndex <- ObservedIndices[,fl]
    Units <- slot(FisheryData,type)@Units[fl]
    
    SimNumberSelected <- ArrayMultiply(SimulatedNumber, SelectivityAtAge) 
    
    if (Units=='Biomass') {
      WeightAtAge <- purrr::map(HistSim@OM@Stock, \(stock) stock@Weight@MeanAtAge |>
                   ArraySubsetTimeStep(HistTimeSteps)) |>
        List2Array('Stock') |> aperm(c('Stock', 'Age', 'TimeStep'))
      
      SimulatedIndex <- ArrayMultiply(WeightAtAge, SimNumberSelected) |> apply('TimeStep', sum)
    } else if (Units=='Number') {
      SimulatedIndex <- SimNumberSelected |> apply('TimeStep', sum)
    } else {
      cli::cli_abort('Not done yet!', .internal=TRUE)
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
    
    slot(HistSim@OM@Obs[[st]][[NameIndices[fl]]],type) <- IndexObs
  }
  
  HistSim
  
}

# ---- CAL ----



# ---- CAA ----