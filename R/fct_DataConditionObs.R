# HistSimHistSim <- HistSimList$`1`

# TODO - check for identical sims - but need to generate independent obs error by sim
# TODO - add conditioning obs error for Effort

ConditionObs <- function(SimList, HistYears, ProjYears) {
  HistSim <- SimList$`1` # for debugging
  
  SimList <- purrr::map(SimList, \(HistSim)
                        ConditionObs_Sim(HistSim, HistYears, ProjYears),
                        .progress = list(
                          type = "iterator",
                          format = "Conditioning Observation Error on Provided Fishery Data {cli::pb_bar} {cli::pb_percent}",
                          clear = TRUE))
  class(SimList) <- 'simlist'
  SimList
}


ConditionObs_Sim <- function(HistSim, HistYears, ProjYears) {
  
  FisheryDataList <- HistSim@OM@Data
  nData <- length(FisheryDataList)
  
  if (nData<1)
    return(HistSim)

  Complexes <- HistSim@OM@Complexes

  # FisheryData # Life History
  for (i in seq_along(FisheryDataList)) {
    FisheryData <- FisheryDataList[[i]]
    stocks <- Complexes[[i]]
    HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistYears, ProjYears, stocks, i)
    HistSim <- ConditionObs_Catch(HistSim, FisheryData, HistYears, ProjYears, stocks, i, 'Discards')
    
    HistSim <- ConditionObs_Index(HistSim, FisheryData, HistYears, ProjYears, stocks, i)
    HistSim <- ConditionObs_Index(HistSim, FisheryData, HistYears, ProjYears,  stocks, i, 'Survey')
  }

  # FisheryData@CAA
  
  # FisheryData@CAL
  
  HistSim
  
}

# ----- Catch ----

ConditionObs_Catch <- function(HistSim, FisheryData, HistYears,
                               ProjYears, stocks, i, type=c('Landings', 'Discards')) {
  
  type <- match.arg(type, c('Landings', 'Discards'))
  nHistTS <- length(HistYears)
  nProjTS <- length(ProjYears)
  
  ObservedCatch <- slot(FisheryData, type)@Value |>
    ArraySubsetYear(Years=HistYears)
  
  nFleet <- ncol(ObservedCatch)
  
  catchList <- slot(HistSim, type)[stocks]
  
  SimulatedCatch <- purrr::map(catchList, \(catch) {
    List2Array(catch) |>
    AddDimNames(c('Age', 'Fleet', 'Area', 'Year'), HistYears) |>
      apply(c('Year', 'Fleet'), sum)
  }) |> 
    List2Array('Stock') |> 
    apply(c('Year', 'Fleet'), sum)
  
  FleetUnits <- slot(FisheryData,type)@Units
  if (any(FleetUnits=='Number')) {
    # Calculate catch in numbers
    SimulatedCatch_Number <- purrr::map2(catchList, HistSim@OM@Fleet, \(catch, fleet) {
      catch <- apply(List2Array(catch), c(1,2,4), sum) |> # sum over areas
        aperm(c(1,3,2))
      apply(catch/fleet@WeightFleet, 2:3, sum) # sum over ages
    })  |> 
      List2Array('Stock') |> 
      apply(c('Year', 'Fleet'), sum)
  }
  
  for (fl in 1:nFleet) {
    CatchObs <- slot(HistSim@OM@Obs[[i]][[fl]], type)
    CatchObs@Type <- slot(FisheryData,type)@Type[fl]
    Units <- FleetUnits[fl]
    
    if (is.null(CatchObs@Years)) 
      CatchObs@Years <- HistYears
    
    if (Units=='Biomass') {
      SimValue <- SimulatedCatch[,fl]  
    } else {
      SimValue <- SimulatedCatch_Number[,fl]
    }
    
    SimValue[SimValue<0] <- 1E-15
    
    # Bias 
    Bias <- ArraySubsetYear(ObservedCatch[,fl], CatchObs@Years)/ArraySubsetYear(SimValue, CatchObs@Years)
    Bias[Bias<0.001] <- NA
    Bias <- Bias[is.finite(Bias)]
    BiasMean <- mean(Bias, na.rm=TRUE) 
    CatchObs@Bias <- array(BiasMean, dim=nHistTS, dimnames = list(Year=HistYears))
    
    # Error
    CatchErrorHist <- ObservedCatch[,fl]/(SimValue*CatchObs@Bias)
    CatchErrorHist[!is.finite(CatchErrorHist)] <- NA
  
    CatchErrorHistCondition <- CatchErrorHist[as.character(CatchObs@Years)]
    CatchErrorHistCondition[CatchErrorHistCondition<0.001] <- NA
    CatchErrorHistCondition <- CatchErrorHistCondition/mean(CatchErrorHistCondition, na.rm=TRUE)
    CatchObs@CV <- as.numeric(CatchErrorHistCondition) |> sd(na.rm=TRUE) 
    SD <- CatchObs@CV 
    if (!is.finite(SD))
      SD <- 1E-6
    
    CatchErrorProj <- exp(rnorm(nProjTS, -((SD^2)/2), SD))
    CatchObs@Error <-c(CatchErrorHist,CatchErrorProj) |> 
      array(dimnames = list(Year=c(HistYears, ProjYears)))
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
  
  if (all(group.length==1)) {
    Residual.Groups <- list(as.numeric(unlist(Residual.Groups)))
    group.ind <- 1
  } else {
    group.ind <- which(group.length>1) 
    
  }
  
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

GenerateIndexResiduals <- function(Stats, ProjYears) {
  sd <- Stats$SD
  ac <- Stats$AC
  ac[!is.finite(ac)] <- 0
  LastError <- Stats$LastError
  nTS <- length(ProjYears)
  
  if (all(is.na(sd))) {
    cli::cli_abort('Not done yet!!', .internal=TRUE)
  }
  
  mu <- -0.5 * (sd)^2 * (1 - ac)/sqrt(1 - ac^2)
  
  Residuals <- array(rnorm(nTS, mu, sd), nTS, dimnames = list(Year=ProjYears))
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

ConditionObs_Index <- function(HistSim, FisheryData, HistYears, ProjYears, 
                               stocks, i, type=c('CPUE', 'Survey')) {
  
  type <- match.arg(type, c('CPUE', 'Survey'))
  
  nTS <- length(HistYears)
  
  NameIndices <- slot(FisheryData, type)@Name
  ObservedIndices <- slot(FisheryData, type)@Value |>
    ArraySubsetYear(Years=HistYears)
  
  if (EmptyObject(ObservedIndices))
    return(HistSim)
  
  nFleet <- ncol(ObservedIndices)
  
  SimulatedNumberList <- purrr::map(HistSim@Number[stocks], \(stock) {
    stock |> AddDimNames(c('Age', 'Year', 'Area'),HistYears) |>
      apply(c('Age', 'Year'), sum)
  })
  
  for (fl in 1:nFleet) {
    IndexObs <- slot(HistSim@OM@Obs[[i]][[NameIndices[fl]]],type)
    SelectivityAtAge <- slot(FisheryData, type)@Selectivity[[fl]]
    
    SelectivityAtAgeList <- MakeNamedList(StockNames(HistSim@OM)[stocks])
    
    if (is.character(SelectivityAtAge)) {
      if (SelectivityAtAge == 'Biomass') {
        for (st in seq_along(stocks)) {
          SelectivityAtAgeList[[st]] <- matrix(1,nAge(HistSim@OM, stocks[st]), 1) |>
            AddDimNames(c('Age', 'Year'), HistYears)
        }
      } else if (SelectivityAtAge == 'SBiomass') {
        for (st in seq_along(stocks)) {
          SelectivityAtAgeList[[st]] <- HistSim@OM@Stock[[stocks[st]]]@Maturity@MeanAtAge |> ArrayReduceDims()
        }
   
      } else if (SelectivityAtAge == 'Obs') {
        for (st in seq_along(stocks)) {
          SelectivityAtAgeList[[st]] <- IndexObs@Selectivity[[st]]
        }
      }
    } else {
      SelectivityAtAgeList <- purrr::map(HistSim@OM@Fleet[stocks], \(stock) {
        stock@Selectivity@MeanAtAge[,,fl] |>
          ArraySubsetYear(HistYears) |> 
          ArrayReduceDims()
        }) 
    }
  
    ObservedIndex <- ObservedIndices[,fl]
    Units <- slot(FisheryData,type)@Units[fl]
    
    SimNumberSelectedList <- purrr::map2(SimulatedNumberList, SelectivityAtAgeList, ArrayMultiply)
  
    if (Units=='Biomass') {
      WeightAtAgeList <- purrr::map(HistSim@OM@Stock[stocks], \(stock) stock@Weight@MeanAtAge |>
                   ArraySubsetYear(HistYears)) 
      
      SimulatedIndex <- purrr::map2(SimNumberSelectedList, WeightAtAgeList, ArrayMultiply) |>
        purrr::map(apply, 'Year', sum) |>
        List2Array('Stock', 'Year') |>
        AddDimNames(c('Year', 'Stock'), HistYears) |> 
        apply(c('Year'), sum) 

    } else if (Units=='Number') {
      SimulatedIndex <- SimNumberSelectedList |>
        purrr::map(apply, 'Year', sum) |>
        List2Array('Stock', 'Year') |>
        AddDimNames(c('Year', 'Stock'), HistYears) |> 
        apply(c('Year'), sum) 
    } else if (Units=='Recruitment') {
      SimulatedIndex <- SimNumberSelectedList |>
        purrr::map(\(stock) {
          ages <- as.numeric(dimnames(stock)[[1]])
          stock |> ArraySubsetAge(min(ages))
        }) |> 
        purrr::map(apply, 'Year', sum) |>
        List2Array('Stock', 'Year') |>
        AddDimNames(c('Year', 'Stock'), HistYears) |> 
        apply(c('Year'), sum) 
    
    } else {
      cli::cli_abort('Only `Biomass`, `Number` and `Recruitment` currently supported for `Units` in `Data@CPUE` and `Data@Survey`', .internal=TRUE)
    }

    NonNAInd <- which(!is.na(ObservedIndex))
    IndexObs@q <- mean(ObservedIndex[NonNAInd], na.rm=TRUE)/mean(SimulatedIndex[NonNAInd], na.rm=TRUE)
    SimulatedIndex <- SimulatedIndex * IndexObs@q
    
    if (is.null(IndexObs@Years))
      IndexObs@Years <- HistYears[NonNAInd]
    
    TSInd <- match(IndexObs@Years, HistYears)
    
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
    # TODO - keep missing (NA) values for seasonal models in the projections
    ResidualsProjection <- GenerateIndexResiduals(Stats, ProjYears)
    
    # TODO - option to discard indices that are NA for x Years before terminal historical year
    IndexObs@Error <- c(ResidualsHistorical, ResidualsProjection) |> 
      array(dimnames = list(Year=c(HistYears,  ProjYears)))
    
    slot(HistSim@OM@Obs[[i]][[NameIndices[fl]]],type) <- IndexObs
  }
  
  HistSim
  
}

# ---- CAL ----



# ---- CAA ----