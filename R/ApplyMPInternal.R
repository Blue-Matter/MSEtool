


UpdateTAC <- function(ProjSim, MPAdvice, TSIndex) {
  
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (length(MPAdvice@TAC)<1)
    return(ProjSim)
  

  # calculate effort for a given TAC ... 
  # TODO doesn't account for spatial closures
  # TODO calculate effortArea here instead - calculate distribution of F/Effort by area
  
  # TODO applies for all stocks - TODO multistock with stock specific TAC
  
  nStock <- length(ProjSim@Number)
  
  NumberAtAge <- purrr::map(ProjSim@Number, \(stock) stock[,TSIndex,, drop=FALSE] |> abind::adrop(2) |> apply(1, sum)) # summed over areas
  SelectivityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  RetentionAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  DiscardMortalityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@DiscardMortality@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  FleetWeightAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@WeightFleet[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  NaturalMortalityAtAge <- purrr::map(ProjSim@OM@Stock, \(stock) stock@NaturalMortality@MeanAtAge[,TSIndex])
  
  RelVuln <- purrr::map2(NumberAtAge, SelectivityAtAge, \(number, select) {
    sum(number * select)
    # sum(ArrayMultiply(number, select))
  }) |> unlist()
  
  StockAllocation <- RelVuln/sum(RelVuln)
  
  for (st in 1:nStock) {
    TotalRemovalsFleet <- MPAdvice@TAC *StockAllocation[st] * ProjSim@OM@Allocation[[st]] |> as.numeric()


    SolvedF <- SolveForFishingMortality(NumberAtAge[[st]],
                                        TotalRemovalsFleet,
                                        SelectivityAtAge[[st]],  
                                        RetentionAtAge[[st]],  
                                        DiscardMortalityAtAge[[st]],  
                                        FleetWeightAtAge[[st]],
                                        NaturalMortalityAtAge[[st]]) 
  
    FInteract <- t(SolvedF$ApicalFInteract)
    RequiredEffort <- FInteract / ProjSim@OM@Fleet[[st]]@Effort@Catchability[TSIndex,] 
    RequiredEffort[RequiredEffort<1E-5] <- 1E-5 
    
    ProjSim@Effort[st,TSIndex,] <- RequiredEffort
  }
  
  ProjSim
}

UpdateEffort <- function(ProjSim, MPAdvice, MPAdvicePrevious, TimeStepsAll, TimeStepsHist, TSIndex) {
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (!length(MPAdvice@Effort))
    return(ProjSim)
  
  if (!is.null(MPAdvicePrevious)) {
    if (IdenticalS4(MPAdvice@Effort, MPAdvicePrevious@Effort))
      return(ProjSim)  
  }
  
  
  TimeStep <- TimeStepsAll[TSIndex]
  TimeStepProj <- TimeStepsAll[TSIndex:length(TimeStepsAll)]
  nprojTS <- length(TimeStepsAll)
  projInd <- TSIndex:nprojTS
  
  if (length(MPAdvice@Effort)<1)
    MPAdvice@Effort <- 1
  
  if (length(MPAdvice@Effort)>0) {
    LastHistIndex <- length(TimeStepsHist)
    futureEffort <- ProjSim@Effort[st,LastHistIndex,fl] * MPAdvice@Effort
    ProjSim@Effort[st,projInd,fl] <- futureEffort
  } 
  ProjSim
}

UpdateSpatial <- function(ProjSim, MPAdvice, MPAdvicePrevious, TSIndex) {
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (!is.null(MPAdvicePrevious)) {
    if (IdenticalS4(MPAdvice@Spatial, MPAdvicePrevious@Spatial))
      return(ProjSim)  
  }
  
  if (length(MPAdvice@Spatial)>0) {
    dd <- dim(ProjSim@OM@Fleet[[st]]@Distribution@Closure)
    nprojTS <- dd[1]
    projInd <- TSIndex:nprojTS
    nArea <- dd[3]
    
    if (length(MPAdvice@Spatial) != nArea) {
      cli::cli_warn(c("x"='`length(MPAdvice@Spatial)` ({.val {(length(MPAdvice@Spatial))}}) != `nArea` ({.val {nArea}})',
                      'i'="Ignoring Spatial Management Advice")
      )
      return(ProjSim)
    }
    ProjSim@OM@Fleet[[st]]@Distribution@Closure[projInd,fl,] <- matrix(MPAdvice@Spatial, length(projInd), nArea, byrow=TRUE)
  } 
  
  ProjSim
}

UpdateRetention <- function(ProjSim, MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) {
  UpdateSelectivity(ProjSim, MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex, 'Retention')
}

UpdateDiscardMortality <- function(ProjSim, MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) {
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (EmptyObject(MPAdvice@DiscardMortality))
    return(ProjSim)
  
  
  
  if (!is.null(MPAdvicePrevious)) {
    if (IdenticalS4(MPAdvice@DiscardMortality, MPAdvicePrevious@DiscardMortality))
      return(ProjSim)  
  }
  
  
  TimeStep <- TimeStepsAll[TSIndex]
  TimeStepProj <- TimeStepsAll[TSIndex:length(TimeStepsAll)]
  nprojTS <- length(TimeStepsAll)
  projInd <- TSIndex:nprojTS
  
  if (length(MPAdvice@DiscardMortality@MeanAtAge)>0) {
    stop("Discard mortality TODO")
    
    # check its length age@Classes
    # calc DiscMatLength
    ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtAge[,projInd,fl]
  }
  # repeat for MeanAtLength
  ProjSim
  
}

UpdateSelectivity <- function(ProjSim, MPAdvice, MPAdvicePrevious, 
                              TimeStepsAll, TSIndex,type='Selectivity') {
  
  
  # *************************** # 
  st <- 1
  fl <- 1
  # *************************** #
  
  
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (!is.null(MPAdvicePrevious)) {
    if (IdenticalS4(slot(MPAdvice,type), slot(MPAdvicePrevious,type)))
      return(ProjSim)  
  }
  
  Selectivity <- slot(MPAdvice,type)
  if (EmptyObject(Selectivity))
    return(ProjSim)
  
  # TODO checks
  
  TimeStep <- TimeStepsAll[TSIndex]
  TimeStepProj <- TimeStepsAll[TSIndex:length(TimeStepsAll)]
  nprojTS <- length(TimeStepsAll)
  projInd <- TSIndex:nprojTS
  
  Selectivity@Model <- FindModel(Selectivity)
  
  ModelClass <- getModelClass(Selectivity@Model)
  
  Length <- ProjSim@OM@Stock[[st]]@Length
  Length@ASK <- Length@ASK[,,TSIndex, drop=FALSE]
  Weight <- ProjSim@OM@Stock[[st]]@Weight
  Ages <- ProjSim@OM@Stock[[st]]@Ages
  if (grepl('at-Length',ModelClass)) {
    Selectivity@Classes <- Length@Classes
    LengthClasses <- ProjSim@OM@Stock[[st]]@Length@Classes
    Selectivity@MeanAtLength <- GenerateMeanatLength(Model=Selectivity@Model,
                                                     Pars=Selectivity@Pars,
                                                     Length=Length@Classes)
    dimnames(Selectivity@MeanAtLength) <- list(Sim=1,
                                               Class=LengthClasses,
                                               TimeStep=TimeStep)
    
    Selectivity@MeanAtLength <- Selectivity@MeanAtLength |> ExpandTimeSteps(TimeSteps=TimeStepProj)
    
  }  else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
    Selectivity <- PopulateMeanAtWeight(Selectivity, Weight, TimeSteps=TimeStep, Ages, nsim, seed, silent)
    Selectivity@MeanAtWeight <- Selectivity@MeanAtWeight |> ExpandTimeSteps(TimeSteps=TimeStepProj)
    
  } else {
    Selectivity <- PopulateMeanAtAge(Selectivity, Ages, TimeSteps=TimeStep)
    Selectivity@MeanAtAge <- Selectivity@MeanAtAge |> ExpandTimeSteps(TimeSteps=TimeStepProj)
  }
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, Ages, nsim=1, TimeSteps=TimeStep)
  
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight, Ages, nsim,
                                        TimeSteps=TimeStep, seed, silent) 
  
  dimnames(Selectivity@MeanAtAge) <- list(Sim=1,
                                          Age=ProjSim@OM@Stock[[st]]@Ages@Classes,
                                          TimeStep=TimeStepProj)
  
  
  
  if (type=="Selectivity") {
    ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtLength[,projInd,fl] <- Selectivity@MeanAtLength[1,,]
    ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtWeight[,projInd,fl] <- Selectivity@MeanAtWeight[1,,]
    ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,projInd,fl] <- Selectivity@MeanAtAge[1,,]
  } else if (type=="Retention") {
    # TODO fix initialization of these
    # ProjSim@OM@Fleet[[st]]@Retention@MeanAtLength[,projInd,fl] <- Selectivity@MeanAtLength[1,,]
    # ProjSim@OM@Fleet[[st]]@Retention@MeanAtWeight[,projInd,fl] <- Selectivity@MeanAtWeight[1,,]
    ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,projInd,fl] <- Selectivity@MeanAtAge[1,,]
  } else{
    stop("type must be `Selectivity` or `Retention`")
  }
  
  ProjSim
}


ApplyMPInternal <- function(ProjSim, MP, TimeStep, TimeStepsHist, TimeStepsProj, ManagementTimeSteps) {
  
  ###############################
  st <- 1 
  ##############################
  
  if (!is.null(ProjSim@Misc$MPAdvice) & length(ProjSim@Misc$MPAdvice)>0) {
    # Has MPAdvice Changed from last time 
    MPAdvicePrevious <- ProjSim@Misc$MPAdvice[[length(ProjSim@Misc$MPAdvice)]] 
  } else {
    MPAdvicePrevious <- NULL
  }
  
  if (TimeStep %in% ManagementTimeSteps) {
    # Apply MP to Data
    MPFunction <- get(MP)
    
    # TODO add tryCatch
    # TODO - by stock?
    MPAdvice <- MPFunction(ProjSim@Data[[st]])
    ProjSim@Data[[st]]@Misc <- MPAdvice@Misc
    
    if (is.null(ProjSim@Misc$MPAdvice))
      ProjSim@Misc$MPAdvice <- list()
    ProjSim@Misc$MPAdvice[[as.character(TimeStep)]] <- MPAdvice
  } else {
    MPAdvice <- NULL
  }
  
  TimeStepsAll <- c(TimeStepsHist, TimeStepsProj) 
  TSIndex <- match(TimeStep, TimeStepsAll)
  
  ProjSim <- ProjSim |> 
    UpdateSpatial(MPAdvice, MPAdvicePrevious, TSIndex) |>
    UpdateSelectivity(MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) |>
    UpdateRetention(MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) |>
    UpdateDiscardMortality(MPAdvice, MPAdvicePrevious, TimeStepsAll, TSIndex) |>
    UpdateTAC(MPAdvice, TSIndex) |>
    UpdateEffort(MPAdvice, MPAdvicePrevious, TimeStepsAll, TimeStepsHist, TSIndex) 
  
  

  
  
  # TODO
  # - apply BioEconomic to Effort
  
  
  ProjSim
}