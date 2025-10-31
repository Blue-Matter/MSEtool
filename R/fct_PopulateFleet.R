#' @describeIn Populate Populate an [fleet-class()] object
#' @export
PopulateFleet <- function(Fleet, 
                          Ages=NULL,
                          Length=NULL,
                          Weight=NULL,
                          RelativeSize=NULL,
                          seed=NULL,
                          silent=FALSE) {
  
  
  nsim <- nSim(Fleet)
  TimeSteps <- TimeSteps(Fleet)
  HistTimeSteps <- TimeSteps(Fleet, 'Historical')
  ProjTimeSteps <- TimeSteps[!TimeSteps %in% HistTimeSteps]
  
  nAreas <- ncol(RelativeSize)
  
  argList <- list(Ages, Length, Weight, RelativeSize, nsim, TimeSteps, seed)
  if (CheckDigest(Fleet, argList) | EmptyObject(Fleet))
    return(Fleet)
  
  SetSeed(Fleet, seed)
  
  Fleet@Effort <- PopulateEffort(Effort=Fleet@Effort, 
                                 HistTimeSteps, 
                                 nsim, 
                                 seed)
  
  Fleet@Distribution <- PopulateDistribution(Distribution=Fleet@Distribution,
                                             nsim,
                                             TimeSteps,
                                             nAreas) 
  Fleet <- PopulateCatchability(Fleet,
                                RelativeSize,
                                nsim,
                                HistTimeSteps,
                                ProjTimeSteps,
                                seed,
                                silent)
  
  Fleet@DiscardMortality <- PopulateDiscardMortality(DiscardMortality=Fleet@DiscardMortality,
                                                     Ages,
                                                     Length,
                                                     nsim,
                                                     TimeSteps,
                                                     CalcAtLength=TRUE,
                                                     seed=seed,
                                                     silent)
  
  Fleet@Selectivity <- PopulateSelectivity(Selectivity=Fleet@Selectivity,
                                           Ages,
                                           Length,
                                           Weight,
                                           nsim,
                                           TimeSteps,
                                           CalcAtLength=TRUE,
                                           seed,
                                           silent=silent)
  
  Fleet@Retention <- PopulateRetention(Fleet@Retention,
                                       Ages,
                                       Length,
                                       Weight,
                                       nsim,
                                       TimeSteps,
                                       CalcAtLength=TRUE,
                                       seed,
                                       silent=silent)
  
  

  Fleet@Closure <- PopulateClosure(Closure=Fleet@Closure,
                                   nAreas,
                                   nsim,
                                   TimeSteps,
                                   silent)
  

    
    
  
  if (all(is.na(Fleet@WeightFleet))) {
    Fleet@WeightFleet <- Weight@MeanAtAge
  } else {
    Fleet@WeightFleet
    # stop('need to add populate for Fleet@WeightFleet')
  }
  
  
  SetDigest(Fleet, argList)
}


PopulateEffort <- function(Effort, TimeSteps, nsim=NULL, seed=NULL) {
  argList <- list(TimeSteps, nsim, seed)
  if (CheckDigest(Effort, argList))
    return(Effort)
  
  SetSeed(Effort, seed)
  
  if (inherits(Effort, 'data.frame')) {
    Effort <- GenerateHistoricalEffort(Effort, nsim, TimeSteps)
  } else if (inherits(Effort, 'array')) {
    dd <- dim(Effort)
    # TODO - check array dimensions
    dimnames(Effort) <- list(Sim=1:nrow(Effort),
                             TimeStep=TimeSteps)
    
  } else {
    cli::cli_abort(c('x'='`Effort` must be {.cls array/matrix}  or {.cls data.frame} ',
                     'i'='Currently class {.cls {class(Effort)}}'))  
  }
  SetDigest(Effort, argList)
}


PopulateDistribution <- function(Distribution, 
                                 nsim=NULL,
                                 TimeSteps=NULL,
                                 nAreas=NULL) {
  
  argList <- list(nsim, TimeSteps, nAreas)
  
  if (CheckDigest(Distribution, argList))
    return(Distribution)
  
  CheckClass(Distribution, c('array', 'matrix'))
  
  if (EmptyObject(Distribution)) {
    Distribution <- array(tiny, dim=c(1, 1, nAreas),
                          dimnames = list(
                            Sim=1,
                            TimeStep=TimeSteps[1],
                            Area=1:nAreas
                          ))
    
  } else {
    CheckClass(Distribution, c('array', 'matrix'))
    dd <- dim(Distribution)
    # TODO check dimensions
    # TODO add dimnames if needed
    
    
  }
  
  SetDigest(Distribution, argList)
}


# PopulateFishingMortality <- function(FishingMortality,
#                                      nsim=NULL,
#                                      TimeSteps=NULL,
#                                      seed=NULL,
#                                      silent=FALSE) {
#   
#   TimeSteps <- TimeStepAttributes(FishingMortality, TimeSteps)
#   argList <- list(nsim, TimeSteps, seed)
#   
#   if (CheckDigest(FishingMortality, argList) | EmptyObject(FishingMortality))
#     return(FishingMortality)
#   
#   SetSeed(FishingMortality, seed)
#  
#   FishingMortality@ApicalF <- AddSimDimension(FishingMortality@ApicalF,
#                                               c('Sim', 'TimeStep'), 
#                                               TimeSteps=TimeSteps)
#   
#   FishingMortality@DeadAtAge <- AddSimDimension(FishingMortality@DeadAtAge,
#                                                 TimeSteps=TimeSteps)
#   FishingMortality@RetainAtAge <- AddSimDimension(FishingMortality@RetainAtAge,
#                                                   TimeSteps=TimeSteps)
#   
#   if (EmptyObject(FishingMortality@ApicalF)) # calculate from `DeadAtAge`
#     FishingMortality@ApicalF <- apply(FishingMortality@DeadAtAge, c(1,3), max)
#   
#   SetDigest(FishingMortality, argList)
#   
# }

PopulateCatchability <- function(Fleet,
                                 RelativeSize,
                                 nsim=NULL,
                                 HistTimeSteps=NULL,
                                 ProjTimeSteps=NULL,
                                 seed=NULL,
                                 silent=FALSE) {
  
  Catchability <- Fleet@Catchability
  pYears <- length(ProjTimeSteps)
  
  TimeSteps <- c(HistTimeSteps, ProjTimeSteps)
  
  if (all(is.na(Catchability)) || all(Catchability <= tiny)) {
    Catchability <- array(1, dim=c(nsim, length(TimeSteps)),
                          dimnames = list(
                            Sim=1:nsim,
                            TimeStep=TimeSteps
                          ))
  } else {
    dd <- dim(Catchability)
    if (dd[1] != nsim) {
      if (dd[1]!=1)
        cli::cli_abort(c('x'="Incorrect number of rows in `Catchability` matrix.",
                         'i'="Must have either 1 row or `nsim` ({.val {nsim}}) rows. "
        ))
    }
    if (is.null( dimnames(Catchability)))
      dimnames(Catchability) <- list(Sim=1:nrow(Catchability),
                                     TimeSteps=TimeSteps[1:ncol(Catchability)])
  }
  
  if (!is.null(Fleet@qInc)) {
    qIncs <- StructurePars_(Fleet@qInc, nsim, TimeSteps)[,1]
    qIncs <- sapply(qIncs, function(x)
      (1+x/100)^(1:pYears)
    ) |> t()
    
    dimnames(qIncs) <- list(Sim=1:nsim,
                            TimeStep=ProjTimeSteps)
    
    qfuture <- ArrayMultiply(SubsetTimeStep(Catchability, ProjTimeSteps), qIncs)
    ArrayFill(Catchability) <- qfuture
    Fleet@qInc <- qIncs
  }
  
  
  if (!is.null(Fleet@qCV)) {
    qCVs <- StructurePars_(Fleet@qCV, nsim, TimeSteps)[,1]
    qmu <- -0.5 * qCVs^2
    qvar <- array(exp(rnorm(pYears * nsim, rep(qmu, pYears), rep(qCVs, pYears))), c(nsim, pYears),
                  dimnames = list(
                    Sim=1:nsim,
                    TimeStep=ProjTimeSteps
                  ))
    
    qfuture <- ArrayMultiply(SubsetTimeStep(Catchability, ProjTimeSteps), qvar)
    Fleet@qCV <- qCVs
    ArrayFill(Catchability) <- qvar
  }
  Fleet@Catchability <- Catchability
  
  if (EmptyObject(Fleet@qArea)) {
    
    Fleet@qArea <- ArrayDivide(array1=AddDimension(Catchability,'Area'),
                               array2=AddDimension(RelativeSize, 'TimeStep', val=TimeSteps[1]) |>
                                 aperm(c('Sim', 'TimeStep', 'Area'))
    )
  } else {
    dd <- dim(Fleet@qArea)
    # TODO: check dimensions
    # TODO: add dimnames if neccessary
    
  }
  Fleet
}

PopulateDiscardMortality <- function(DiscardMortality,
                                     Ages=NULL,
                                     Length=NULL,
                                     nsim=NULL,
                                     TimeSteps=NULL,
                                     CalcAtLength=FALSE,
                                     seed=NULL,
                                     silent=FALSE) {
  TimeSteps <- TimeStepAttributes(DiscardMortality, TimeSteps)
  argList <- list(Ages, Length, nsim, TimeSteps, CalcAtLength, seed)
  
  if (CheckDigest(DiscardMortality, argList) | EmptyObject(DiscardMortality))
    return(DiscardMortality)
  
  SetSeed(DiscardMortality, seed)
  
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length,
                                             Ages, nsim, TimeSteps, seed, silent)
  if (CalcAtLength)
    DiscardMortality <- MeanAtAge2MeanAtLength(DiscardMortality, Length, Ages,
                                               nsim, TimeSteps, seed, silent)
  
  DiscardMortality <- AddMeanAtAgeAttributes(DiscardMortality, TimeSteps, Ages)
  
  # Dimnames for at length
  if (!is.null(DiscardMortality@MeanAtLength)) {
    dd <- dim(DiscardMortality@MeanAtLength)
    dnames <- names(dimnames(DiscardMortality@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(DiscardMortality@MeanAtLength) <- list(Sim=1:dd[1],
                                                      Class=DiscardMortality@Classes,
                                                      TimeStep=TimeSteps[1:dd[3]])
  }
  
  SetDigest(DiscardMortality, argList)
}

PopulateSelectivity <- function(Selectivity,
                                Ages=NULL,
                                Length=NULL,
                                Weight=NULL,
                                nsim=NULL,
                                TimeSteps=NULL,
                                CalcAtLength=TRUE,
                                seed=NULL,
                                silent=FALSE,
                                CheckMaxValue=TRUE) {
  
  TimeSteps <- TimeStepAttributes(Selectivity, TimeSteps)
  argList <- list(Ages, Length, Weight, TimeSteps, nsim, CalcAtLength, seed)
  
  if (CheckDigest(Selectivity, argList))
    return(Selectivity)
  
  SetSeed(Selectivity, seed)
  
  Selectivity@Pars <- StructurePars(Pars=Selectivity@Pars, nsim, TimeSteps)
  Selectivity@Model <- FindModel(Selectivity)
  ModelClass <- getModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    
    if (grepl('at-Length',getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtLength(Selectivity, 
                                          Length, 
                                          TimeSteps,
                                          Ages, 
                                          nsim,
                                          seed, 
                                          silent)
    } else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtWeight(Selectivity, Weight, TimeSteps, Ages, nsim, seed, silent)
      
    } else {
      Selectivity <- PopulateMeanAtAge(Selectivity, Ages, TimeSteps)
    }
  } 
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, Ages, nsim,
                                        TimeSteps, seed, silent)
  
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight, Ages, nsim,
                                        TimeSteps, seed, silent) 
  
  if (CalcAtLength)
    Selectivity <- MeanAtAge2MeanAtLength(Selectivity, Length, Ages, nsim, TimeSteps, seed, silent)
  
  if (is.null(Selectivity@MeanAtAge)) {
    # chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    # if (!chk@populated)
    #   FishingMortality <- PopulateFishingMortality(FishingMortality,
    #                                nsim,
    #                                TimeSteps,
    #                                seed,
    #                                silent)
    # 
    # if (!EmptyObject(FishingMortality@DeadAtAge)) {
    #   Selectivity@MeanAtAge <- FishingMortality2Selectivity(FishingMortality,
    #                                                         DiscardMortality,
    #                                                         Ages,
    #                                                         TimeSteps,
    #                                                         Length)
    # } else {
    cli::cli_abort('`Selectivity` requires either `Pars` or `MeanAtAge`')
    # }
  }
  
  # Check Selectivity has a max value of one across age classes
  if(CheckMaxValue) 
    Selectivity@MeanAtAge <- CheckSelectivityMaximum(Selectivity@MeanAtAge)
  
  # Dimnames for at length
  if (!is.null(Selectivity@MeanAtLength)) {
    dd <- dim(Selectivity@MeanAtLength)
    dnames <- names(dimnames(Selectivity@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(Selectivity@MeanAtLength) <- list(Sim=1:dd[1],
                                                 Class=Selectivity@Classes,
                                                 TimeStep=TimeSteps[1:dd[3]])
  }
  
  SetDigest(Selectivity, argList)
}


PopulateRetention <- function(Retention, 
                              Ages=NULL,
                              Length=NULL,
                              Weight=NULL,
                              nsim=NULL,
                              TimeSteps=NULL,
                              CalcAtLength=FALSE,
                              seed=NULL,
                              silent=FALSE) {
  
  TimeSteps <- TimeStepAttributes(Retention, TimeSteps)
  argList <- list(Ages, Length, 
                  TimeSteps, nsim, CalcAtLength, seed)
  
  
  if (CheckDigest(Retention, argList))
    return(Retention)
  
  if (EmptyObject(Retention)) {
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(TimeSteps=TimeSteps)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'TimeStep'), TimeSteps=TimeSteps)
    
    return(SetDigest(Retention, argList))
  }
  
  SetSeed(Retention, seed)
  
  
  Retention@Pars <- StructurePars(Pars=Retention@Pars, nsim, TimeSteps)
  
  ParsZero <- all((lapply(lapply(Retention@Pars, `==`, 0), prod) |> 
                     unlist())==1)
  
  if (!ParsZero) {
    Retention@Model <- FindModel(Retention)
    ModelClass <- getModelClass(Retention@Model)
    
    if (!is.null(ModelClass)) {
      if (grepl('at-Length',getModelClass(Retention@Model))) {
        Retention <- PopulateMeanAtLength(Retention, Length, TimeSteps,
                                          Ages, nsim,
                                          seed, silent)
      } else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
        Retention <- PopulateMeanAtWeight(Retention, Weight, TimeSteps, Ages, nsim, seed, silent)
        
      } else {
        Retention <- PopulateMeanAtAge(Retention, Ages, TimeSteps)
      }
    } 
    
  }
  
  if (ParsZero & is.null(Retention@MeanAtAge) & is.null(Retention@MeanAtLength)) {
    
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(TimeSteps=TimeSteps)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'TimeStep'),TimeSteps=TimeSteps)
    
    return(SetDigest(Retention, argList))
  } 
  
  Retention <- MeanAtLength2MeanAtAge(Retention, Length, Ages,
                                      nsim, TimeSteps, seed, silent)
  
  Retention <- MeanAtWeight2MeanAtAge(Retention, Weight, Ages, nsim,
                                      TimeSteps, seed, silent) 
  
  if (CalcAtLength)
    Retention <- MeanAtAge2MeanAtLength(Retention, Length, Ages, 
                                        nsim, TimeSteps, seed, silent)
  
  if (is.null(Retention@MeanAtAge)) {
    # chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    # if (!chk@populated)
    #   FishingMortality <- PopulateFishingMortality(FishingMortality,
    #                                nsim,
    #                                TimeSteps,
    #                                seed,
    #                                silent)
    # 
    # if (!EmptyObject(FishingMortality@DeadAtAge)) {
    #   Retention@MeanAtAge <- FishingMortality2Retention(FishingMortality,
    #                                                     DiscardMortality,
    #                                                     Ages,
    #                                                     TimeSteps,
    #                                                     Length)
    # } else {
    cli::cli_abort('`Retention` requires either `Pars` or `MeanAtAge`')
    # }
  }
  
  
  # Dimnames for at length
  if (!is.null(Retention@MeanAtLength)) {
    dd <- dim(Retention@MeanAtLength)
    dnames <- names(dimnames(Retention@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(Retention@MeanAtLength) <- list(Sim=1:dd[1],
                                               Class=Retention@Classes,
                                               TimeStep=TimeSteps[1:dd[3]])
  }
  
  # Retention <- AddMeanAtAgeAttributes(Retention, TimeSteps, Ages)
  SetDigest(Retention, argList)
  
}



PopulateClosure <- function(Closure, nAreas, nsim, TimeSteps, silent) {
  argList <- list(nAreas, TimeSteps, nsim)
  
  if (EmptyObject(Closure)) {
    Closure <- array(1, dim=c(1,1, nAreas), 
                     dimnames =list(
                       Sim=1,
                       TimeStep=TimeSteps[1],
                       Area=1:nAreas)
    )
  } else {
    dd <- dim(Closure)
    # TODO: check dimensions
    # TODO: add dimnames if neccessary
  }
  
  SetDigest(Closure, argList)
}





