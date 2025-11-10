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
  Years <- Years(Fleet)
  HistYears <- Years(Fleet, 'Historical')
  ProjYears <- Years[!Years %in% HistYears]
  
  nAreas <- ncol(RelativeSize)
  
  argList <- list(Ages, Length, Weight, RelativeSize, nsim, Years, seed)
  if (CheckDigest(Fleet, argList) | EmptyObject(Fleet))
    return(Fleet)
  
  SetSeed(Fleet, seed)
  
  Fleet@Effort <- PopulateEffort(Effort=Fleet@Effort, 
                                 HistYears, 
                                 nsim, 
                                 seed)
  
  Fleet@Distribution <- PopulateDistribution(Distribution=Fleet@Distribution,
                                             nsim,
                                             Years,
                                             nAreas) 
  Fleet <- PopulateCatchability(Fleet,
                                RelativeSize,
                                nsim,
                                HistYears,
                                ProjYears,
                                seed,
                                silent)
  
  Fleet@DiscardMortality <- PopulateDiscardMortality(DiscardMortality=Fleet@DiscardMortality,
                                                     Ages,
                                                     Length,
                                                     nsim,
                                                     Years,
                                                     CalcAtLength=FALSE,
                                                     seed=seed,
                                                     silent)
  
  Fleet@Selectivity <- PopulateSelectivity(Selectivity=Fleet@Selectivity,
                                           Ages,
                                           Length,
                                           Weight,
                                           nsim,
                                           Years,
                                           CalcAtLength=FALSE,
                                           seed,
                                           silent=silent)
  
  Fleet@Retention <- PopulateRetention(Fleet@Retention,
                                       Ages,
                                       Length,
                                       Weight,
                                       nsim,
                                       Years,
                                       CalcAtLength=FALSE,
                                       seed,
                                       silent=silent)
  
  Fleet@Closure <- PopulateClosure(Closure=Fleet@Closure,
                                   nAreas,
                                   nsim,
                                   Years,
                                   silent)
  

  if (all(is.na(Fleet@WeightFleet))) {
    Fleet@WeightFleet <- Weight@MeanAtAge
  } else {
    Fleet@WeightFleet
    # stop('need to add populate for Fleet@WeightFleet')
  }
  
  
  SetDigest(Fleet, argList)
}


PopulateEffort <- function(Effort, Years, nsim=NULL, seed=NULL) {
  argList <- list(Years, nsim, seed)
  if (CheckDigest(Effort, argList))
    return(Effort)
  
  SetSeed(Effort, seed)
  
  if (inherits(Effort, 'data.frame')) {
    Effort <- GenerateHistoricalEffort(Effort, nsim, Years)
  } else if (inherits(Effort, 'array')) {
    dd <- dim(Effort)
    # TODO - check array dimensions
    dimnames(Effort) <- list(Sim=1:nrow(Effort),
                             Year=Years)
    
  } else {
    cli::cli_abort(c('x'='`Effort` must be {.cls array/matrix}  or {.cls data.frame} ',
                     'i'='Currently class {.cls {class(Effort)}}'))  
  }
  SetDigest(Effort, argList)
}


PopulateDistribution <- function(Distribution, 
                                 nsim=NULL,
                                 Years=NULL,
                                 nAreas=NULL) {
  
  argList <- list(nsim, Years, nAreas)
  
  if (CheckDigest(Distribution, argList))
    return(Distribution)
  
  CheckClass(Distribution, c('array', 'matrix'))
  
  if (EmptyObject(Distribution)) {
    Distribution <- array(tiny, dim=c(1, 1, nAreas),
                          dimnames = list(
                            Sim=1,
                            Year=Years[1],
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
#                                      Years=NULL,
#                                      seed=NULL,
#                                      silent=FALSE) {
#   
#   Years <- YearAttributes(FishingMortality, Years)
#   argList <- list(nsim, Years, seed)
#   
#   if (CheckDigest(FishingMortality, argList) | EmptyObject(FishingMortality))
#     return(FishingMortality)
#   
#   SetSeed(FishingMortality, seed)
#  
#   FishingMortality@ApicalF <- AddSimDimension(FishingMortality@ApicalF,
#                                               c('Sim', 'Year'), 
#                                               Years=Years)
#   
#   FishingMortality@DeadAtAge <- AddSimDimension(FishingMortality@DeadAtAge,
#                                                 Years=Years)
#   FishingMortality@RetainAtAge <- AddSimDimension(FishingMortality@RetainAtAge,
#                                                   Years=Years)
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
                                 HistYears=NULL,
                                 ProjYears=NULL,
                                 seed=NULL,
                                 silent=FALSE) {
  
  Catchability <- Fleet@Catchability
  pYears <- length(ProjYears)
  
  Years <- c(HistYears, ProjYears)
  
  if (all(is.na(Catchability)) || all(Catchability <= tiny)) {
    Catchability <- array(1, dim=c(nsim, length(Years)),
                          dimnames = list(
                            Sim=1:nsim,
                            Year=Years
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
                                     Year=Years[1:ncol(Catchability)])
    
    Catchability <- ExpandYears(Catchability, HistYears) 
  }
  
  if (!is.null(Fleet@qInc)) {
    qIncs <- StructurePars_(Fleet@qInc, nsim, Years)[,1]
    qIncs <- sapply(qIncs, function(x)
      (1+x/100)^(1:pYears)
    ) |> t()
    
    dimnames(qIncs) <- list(Sim=1:nsim,
                            Year=ProjYears)
    
    qfuture <- ArrayMultiply(SubsetYear(Catchability, ProjYears), qIncs)
    ArrayFill(Catchability) <- qfuture
    Fleet@qInc <- qIncs
  }
  
  
  if (!is.null(Fleet@qCV)) {
    qCVs <- StructurePars_(Fleet@qCV, nsim, Years)[,1]
    Fleet@qCV <- qCVs
    
    qmu <- -0.5 * qCVs^2
    qvar <- array(exp(rnorm(pYears * nsim, rep(qmu, pYears), rep(qCVs, pYears))), c(nsim, pYears),
                  dimnames = list(
                    Sim=1:nsim,
                    Year=ProjYears
                  ))
    
    qfuture <- ArrayMultiply(SubsetYear(Catchability, ProjYears), qvar)
    if (!all(qfuture==1)) 
      ArrayFill(Catchability) <- qfuture
  }
  Fleet@Catchability <- Catchability
  
  if (EmptyObject(Fleet@qArea)) {
    
    Fleet@qArea <- ArrayDivide(array1=AddDimension(Catchability,'Area'),
                               array2=AddDimension(RelativeSize, 'Year', val=Years[1]) |>
                                 aperm(c('Sim', 'Year', 'Area'))
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
                                     Years=NULL,
                                     CalcAtLength=FALSE,
                                     seed=NULL,
                                     silent=FALSE) {
  # Years <- YearAttributes(DiscardMortality, Years)
  argList <- list(Ages, Length, nsim, Years, CalcAtLength, seed)
  
  if (CheckDigest(DiscardMortality, argList) | EmptyObject(DiscardMortality))
    return(DiscardMortality)
  
  SetSeed(DiscardMortality, seed)
  
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length,
                                             Ages, nsim, Years, seed, silent)
  if (CalcAtLength)
    DiscardMortality <- MeanAtAge2MeanAtLength(DiscardMortality, Length, Ages,
                                               nsim, Years, seed, silent)
  
  # DiscardMortality <- AddMeanAtAgeAttributes(DiscardMortality, Years, Ages)
  
  # Dimnames for at length
  if (!is.null(DiscardMortality@MeanAtLength)) {
    dd <- dim(DiscardMortality@MeanAtLength)
    dnames <- names(dimnames(DiscardMortality@MeanAtLength))
    if (is.null(dnames)) 
      dimnames(DiscardMortality@MeanAtLength) <- list(Sim=1:dd[1],
                                                      Class=DiscardMortality@Classes,
                                                      Year=Years[1:dd[3]])
  }
  
  SetDigest(DiscardMortality, argList)
}

PopulateSelectivity <- function(Selectivity,
                                Ages=NULL,
                                Length=NULL,
                                Weight=NULL,
                                nsim=NULL,
                                Years=NULL,
                                CalcAtLength=TRUE,
                                seed=NULL,
                                silent=FALSE,
                                CheckMaxValue=TRUE) {
  
  # Years <- YearAttributes(Selectivity, Years)
  argList <- list(Ages, Length, Weight, Years, nsim, CalcAtLength, seed)
  
  if (CheckDigest(Selectivity, argList))
    return(Selectivity)
  
  SetSeed(Selectivity, seed)
  
  Selectivity@Pars <- StructurePars(Pars=Selectivity@Pars, nsim, Years)
  Selectivity@Model <- FindModel(Selectivity)
  ModelClass <- getModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    
    if (grepl('at-Length',getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtLength(Selectivity, 
                                          Length, 
                                          Years,
                                          Ages, 
                                          nsim,
                                          seed, 
                                          silent)
    } else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtWeight(Selectivity, Weight, Years, Ages, nsim, seed, silent)
      
    } else {
      Selectivity <- PopulateMeanAtAge(Selectivity, Ages, Years)
    }
  } 
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, Ages, nsim,
                                        Years, seed, silent)
  
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight, Ages, nsim,
                                        Years, seed, silent) 
  
  if (CalcAtLength)
    Selectivity <- MeanAtAge2MeanAtLength(Selectivity, Length, Ages, nsim, Years, seed, silent)
  
  if (is.null(Selectivity@MeanAtAge)) {
    cli::cli_abort('`Selectivity` requires either `Pars` or `MeanAtAge`')
  }
  
  # Check Selectivity has a max value of one across age classes
  if(CheckMaxValue) 
    Selectivity@MeanAtAge <- CheckSelectivityMaximum(Selectivity@MeanAtAge)
  
  # Dimnames for at length
  if (!is.null(Selectivity@MeanAtLength)) {
    if (is.null(names(dimnames(Selectivity@MeanAtLength)))) {
      dd <- dim(Selectivity@MeanAtLength)
      dimnames(Selectivity@MeanAtLength) <- list(Sim=1:dd[1],
                                                 Class=Selectivity@Classes,
                                                 Year=Years[1:dd[3]])
    }
  }
  
  if (!is.null(Selectivity@MeanAtAge)) {
    if (is.null(names(dimnames(Selectivity@MeanAtAge)))) {
      dd <- dim(Selectivity@MeanAtAge)
      dimnames(Selectivity@MeanAtAge) <- list(Sim=1:dd[1],
                                              Age=Ages@Classes[1:dd[2]],
                                              Year=Years[1:dd[3]])
    }
  }
  
  SetDigest(Selectivity, argList)
}


PopulateRetention <- function(Retention, 
                              Ages=NULL,
                              Length=NULL,
                              Weight=NULL,
                              nsim=NULL,
                              Years=NULL,
                              CalcAtLength=FALSE,
                              seed=NULL,
                              silent=FALSE) {
  
  # Years <- YearAttributes(Retention, Years)
  argList <- list(Ages, Length, 
                  Years, nsim, CalcAtLength, seed)
  
  
  if (CheckDigest(Retention, argList))
    return(Retention)
  
  if (EmptyObject(Retention)) {
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(Years=Years)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'Year'), Years=Years)
    
    return(SetDigest(Retention, argList))
  }
  
  SetSeed(Retention, seed)
  
  
  Retention@Pars <- StructurePars(Pars=Retention@Pars, nsim, Years)
  
  ParsZero <- all((lapply(lapply(Retention@Pars, `==`, 0), prod) |> 
                     unlist())==1)
  
  if (!ParsZero) {
    Retention@Model <- FindModel(Retention)
    ModelClass <- getModelClass(Retention@Model)
    
    if (!is.null(ModelClass)) {
      if (grepl('at-Length',getModelClass(Retention@Model))) {
        Retention <- PopulateMeanAtLength(Retention, Length, Years,
                                          Ages, nsim,
                                          seed, silent)
      } else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
        Retention <- PopulateMeanAtWeight(Retention, Weight, Years, Ages, nsim, seed, silent)
        
      } else {
        Retention <- PopulateMeanAtAge(Retention, Ages, Years)
      }
    } 
    
  }
  
  if (ParsZero & is.null(Retention@MeanAtAge) & is.null(Retention@MeanAtLength)) {
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(Years=Years)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'Year'),Years=Years)
    
    return(SetDigest(Retention, argList))
  } 
  
  Retention <- MeanAtLength2MeanAtAge(Retention, Length, Ages,
                                      nsim, Years, seed, silent)
  
  Retention <- MeanAtWeight2MeanAtAge(Retention, Weight, Ages, nsim,
                                      Years, seed, silent) 
  
  if (CalcAtLength)
    Retention <- MeanAtAge2MeanAtLength(Retention, Length, Ages, 
                                        nsim, Years, seed, silent)
  
  if (is.null(Retention@MeanAtAge)) {
    # chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    # if (!chk@populated)
    #   FishingMortality <- PopulateFishingMortality(FishingMortality,
    #                                nsim,
    #                                Years,
    #                                seed,
    #                                silent)
    # 
    # if (!EmptyObject(FishingMortality@DeadAtAge)) {
    #   Retention@MeanAtAge <- FishingMortality2Retention(FishingMortality,
    #                                                     DiscardMortality,
    #                                                     Ages,
    #                                                     Years,
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
                                               Year=Years[1:dd[3]])
  }
  
  # Retention <- AddMeanAtAgeAttributes(Retention, Years, Ages)
  SetDigest(Retention, argList)
  
}



PopulateClosure <- function(Closure, nAreas, nsim, Years, silent) {
  argList <- list(nAreas, Years, nsim)
  
  if (EmptyObject(Closure)) {
    Closure <- array(1, dim=c(1,1, nAreas), 
                     dimnames =list(
                       Sim=1,
                       Year=Years[1],
                       Area=1:nAreas)
    )
  } else {
    dd <- dim(Closure)
    # TODO: check dimensions
    # TODO: add dimnames if neccessary
  }
  
  SetDigest(Closure, argList)
}





