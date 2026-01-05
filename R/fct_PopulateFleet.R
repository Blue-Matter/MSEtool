#' @describeIn Populate Populate an [fleet-class()] object
#' @export
PopulateFleet <- function(Fleet, 
                          Stock,
                          seed=103,
                          silent=FALSE) {

  Ages <- Stock@Ages
  Length <- Stock@Length 
  Weight <- Stock@Weight
  RelativeSize <- Stock@Spatial@RelativeSize
  
  
  Fleet@CurrentYear <- Stock@CurrentYear
  Fleet@nSim <- Stock@nSim
  Fleet@Years <- Stock@Years
  Fleet@nYear <- Stock@nYear
  Fleet@pYear <- Stock@pYear
  Fleet@Seasons <- Stock@Seasons
  
  Fleet@Years <- CalcYears(nYear=Stock@nYear, 
                           pYear=Stock@pYear, 
                           CurrentYear=Stock@CurrentYear, 
                           Seasons= Stock@Seasons )
  
  nSim <- Fleet@nSim 
  Years <- Fleet@Years 
  HistYears <- Years(Fleet, 'Historical')
  ProjYears <- Years[!Years %in% HistYears]
  nArea <- ncol(RelativeSize)
  
  argList <- list(Ages, Length, Weight, RelativeSize, nsim, Years, seed)
  if (CheckDigest(Fleet, argList) | EmptyObject(Fleet))
    return(Fleet)
  
  SetSeed(seed)
  
  Fleet@Effort <- PopulateEffort(Effort=Fleet@Effort, 
                                 HistYears, 
                                 nArea,
                                 nSim, 
                                 seed)

  Fleet <- PopulateCatchability(Fleet,
                                RelativeSize,
                                nSim,
                                HistYears,
                                ProjYears,
                                seed,
                                silent)
  
  Fleet@Selectivity <- PopulateSelectivity(Selectivity=Fleet@Selectivity,
                                           Ages,
                                           Length,
                                           Weight,
                                           nSim,
                                           Years,
                                           nArea,
                                           CalcAtLength=FALSE,
                                           seed,
                                           silent=silent)
  
  Fleet@Retention <- PopulateRetention(Retention=Fleet@Retention,
                                       Ages,
                                       Length,
                                       Weight,
                                       nSim,
                                       Years,
                                       nArea,
                                       CalcAtLength=FALSE,
                                       seed,
                                       silent=silent)
  
  Fleet@DiscardMortality <- PopulateDiscardMortality(DiscardMortality=Fleet@DiscardMortality,
                                                     Ages,
                                                     Length,
                                                     nSim,
                                                     Years,
                                                     nArea,
                                                     CalcAtLength=FALSE,
                                                     seed=seed,
                                                     silent)
  


  Fleet@Closure <- PopulateClosure(Closure=Fleet@Closure,
                                   nArea,
                                   nSim,
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


PopulateEffort <- function(Effort, HistYears, nArea=1, nSim=5, seed=NULL) {

  SetSeed(seed)

  if (is.null(Effort@Value))
    return(Effort)
  
  if (inherits(Effort@Value, 'data.frame')) {
    Effort <- GenerateHistoricalEffort(Effort, nSim, HistYears)
  }
  
  dd <- dim(Effort@Value)
  if (dd[2] != length(HistYears)) {
    cli::cli_abort("`ncol(Effort@Value)` is not equal to `length(HistYears)`")
  }
  dimnames(Effort@Value) <- list(Sim=1:nrow(Effort@Value),
                                 Year=HistYears)
    
  Effort@Distribution <- PopulateDistribution(Distribution=Effort@Distribution,
                                              nSim,
                                              HistYears,
                                              nArea)
  Effort
}


PopulateDistribution <- function(Distribution, 
                                 nSim=5,
                                 HistYears=NULL,
                                 nArea=NULL) {
  
  if (is.null(Distribution)) {
    Distribution <- array(tiny, dim=c(1, 1, nArea),
                          dimnames = list(
                            Sim=1,
                            Year=HistYears[1],
                            Area=1:nArea
                          ))
    
  } else {
    CheckClass(Distribution, c('array', 'matrix'))
    dd <- dim(Distribution)
    # TODO check dimensions
    # TODO add dimnames if needed
    
    
  }
  
  Distribution
}


PopulateCatchability <- function(Fleet,
                                 RelativeSize,
                                 nSim=5,
                                 HistYears=NULL,
                                 ProjYears=NULL,
                                 seed=NULL,
                                 silent=FALSE) {
  
  Catchability <- Fleet@Catchability
  pYears <- length(ProjYears)
  
  Years <- c(HistYears, ProjYears)
  
  if (all(is.na(Catchability@Value)) || all(Catchability@Value <= tiny)) {
    Catchability@Value <- array(1, dim=c(nSim, length(Years)),
                          dimnames = list(
                            Sim=1:nSim,
                            Year=Years
                          ))
  } else {
    dd <- dim(Catchability@Value)
    if (dd[1] != nSim) {
      if (dd[1]!=1)
        cli::cli_abort(c('x'="Incorrect number of rows in `Catchability@Value` matrix.",
                         'i'="Must have either 1 row or `nSim` ({.val {nSim}}) rows. "
        ))
    }

    if (is.null( dimnames(Catchability@Value)))
      dimnames(Catchability@Value) <- list(Sim=1:nrow(Catchability@Value),
                                     Year=Years[1:ncol(Catchability@Value)])
    
    Catchability@Value <- ExtendYears(Catchability@Value, HistYears) 
  }
  
  if (!is.null(Fleet@Catchability@qInc)) {
    qIncs <- StructurePars_(Fleet@Catchability@qInc, nSim, Years)[,1]
    qIncs <- sapply(qIncs, function(x)
      (1+x/100)^(1:pYears)
    ) |> t()
    
    dimnames(qIncs) <- list(Sim=1:nSim,
                            Year=ProjYears)
    
    qfuture <- ArrayMultiply(SubsetYear(Catchability@Value, ProjYears), qIncs)
    ArrayFill(Catchability@Value) <- qfuture
    Fleet@Catchability@qInc <- qIncs
  }
  
  
  if (!is.null(Fleet@Catchability@qCV)) {
    qCVs <- StructurePars_(Fleet@Catchability@qCV, nSim, Years)[,1]
    Fleet@Catchability@qCV <- qCVs
    
    qmu <- -0.5 * qCVs^2
    qvar <- array(exp(rnorm(pYears * nSim, rep(qmu, pYears), rep(qCVs, pYears))), c(nSim, pYears),
                  dimnames = list(
                    Sim=1:nSim,
                    Year=ProjYears
                  ))
    
    qfuture <- ArrayMultiply(SubsetYear(Catchability@Value, ProjYears), qvar)
    if (!all(qfuture==1)) 
      ArrayFill(Catchability@Value) <- qfuture
  }
  Fleet@Catchability@Value <- Catchability@Value
  
  if (EmptyObject(Fleet@Catchability@qArea)) {
    
    Fleet@Catchability@qArea <- ArrayDivide(array1=AddDimension(Catchability@Value,'Area'),
                               array2=AddDimension(RelativeSize, 'Year', val=Years[1]) |>
                                 aperm(c('Sim', 'Year', 'Area'))
    )
  } else {
    
    
    dd <- dim(Fleet@Catchability@qArea)
    # TODO: check dimensions
    # TODO: add dimnames if neccessary
    
  }
  Fleet
}


PopulateSelectivity <- function(Selectivity,
                                Ages=NULL,
                                Length=NULL,
                                Weight=NULL,
                                nSim=5,
                                Years=NULL,
                                nArea=1,
                                CalcAtLength=TRUE,
                                seed=NULL,
                                silent=FALSE,
                                CheckMaxValue=TRUE,
                                class='Selectivity') {

  argList <- list(Ages, Length, Weight, Years, nArea, nSim, CalcAtLength, seed)
  
  if (CheckDigest(Selectivity, argList))
    return(Selectivity)
  
  SetSeed(seed)
  
  Selectivity@Pars <- StructurePars(Pars=Selectivity@Pars, nSim, Years, nArea)
  
  Selectivity@Model <- FindModel(Selectivity)
  ModelClass <- getModelClass(Selectivity@Model)
  
  if (!is.null(ModelClass)) {
    # Model & Parameters exist
    if (grepl('at-Length', getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtLength(Selectivity, 
                                          Length, 
                                          Years,
                                          Ages, 
                                          nSim,
                                          seed, 
                                          silent)
      
    } else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtWeight(Selectivity, Weight, Years, Ages, nSim, seed, silent)
      
    } else if (grepl('at-Age', getModelClass(Selectivity@Model))) {
      Selectivity <- PopulateMeanAtAge(Selectivity, Ages, Years, Length)  
    }
  }
  
  Selectivity <- MeanAtLength2MeanAtAge(Selectivity, Length, max1=TRUE)
  Selectivity <- MeanAtWeight2MeanAtAge(Selectivity, Weight, max1=TRUE)
  
  if (CalcAtLength) {
    Selectivity <- MeanAtAge2MeanAtLength(Selectivity, Length, replace=FALSE)
  }

  
  if (is.null(Selectivity@MeanAtAge)) {
    cli::cli_abort(' {.var {class}} requires either `Model` & `Pars` or `MeanAtAge`')
  }
  
  # Check Selectivity has a max value of 1 across age classes
  if(CheckMaxValue) 
    Selectivity@MeanAtAge <- CheckSelectivityMaximum(Selectivity@MeanAtAge)
  
  # Dimnames for at length
  # TODO - add Area dimension
  if (!is.null(Selectivity@MeanAtLength)) {
    if (is.null(names(dimnames(Selectivity@MeanAtLength)))) {
      dd <- dim(Selectivity@MeanAtLength)
      dimnames(Selectivity@MeanAtLength) <- list(Sim=1:dd[1],
                                                 Class=Selectivity@Classes,
                                                 Year=Years[1:dd[3]])
    }
  }
  
  if (!is.null(Selectivity@MeanAtAge)) {
    dd <- dim(Selectivity@MeanAtAge)
    if (length(dd)==3) {
      # add Area dimension
      Selectivity@MeanAtAge <- Selectivity@MeanAtAge |> AddDimension('Area')
    }
    if (is.null(names(dimnames(Selectivity@MeanAtAge)))) {
        dimnames(Selectivity@MeanAtAge) <- list(Sim=1:dd[1],
                                                Age=Ages@Classes[1:dd[2]],
                                                Year=Years[1:dd[3]],
                                                Area=1)
     
    }
  }
  
  SetDigest(Selectivity, argList)
}


PopulateRetention <- function(Retention, 
                              Ages=NULL,
                              Length=NULL,
                              Weight=NULL,
                              nSim=5,
                              Years=NULL,
                              nArea=1,
                              CalcAtLength=TRUE,
                              seed=NULL,
                              silent=FALSE) {
  
  # Years <- YearAttributes(Retention, Years)
  argList <- list(Ages, Length, Years, nSim, CalcAtLength, seed)
  
  if (CheckDigest(Retention, argList))
    return(Retention)
  
  
  
  if (EmptyObject(Retention)) {
    Retention@MeanAtAge <- array(1, dim=c(1,length(Ages@Classes),1)) |> 
      AddDimNames(Years=Years, Ages=Ages@Classes)
    if (!is.null(Length@Classes)) {
      Retention@MeanAtLength <- array(1, dim=c(1,length(Length@Classes),1)) |> 
        AddDimNames(c('Sim', 'Class', 'Year'), Years=Years)
      dimnames(Retention@MeanAtLength)[[2]] <- Length@Classes
      
    } else {
      Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
        AddDimNames(c('Sim', 'Class', 'Year'), Years=Years)  
    }
    return(SetDigest(Retention, argList))
  }
  
  SetSeed(seed)
  
  Retention@Pars <- StructurePars(Pars=Retention@Pars, nSim, Years)
  
  ParsZero <- all((lapply(lapply(Retention@Pars, `==`, 0), prod) |> 
                     unlist())==1)
  
  if (!ParsZero) {
    Retention@Model <- FindModel(Retention)
    ModelClass <- getModelClass(Retention@Model)
    
    if (!is.null(ModelClass)) {
      if (grepl('at-Length',getModelClass(Retention@Model))) {
        Retention <- PopulateMeanAtLength(Retention, Length, Years,
                                          Ages, nSim,
                                          seed, silent)
      } else if (grepl('at-Weight',getModelClass(Selectivity@Model))) {
        Retention <- PopulateMeanAtWeight(Retention, Weight, Years, Ages, nSim, seed, silent)
        
      } else {
        Retention <- PopulateMeanAtAge(Retention, Ages, Years)
      }
    } 
    
  }
  
  if (ParsZero & is.null(Retention@MeanAtAge) & is.null(Retention@MeanAtLength)) {
    Retention@MeanAtAge <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(Years=Years)
    Retention@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'Year'), Years=Years)
    
    return(SetDigest(Retention, argList))
  } 
  
  Retention <- MeanAtLength2MeanAtAge(Retention, Length)
  Retention <- MeanAtWeight2MeanAtAge(Retention, Weight) 
  
  if (CalcAtLength)
    Retention <- MeanAtAge2MeanAtLength(Retention, Length, Ages, 
                                        nSim, Years, seed, silent)
  
  if (is.null(Retention@MeanAtAge)) {
    # chk <- CheckRequiredObject(FishingMortality, 'fishingmortality', 'FishingMortality')
    # if (!chk@populated)
    #   FishingMortality <- PopulateFishingMortality(FishingMortality,
    #                                nSim,
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


PopulateDiscardMortality <- function(DiscardMortality,
                                     Ages=NULL,
                                     Length=NULL,
                                     nSim=5,
                                     Years=NULL,
                                     nArea=1,
                                     CalcAtLength=TRUE,
                                     seed=NULL,
                                     silent=FALSE) {
  # Years <- YearAttributes(DiscardMortality, Years)
  argList <- list(Ages, Length, nSim, Years, CalcAtLength, seed)
  
  if (CheckDigest(DiscardMortality, argList))
    return(DiscardMortality)
  
  if (EmptyObject(DiscardMortality)) {
    DiscardMortality@MeanAtAge <- array(0, dim=c(1,length(Ages@Classes),1)) |> 
      AddDimNames(Years=Years, Ages=Ages@Classes)
    DiscardMortality@MeanAtLength <- array(1, dim=c(1,1,1)) |> 
      AddDimNames(c('Sim', 'Class', 'Year'), Years=Years)  
    return(SetDigest(DiscardMortality, argList))
  }
  
  SetSeed(seed)
  
  DiscardMortality <- MeanAtLength2MeanAtAge(DiscardMortality, Length)
  if (CalcAtLength)
    DiscardMortality <- MeanAtAge2MeanAtLength(DiscardMortality, Length, Ages,
                                               nSim, Years, seed, silent)
  
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



PopulateClosure <- function(Closure, nArea, nSim=5, Years, silent) {
  argList <- list(nArea, Years, nSim)
  
  if (EmptyObject(Closure)) {
    Closure <- array(1, dim=c(1,1, nArea), 
                     dimnames =list(
                       Sim=1,
                       Year=Years[1],
                       Area=1:nArea)
    )
  } else {
    dd <- dim(Closure)
    if (dd[3]!=nArea) {
      if (dd[1]==1 && dd[2]==1) {
        Closure <- array(1, dim=c(1,1, nArea), 
                         dimnames =list(
                           Sim=1,
                           Year=Years[1],
                           Area=1:nArea)
        )
        
        
      } else {
        cli::cli_abort("Error in {.val Fleet@Closure", .internal=TRUE)
      }
    }
   
    
    # TODO: check dimensions
    # TODO: add dimnames if neccessary
  }
  
  SetDigest(Closure, argList)
}





