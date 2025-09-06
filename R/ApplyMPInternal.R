

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
  FleetAllocation <- ProjSim@OM@Allocation
  
  # TODO - this is setting effort individually for stocks - ie different effort by stock for each fleet
  
  for (st in 1:nStock) {
    
    TotalRemovalsFleet <- MPAdvice@TAC *StockAllocation[st] * FleetAllocation[[st]] |> as.numeric()

    SolvedF <- SolveForFishingMortality(NumberAtAge[[st]],
                                        TotalRemovalsFleet,
                                        SelectivityAtAge[[st]],  
                                        RetentionAtAge[[st]],  
                                        DiscardMortalityAtAge[[st]],  
                                        FleetWeightAtAge[[st]],
                                        NaturalMortalityAtAge[[st]]) 
  
    FInteract <- t(SolvedF$ApicalFInteract)
    apicalFDead <- apply(SolvedF$FDeadAtAge, 1, sum) |> max()
    
    if (apicalFDead > ProjSim@OM@maxF) {
      FInteract <- FInteract * ProjSim@OM@maxF/apicalFDead
    }
    
    RequiredEffort <- FInteract / ProjSim@OM@Fleet[[st]]@Effort@Catchability[TSIndex,] 
    RequiredEffort[RequiredEffort<1E-5] <- 1E-5 
    
    ProjSim@Effort[st,TSIndex,] <- as.vector(RequiredEffort)
  }
  
  ProjSim
}



UpdateApicalF <- function(ProjSim, MPAdvice, TimeStep, TSIndex) {
  if (is.null(MPAdvice))
    return(ProjSim)
  
  if (length(MPAdvice@apicalF)<1)
    return(ProjSim)
  
  apicalF <- MPAdvice@apicalF 

  FleetAllocationF <- CalcFleetAllocationF(ProjSim@OM@Fleet, TimeStep)
  

  apicalFAge <- apicalF * FleetAllocationF  |> 
    AddDimension("Age") |> 
    aperm(c('Stock', 'Age', 'TimeStep', 'Fleet'))
  
  # TODO - currently only for stock = 1 
  
  SelectivityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Selectivity@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  RetentionAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@Retention@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  DiscardMortalityAtAge <- purrr::map(ProjSim@OM@Fleet, \(stock) stock@DiscardMortality@MeanAtAge[,TSIndex,, drop=FALSE] |> abind::adrop(2))
  
  st <- 1

  FInteract <- ArrayMultiply(apicalFAge[st,,1,, drop=FALSE] |> abind::adrop(c(1,3)),
                             SelectivityAtAge[[st]])
  
  FRetain <- ArrayMultiply(FInteract, RetentionAtAge[[st]])  
  FDiscardTotal <- ArraySubtract(FInteract, FRetain)
  FDiscardDead <- ArrayMultiply(FDiscardTotal, DiscardMortalityAtAge[[st]])
  FDead <- FRetain + FDiscardDead
  FDeadTotal <- apply(FDead, 'Age',  sum) 
  ActualApicalF <- max(FDeadTotal)  
  if (abs(ActualApicalF/apicalF -1) > 1e-2) {
    adjust <- apicalF/ActualApicalF
    FInteract <- FInteract * adjust
  }
  
  RequiredEffort <- apply(FInteract, 2, max) / ProjSim@OM@Fleet[[st]]@Effort@Catchability[TSIndex,] 
  RequiredEffort[RequiredEffort<1E-5] <- 1E-5 
  
  ProjSim@Effort[st,TSIndex,] <- RequiredEffort
  ProjSim
  
}

UpdateEffort <- function(ProjSim, MPAdvice, MPAdvicePrevious, TimeStepsAll, TimeStepsHist, TSIndex) {
  
  # *************************** # 
  st <- 1
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
    futureEffort <- ProjSim@Effort[st,LastHistIndex,] * MPAdvice@Effort
    ProjSim@Effort[st,projInd,] <-  matrix(futureEffort, nrow=length(projInd), ncol=length(futureEffort), byrow=TRUE)
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
    
    nAge <- length(ProjSim@OM@Stock[[st]]@Ages@Classes)
    MeanAtAge <- MPAdvice@DiscardMortality@MeanAtAge
    if (length(MeanAtAge)!=1& length(MeanAtAge)!=nAge) 
      cli::cli_abort("`MPAdvice@DiscardMortality@MeanAtAge` must be either length 1 or length `nAge` ({.val {nAge}})")

    if (length(MeanAtAge)==1)
      MeanAtAge <- rep(MeanAtAge, nAge)
    
    MeanAtAge <- matrix(MeanAtAge, nrow=nAge, ncol=nFleet(ProjSim@OM))
    MeanAtAge <- replicate(length(projInd), MeanAtAge) |> aperm(c(1,3,2))
    ProjSim@OM@Fleet[[st]]@DiscardMortality@MeanAtAge[,projInd,] <- MeanAtAge
    
    # TODO 
    # r = MeanAtAge2MeanAtLength(object=ProjSim@OM@Fleet[[st]]@DiscardMortality,
    #                            Length=ProjSim@OM@Stock[[st]]@Length,
    #                            Ages=ProjSim@OM@Stock[[st]]@Ages,
    #                            nsim=1,
    #                            TimeSteps=TimeStepProj,
    #                            replace = TRUE
    #                            )
    # 
    # r@MeanAtAge[,120,]
    # r@MeanAtLength[,120,]
  }
  if (length(MPAdvice@DiscardMortality@MeanAtLength)>0) {
    cli::cli_abort("`MPAdvice@DiscardMortality@MeanAtLength` not done yet", .internal=TRUE)
    
  }
  # repeat for MeanAtLength
  ProjSim
  
}

UpdateSelectivity <- function(ProjSim, MPAdvice, MPAdvicePrevious, 
                              TimeStepsAll, TSIndex,type=c('Selectivity', 'Retention')) {
  type <- match.arg(type)
  
  if (is.null(MPAdvice))
    return(ProjSim)
  

  # if (!is.null(MPAdvicePrevious)) {
  #   if (IdenticalS4(slot(MPAdvice,type), slot(MPAdvicePrevious,type)))
  #     return(ProjSim)  
  # }
  
  Selectivity <- slot(MPAdvice,type)
  if (EmptyObject(Selectivity))
    return(ProjSim)
  
  # TODO checks
  
  TimeStep <- TimeStepsAll[TSIndex]
  nprojTS <- length(TimeStepsAll)
  projInd <- TSIndex:nprojTS
  
  Selectivity@Model <- FindModel(Selectivity)
  
  ModelClass <- getModelClass(Selectivity@Model)
  
  nStock <- nStock(ProjSim@OM)
  nFleet <- nFleet(ProjSim@OM)
  
  for (st in 1:nStock) {
    
    Length <- ProjSim@OM@Stock[[st]]@Length
    
    for (fl in 1:nFleet) {
      if (type=="Selectivity") {
        Length@Classes <- ProjSim@OM@Fleet[[st]]@Selectivity@Classes[[fl]]
      } else  {
        Length@Classes <- ProjSim@OM@Fleet[[st]]@Retention@Classes[[fl]] # TODO Classes should be populated
        if (is.null(Length@Classes))
          Length@Classes <- ProjSim@OM@Fleet[[st]]@Selectivity@Classes[[fl]]
      }
      
      Length@MeanAtAge <- Length@MeanAtAge |> ArraySubsetTimeStep(TimeStep)
      Length@CVatAge <- Length@CVatAge |> ArraySubsetTimeStep(TimeStep)
      SDatAge <- ArrayMultiply(Length@MeanAtAge, Length@CVatAge)
      
      Length@ASK <- CalcAgeSizeKey_(Length@MeanAtAge, 
                                    SDatAge, 
                                    Length@Classes, 
                                    Length@TruncSD, 
                                    Length@Dist)
      
      Weight <- ProjSim@OM@Stock[[st]]@Weight |> ArraySubsetTimeStep(TimeStep)
      Ages <- ProjSim@OM@Stock[[st]]@Ages
      if (grepl('at-Length',ModelClass)) {
        Selectivity@Classes <- Length@Classes
        
        Selectivity@MeanAtLength <- GenerateMeanatLength(Model=Selectivity@Model,
                                                         Pars=Selectivity@Pars,
                                                         Length=Selectivity@Classes)
        dimnames(Selectivity@MeanAtLength) <- list(Sim=1,
                                                   Class=   Selectivity@Classes,
                                                   TimeStep=TimeStep)
        
        Selectivity@MeanAtLength <- Selectivity@MeanAtLength 
        
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
                                              TimeStep=TimeStep)
      
      if (type=="Selectivity") {
        ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtLength[,projInd,fl] <- Selectivity@MeanAtLength[1,,]
        ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtWeight[,projInd,fl] <- Selectivity@MeanAtWeight[1,,]
        ProjSim@OM@Fleet[[st]]@Selectivity@MeanAtAge[,projInd,fl] <- Selectivity@MeanAtAge[1,,]
      } else if (type=="Retention") {
        ProjSim@OM@Fleet[[st]]@Retention@MeanAtLength[,projInd,fl] <- Selectivity@MeanAtLength[1,,]
        ProjSim@OM@Fleet[[st]]@Retention@MeanAtWeight[,projInd,fl] <- Selectivity@MeanAtWeight[1,,]
        ProjSim@OM@Fleet[[st]]@Retention@MeanAtAge[,projInd,fl] <- Selectivity@MeanAtAge[1,,]
      } else{
        stop("type must be `Selectivity` or `Retention`")
      }
    }
  }
  
  ProjSim
}

