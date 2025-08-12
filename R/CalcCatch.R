



CalcCatchN <- function(FDeadAtAge, FRetainAtAge, NatAge, NMortAtAge) {
  # calculate removals and retained number at age
  
  # NatAge is number at begining of time-step before mortality in this time step
  # FDeatAtAge is retained + dead discards
  
  dropArea <- !('Area' %in% names(dimnames(NatAge)))
  
  # Add Area Dimension if Missing
  FDeadAtAge <- AddDimension(FDeadAtAge, 'Area')
  FRetainAtAge <- AddDimension(FRetainAtAge, 'Area')
  NatAge <- AddDimension(NatAge, 'Area')
  NMortAtAge <- AddDimension(NMortAtAge, 'Area')
  
  FDeadTotal <- apply(FDeadAtAge, c('Sim', 'Age', 'TimeStep', 'Area'), sum)
  ZDeadTotal <- ArrayAdd(NMortAtAge, FDeadTotal)
  
  NDead <- ArrayMultiply(NatAge, (1-exp(-ZDeadTotal)))
  # Baranov
  ZDeadTotal <- AddDimension(ZDeadTotal, 'Fleet') |> aperm(c(1,2,3,5,4))
  FishingDead <- ArrayDivide(FDeadAtAge, ZDeadTotal)
  FishingRetain <- ArrayDivide(FRetainAtAge, ZDeadTotal)

  NDead <- AddDimension(NDead, 'Fleet') |> aperm(c(1,2,3,5,4))
  
  Removal <- ArrayMultiply(FishingDead, NDead) 
  Retain <- ArrayMultiply(FishingRetain, NDead)
  
  if (dropArea) {
    # drops the Area dimension if it isn't used
    Removal <- Removal |> DropDimension()
    Retain <- Retain|> DropDimension()
  }
    
  list(Removal=Removal,
       Retain= Retain
  )
}


# Calculates Catch and Fishing Mortality by each area
CalcCatch <- function(Hist, TimeStep=NULL) {
  if (length(TimeStep)!=1)
    stop('`TimeStep` must be length 1')
  
  # relative catchability in each area proportional to density
  Catchability <- purrr::map(Hist@Fleet, \(x) {
    GetCatchability(x, TimeStep) |>
      AddDimension('Area')
  })
  
  Density <- GetDensity(Hist, TimeStep)
  Catchability <- purrr::map2(Catchability, Density, ArrayDivide)
  EffortDist <- GetEffortArea(Hist, TimeStep)
  
  NatAge <- GetNumberAtAge(Hist, TimeStep)
  
  fleetnames <- FleetNames(Hist)
  
  # Calculate Catch and F within each area and F over all Areas
  for (st in 1:nStock(Hist)) {
    Catchability[[st]] <- Catchability[[st]] |> 
      aperm(c('Sim', 'TimeStep','Fleet', 'Area'))
    
    ApicalF <- ArrayMultiply(Catchability[[st]], EffortDist[[st]])
    
    if (all(ApicalF[[st]]<= tiny)) {
      apicalFEmptyArray <- array(tiny, dim=c(nSim(Hist),1),
                                 dimnames = list(
                                   Sim=1:nSim(Hist),
                                   TimeStep=TimeStep))
      
      FEmptyArray <- array(tiny, dim=c(nSim(Hist),nAge(Hist@Stock[[st]]),1),
                           dimnames = list(
                             Sim=1:nSim(Hist),
                             Age=0:MaxAge(Hist@Stock[[st]]),
                             TimeStep=TimeStep))
      for (fl in seq_along(fleetnames)) {
        ArrayFill(Hist@Fleet[[st]][[fl]]@FishingMortality@ApicalF) <- apicalFEmptyArray
        ArrayFill(Hist@Fleet[[st]][[fl]]@FishingMortality@DeadAtAge) <- FEmptyArray
        ArrayFill(Hist@Fleet[[st]][[fl]]@FishingMortality@RetainAtAge) <- FEmptyArray
        
      }
      return(Hist)
    }
    
  
    selectivity <- GetSelectivityAtAge(Hist@Fleet[[st]], TimeSteps=TimeStep) |>
      AddDimension('Area')
    retention <- GetRetentionAtAge(Hist@Fleet[[st]], TimeSteps=TimeStep) |>
      AddDimension('Area')
    discardmortality <- GetDiscardMortalityAtAge(Hist@Fleet[[st]], TimeSteps=TimeStep) |>
      AddDimension('Area')
    
    ApicalF <- ApicalF |> AddDimension('Age',0) |>
      aperm(c('Sim', 'Age', 'TimeStep', 'Fleet', 'Area'))
    FInteract <- ArrayMultiply(ApicalF, selectivity)
    FRetainAtAge <- ArrayMultiply(FInteract, retention)
    
    FDiscardTotal <- ArraySubtract(FInteract, FRetainAtAge)
    FDiscardDead <- ArrayMultiply(FDiscardTotal, discardmortality)
    FDeadAtAge <- ArrayAdd(FRetainAtAge, FDiscardDead)
    
    # TODO add Semelparous
    # see CalcNumber.R
    NMortAtAge <- GetNaturalMortalityAtAge(Hist@Stock[[st]], TimeSteps=TimeStep)
    
    CatchN <- CalcCatchN(FDeadAtAge, 
                         FRetainAtAge,
                         NatAge[[st]],
                         NMortAtAge)
    
    WeightAtAge <- GetFleetWeightAtAge(Hist@Stock[[st]], Hist@Fleet[[st]], TimeStep) |>
      AddDimension('Area')
    
    RemovalB <- ArrayMultiply(WeightAtAge, CatchN$Removal)
    RetainB <- ArrayMultiply(WeightAtAge, CatchN$Retain)
    
    ArrayFill(Hist@FDeadArea[[st]]) <- FDeadAtAge
    ArrayFill(Hist@FRetainArea[[st]]) <- FRetainAtAge
    ArrayFill(Hist@Removal[[st]]) <- RemovalB
    ArrayFill(Hist@Retain[[st]]) <- RetainB
    
    # F Overall
    FFleetEmptyArray <- array(tiny, dim=c(nSim(Hist),
                                          nAge(Hist@Stock[[st]]),1,
                                          length(fleetnames)),
                              dimnames = list(
                                Sim=1:nSim(Hist),
                                Age=0:MaxAge(Hist@Stock[[st]]),
                                TimeStep=TimeStep,
                                Fleet=fleetnames))
    
    FDeadArray <- FFleetEmptyArray
    FRetainArray <- FFleetEmptyArray
    
    
    for (sim in 1:nSim(Hist)) {
      thisSim <- as.numeric(dimnames(ApicalF)$Sim[sim])
      
      FFleetArea <- abind::adrop(ApicalF[sim,1,1,,, drop=FALSE], 1:3) 
      if (all(round(FFleetArea/mean(FFleetArea),1)==1)) {
        # Fs are the same across areas
        FDeadAtAgeList <- DropDimension(FDeadAtAge, 'Area', FALSE) |>
          SubsetSim(thisSim) |>
          Array2List(4) 
        FRetainAtAgeList <- DropDimension(FRetainAtAge, 'Area', FALSE) |>
          SubsetSim(thisSim) |>
          Array2List(4) 
        
        for (fl in seq_along(fleetnames)) {
          FDeadArray[sim,,1,] <-  FDeadAtAgeList[[fl]]
          FRetainArray[sim,,1,] <- FRetainAtAgeList[[fl]]
        }
      } else {
        # need to calculate overall Fs from catches
        CatchAtAge_i <- abind::adrop(CatchN$Removal[sim,,1,,,drop=FALSE],c(1,3)) |> 
          apply(c('Age', 'Fleet'), sum)
        
        PopatAge_i <- abind::adrop(NatAge[[st]][sim,,1,,drop=FALSE],c(1,3)) |> 
          apply(c('Age'), sum)
        NMortalityAtAge_i <- NMortAtAge[sim,,1]
        SelectAtAge_i <- abind::adrop(selectivity[sim,,1,,1, drop=FALSE], c(1,3, 5))
        RetainAtAge_i <- abind::adrop(selectivity[sim,,1,,1, drop=FALSE], c(1,3, 5))
        DiscardAtAge_i <- abind::adrop(selectivity[sim,,1,,1, drop=FALSE], c(1,3, 5))
        
        apicalF <- CalcFfromCatch_i(CatchAtAge_i, 
                                    PopatAge_i, 
                                    NMortalityAtAge_i,
                                    SelectAtAge_i,
                                    RetainAtAge_i,
                                    DiscardAtAge_i)
        apicalF <- matrix(apicalF, nrow(SelectAtAge_i), length(fleetnames))
        FInteract <- SelectAtAge_i * apicalF
        FRetainAtAge <- FInteract * RetainAtAge_i
        FDiscardTotal <- FInteract-FRetainAtAge
        FDiscardDead <- FDiscardTotal*DiscardAtAge_i
        FDeadAtAge <- FRetainAtAge+FDiscardDead
        
        FDeadAtAgeList <- FDeadAtAge |> AddDimension('Sim', sim) |>
          AddDimension('TimeStep', TimeStep) |>
          aperm(c('Sim', 'Age', 'TimeStep', 'Fleet')) |>
          Array2List(pos=4)
        
        FRetainAtAgeList <- FRetainAtAge |> AddDimension('Sim', sim) |>
          AddDimension('TimeStep', TimeStep) |>
          aperm(c('Sim', 'Age', 'TimeStep', 'Fleet')) |>
          Array2List(pos=4)
        
        for (fl in seq_along(fleetnames)) {
          FDeadArray[sim,,1,] <-  FDeadAtAgeList[[fl]]
          FRetainArray[sim,,1,] <- FRetainAtAgeList[[fl]]
        }
      }
    }
    for (fl in seq_along(fleetnames)) {
      ArrayFill(Hist@Fleet[[st]][[fl]]@FishingMortality@ApicalF) <- apply(FDeadArray, c('Sim', 'Fleet'), sum)
      ArrayFill(Hist@Fleet[[st]][[fl]]@FishingMortality@DeadAtAge) <- abind::adrop(FDeadArray[,,,fl, drop=FALSE],4)
      ArrayFill(Hist@Fleet[[st]][[fl]]@FishingMortality@RetainAtAge) <- abind::adrop(FRetainArray[,,,fl, drop=FALSE],4)
    }
  }
  
  Hist
}
