
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
  
  FDeadTotal <- apply(FDeadAtAge, c('Sim', 'Age', 'Time Step', 'Area'), sum)
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


CatchByArea <- function(Hist, TimeSteps=NULL) {
  
  
 
  
  # Distribute Effort across Areas proportional to Density
  EffortDist <- CalcEffortDist(Hist, TimeSteps)
  
  NatAgeArea <- purrr::map(Hist@Number, 
                           ArraySubsetTimeStep, 
                           TimeSteps=TimeSteps)
  


  
  # Relative catchability by Area proportional to Density
  RelDensity <- purrr::pmap(list(Hist@Stock,
                                 Hist@Fleet,
                                 NatAgeArea), 
                            TimeSteps=TimeSteps, CalcDensity)
  
  Catchability <- purrr::map(Hist@Fleet, GetCatchability,
                             TimeSteps=TimeSteps) |>
    purrr::map(AddDimension, 'Area')
  
  Catchability <- purrr::map2(Catchability, RelDensity, ArrayDivide)
  
  ApicalF <- purrr::map2(Catchability, EffortDist, ArrayMultiply) |>
    purrr::map(AddDimension, 'Age') |>
    purrr::map(aperm, c(1,5,2,3,4))
  
  # TODO skip if all Fs are 0
  
  selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=TimeSteps) |>
    purrr::map(AddDimension, 'Area')
  retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=TimeSteps) |>
    purrr::map(AddDimension, 'Area')
  discardmortality <- purrr::map(Hist@Fleet,
                                 GetDiscardMortalityAtAge, TimeSteps=TimeSteps) |>
    purrr::map(AddDimension, 'Area')
  
  FInteract <- purrr::map2(ApicalF, selectivity, ArrayMultiply)
  FRetainAtAge <- purrr::map2(FInteract, retention, ArrayMultiply)
  FDiscardTotal <- purrr::map2(FInteract, FRetainAtAge, ArraySubtract)
  FDiscardDead <-  purrr::map2(FDiscardTotal, discardmortality, ArrayMultiply)
  FDeadAtAge <- purrr::map2(FRetainAtAge, FDiscardDead, ArrayAdd)
  
  NMortAtAge <- purrr::map(Hist@Stock, GetNMortalityAtAge, TimeSteps=TimeSteps)
  
  CatchN <- purrr::pmap(list(FDeadAtAge=FDeadAtAge,
                             FRetainAtAge=FRetainAtAge,
                             NatAge=NatAgeArea,
                             NMortAtAge=NMortAtAge), 
                        CalcCatchN)
  
  RemovalN <- lapply(CatchN , '[[', 'Removal')
  RetainN <- lapply(CatchN , '[[', 'Retain')
  
  WeightAtAge <- purrr::map2(Hist@Stock, Hist@Fleet, 
                             GetFleetWeightAtAge, TimeSteps=TimeSteps) |>
    purrr::map(AddDimension, 'Area')
  
  RemovalB <- purrr::map2(WeightAtAge, RemovalN, ArrayMultiply)
  RetainB <- purrr::map2(WeightAtAge, RemovalN, ArrayMultiply)
  
  for (i in seq_along(RemovalB)) {
    ArrayFill(Hist@Removal[[i]]) <- RemovalB[[i]]
    ArrayFill(Hist@Retain[[i]]) <- RetainB[[i]]
  }
  
  # if F is the same in each area, update overall F (same) in the Hist object
  chk <- apply(ApicalF[[1]],c(1,5), mean)
  chksame <- chk/matrix(apply(chk, 1, mean), nrow(chk), ncol(chk), byrow=FALSE) |>
    round(2)
  
  if (prod(chksame) || all(chk==0)) {
    ApicalF <- purrr::map(ApicalF, \(x) 
                          abind::asub(x, list(1,1,1), c(2,4,5), drop=FALSE) |>
                            abind::adrop(c(2,4,5))
    )
    Hist@Fleet <- purrr::map2(Hist@Fleet, ApicalF, \(x,y)
                              CalcFatAge(x, TimeSteps=TimeSteps, y)
    )
  } else { 
    # otherwise, calculate F by Area from Catch 
    Hist <- CalcFleetFMortality(Hist, TimeSteps)
  }
  
  
  Hist
}
