CalcNumberNext <- function(Hist, thisTimeStep) {
  # calculates number by stock and area at beginning of next time step
  # Age-0 is calculated later
  
  timesteps <- TimeSteps(Hist)
  ind <- which(timesteps==thisTimeStep)
  nextTimeStep <- timesteps[ind+1]
  
  NumberThisTimeStep <- GetNumberAtAge(Hist, thisTimeStep)
  NumberNextTimeStep <- vector('list', nStock(Hist))
  names(NumberNextTimeStep) <- StockNames(Hist)
  
  EffortDist <- CalcEffortDist(Hist, thisTimeStep)
  RelDensity <- purrr::pmap(list(Hist@Stock,
                                 Hist@Fleet,
                                 NumberThisTimeStep), 
                            TimeSteps=thisTimeStep, CalcDensity)
  
  Catchability <- purrr::map(Hist@Fleet, GetCatchability,
                             TimeSteps=thisTimeStep) |>
    purrr::map(AddDimension, 'Area')
  Catchability <- purrr::map2(Catchability, RelDensity, ArrayDivide)
  
  ApicalF <- purrr::map2(Catchability, EffortDist, ArrayMultiply) |>
    purrr::map(AddDimension, 'Age') |>
    purrr::map(aperm, c(1,5,2,3,4))
  
  selectivity <- purrr::map(Hist@Fleet, GetSelectivityAtAge, TimeSteps=thisTimeStep) |>
    purrr::map(AddDimension, 'Area')
  retention <- purrr::map(Hist@Fleet, GetRetentionAtAge, TimeSteps=thisTimeStep) |>
    purrr::map(AddDimension, 'Area')
  discardmortality <- purrr::map(Hist@Fleet,
                                 GetDiscardMortalityAtAge, TimeSteps=thisTimeStep) |>
    purrr::map(AddDimension, 'Area')
  
  FInteract <- purrr::map2(ApicalF, selectivity, ArrayMultiply)
  FRetainAtAge <- purrr::map2(FInteract, retention, ArrayMultiply)
  FDiscardTotal <- purrr::map2(FInteract, FRetainAtAge, ArraySubtract)
  FDiscardDead <-  purrr::map2(FDiscardTotal, discardmortality, ArrayMultiply)
  FDeadAtAge <- purrr::map2(FRetainAtAge, FDiscardDead, ArrayAdd)
  
  NMortAtAge <- purrr::map(Hist@Stock, GetNMortalityAtAge, TimeSteps=thisTimeStep) |>
    purrr::map(AddDimension, 'Fleet') |>
    purrr::map(AddDimension, 'Area')
  
  ZMortAtAge <- purrr::map2(FDeadAtAge, NMortAtAge, ArrayAdd)
  
  for (st in 1:nStock(Hist)) {
    dd <- dim(ZMortAtAge[[st]])
    nSim <- dd[1]
    nAge <- dd[2]
    nFleet <- dd[4]
    nArea <- dd[5]
    SAFR <- expand.grid(1:nSim, 1:(nAge-1),1, 1:nFleet, 1:nArea) |> as.matrix()
    SA1FR <- expand.grid(1:nSim, 2:nAge, 1, 1:nFleet, 1:nArea) |> as.matrix()
    SALFR <- expand.grid(1:nSim, nAge, 1, 1:nFleet, 1:nArea) |> as.matrix()
    
    NumberNextTimeStep[[st]] <- NumberThisTimeStep[[st]]
    dimnames(NumberNextTimeStep[[st]])$`Time Step` <- nextTimeStep
    
    NumberNextTimeStep[[st]][] <- 0
    
    NumberNextTimeStep[[st]][SA1FR[,c(1,2,3,5)]] <- NumberThisTimeStep[[st]][SAFR[,c(1,2,3,5)]] *
      exp(-ZMortAtAge[[st]][SAFR])
    
    if (GetPlusGroup(Hist@Stock[[st]])) {
      NumberNextTimeStep[[st]][SALFR[,c(1,2,3,5)]] <-  NumberNextTimeStep[[st]][SALFR[,c(1,2,3,5)]] +
        NumberThisTimeStep[[st]][SALFR[,c(1,2,3,5)]] * exp(-ZMortAtAge[[st]][SALFR])
    }
    
    # movement at beginning of  next timestep
    Movement <- GetMovementAtAge(Hist@Stock[[st]], TimeSteps = nextTimeStep)
    if (!is.null(Movement)) {
      nArea <- nArea(Hist@Stock[[st]])
      preMovement <- replicate(nArea, NumberNextTimeStep[[st]])
      l <- dimnames(preMovement)
      l[[5]] <- l[[4]]
      names(l)[5] <- names(l)[4]
      dimnames(preMovement) <- l
      movement <- Movement |> aperm(c(1,4,5,2,3))
      postMovement <- ArrayMultiply(preMovement, movement) |>
        apply(c(1:3,5), sum)
      NumberNextTimeStep[[st]] <- postMovement
    }
    
    ArrayFill(Hist@Number[[st]]) <-  NumberNextTimeStep[[st]]
  }
  Hist
}