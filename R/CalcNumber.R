CalcNumberNext <- function(Hist, thisTimeStep) {
  # calculates number by stock and area at beginning of next time step
  # Age-0 is calculated later
  
  timesteps <- TimeSteps(Hist)
  ind <- which(timesteps==thisTimeStep)
  nextTimeStep <- timesteps[ind+1]
  
  NumberThisTimeStep <- GetNumberAtAge(Hist, thisTimeStep)
  NumberNextTimeStep <- vector('list', nStock(Hist))
  names(NumberNextTimeStep) <- StockNames(Hist)

  for (st in 1:nStock(Hist)) {
    
    FDeadAtAge <- Hist@FDeadArea[[st]] |> ArraySubsetTimeStep(thisTimeStep) 
    FDeadAtAgeTotal <- apply(FDeadAtAge, c('Sim', 'Age', 'Time Step', 'Area'), sum)
    NMortAtAge <- GetNMortalityAtAge(Hist@Stock[[st]], thisTimeStep) |>
      AddDimension('Area')
    
    ZMortAtAge <- ArrayAdd(FDeadAtAgeTotal,NMortAtAge)
  
    dd <- dim(FDeadAtAge)
    nSim <- dd[1]
    nAge <- dd[2]
    nFleet <- dd[4]
    nArea <- dd[5]
    SAFR <- expand.grid(1:nSim, 1:(nAge-1),1, 1:nFleet, 1:nArea) |> as.matrix()
    SA1FR <- expand.grid(1:nSim, 2:nAge, 1, 1:nFleet, 1:nArea) |> as.matrix()
    SALFR <- expand.grid(1:nSim, nAge, 1, 1:nFleet, 1:nArea) |> as.matrix()
    
    NumberNextTimeStep[[st]] <- NumberThisTimeStep[[st]]
    dimnames(NumberNextTimeStep[[st]])$`Time Step` <- nextTimeStep
    
    NumberNextTimeStep[[st]][] <- tiny
    
    NumberNextTimeStep[[st]][SA1FR[,c(1,2,3,5)]] <- NumberThisTimeStep[[st]][SAFR[,c(1,2,3,5)]] *
      exp(-ZMortAtAge[SAFR[,c(1:3,5)]])
    
    if (GetPlusGroup(Hist@Stock[[st]])) {
      NumberNextTimeStep[[st]][SALFR[,c(1,2,3,5)]] <-  NumberNextTimeStep[[st]][SALFR[,c(1,2,3,5)]] +
        NumberThisTimeStep[[st]][SALFR[,c(1,2,3,5)]] * exp(-ZMortAtAge[SALFR[,c(1:3,5)]])
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