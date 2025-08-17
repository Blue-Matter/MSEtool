
CalcInitialTimeStep <- function(Hist, silent=FALSE) {
  
  if (is.null(Hist@Unfished@Equilibrium@Number))
    Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  
  nSim <- Hist@OM@nSim
  nStock <- nStock(Hist@OM)
  nArea <- nArea(Hist@OM)
  
  # Dynamic Initial Number at Age Area
  for (st in 1:nStock) {
    
    EquilNumber <- abind::adrop(Hist@Unfished@Equilibrium@Number[[st]][,,1, drop=FALSE], 3) |>
      ExpandSims(nSim)
    DynNumber <- EquilNumber
    
    RecDevInit <- Hist@OM@Stock[[st]]@SRR@RecDevInit |>
      ExpandSims(nSim)
    RecDevHist <- Hist@OM@Stock[[st]]@SRR@RecDevHist
    RecDevHist1 <- RecDevHist[,1, drop=FALSE] |>
      ExpandSims(nSim)
    names(dimnames(RecDevHist1))[2] <- 'Age'
  
    ages <- as.numeric(dimnames(RecDevInit)[['Age']])
    
    InitAgeClassRecDevs <- cbind(RecDevHist1, RecDevInit) 
    dimnames(InitAgeClassRecDevs) <- list(Sim=1:nSim,
                                          Age=c(ages[1]-1, ages))
    
    NatAge <- ArrayMultiply(InitAgeClassRecDevs, EquilNumber) |>
      AddDimension('Area') 
    
    UnfishedDist <- abind::adrop(Hist@OM@Stock[[st]]@Spatial@UnfishedDist[,,,1,drop=FALSE], 4) |>
      aperm(c('Sim', 'Age', 'Area'))
    
    Hist@Number[[st]][,,1,] <- ArrayMultiply(NatAge, UnfishedDist)
    
    InitialDepletion <- Hist@OM@Stock[[st]]@Depletion@Initial
    if (length(InitialDepletion) && all(InitialDepletion!=1)) 
      Hist <- DoOptInitialDepletion(Hist, st)
    
  }
  Hist
}


DoOptInitialDepletion <- function(Hist, st) {
  DepletionInitial <- Hist@OM@Stock[[st]]@Depletion@Initial
  DepletionReference <- Hist@OM@Stock[[st]]@Depletion@Reference
 
  if (is.null(DepletionInitial))
    return(Hist)
  
  if (!DepletionReference %in% c('B0', 'SB0'))
    cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
  
  if (DepletionReference == 'B0') {
    # currently using Unfished Equilibrium Biomass from first time step
    RefVal <- abind::adrop(Hist@Unfished@Equilibrium@Biomass[,,1, drop=FALSE], 3) |> ExpandSims(Hist@OM@nSim) |>
      apply(c('Sim', 'Stock'), sum)
  } else {
    RefVal <- abind::adrop(Hist@Unfished@Equilibrium@SBiomass[,,1, drop=FALSE], 3) |> ExpandSims(Hist@OM@nSim) |>
      apply(c('Sim', 'Stock'), sum) 
  }
  RefVal <- RefVal[,st, drop=FALSE] |> abind::adrop(2)
  
  NatAge <- Hist@Number[[st]][,,1,, drop=FALSE] |> abind::adrop(3)
  NumberAtAgeList <- Array2List(apply(NatAge, c('Sim', 'Age'), sum), 1)
  WeightAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock[[st]]@Weight@MeanAtAge[,,1, drop=FALSE],3), 1)
  MaturityAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock[[st]]@Maturity@MeanAtAge[,,1, drop=FALSE],3), 1)
  FecundityAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock[[st]]@Fecundity@MeanAtAge[,,1, drop=FALSE],3), 1)
  
  interval=c(0.01, 10)
  
  dopt <- purrr::pmap(list(
    NumberAtAge=NumberAtAgeList,
    WeightAtAge=WeightAtAgeList,
    MaturityAtAge=MaturityAtAgeList, 
    FecundityAtAge=FecundityAtAgeList, 
    DepletionInitial=as.list(DepletionInitial), 
    RefVal=as.list(RefVal)
  ), \(NumberAtAge, WeightAtAge, MaturityAtAge, FecundityAtAge,
       DepletionInitial, RefVal) {
    
    optimize(OptInitialDepletion,
             interval=c(0.01, 10),
             NumberAtAge=NumberAtAge,
             WeightAtAge=WeightAtAge,
             MaturityAtAge=MaturityAtAge,
             FecundityAtAge=FecundityAtAge,
             DepletionInitial=DepletionInitial,
             DepletionReference=DepletionReference,
             RefVal=RefVal)
  })
  
  
  nAge <- ncol(NatAge)
  nArea <- nArea(Hist@OM)
  
  adjust <- lapply(dopt, '[[', 'minimum') |> 
    unlist() |>
    array(dim=length(dopt), dimnames=list(Sim=1:length(dopt))) |>
    AddDimension("Age") |> AddDimension("Area")
  
  Hist@Number[[st]][,,1,] <- ArrayMultiply(NatAge, adjust)
  Hist
}
    
OptInitialDepletion <- function(par=1, 
                                NumberAtAge,
                                WeightAtAge,
                                MaturityAtAge, 
                                FecundityAtAge, 
                                DepletionInitial, 
                                DepletionReference, 
                                RefVal) {
  NumberAtAge <- par * NumberAtAge
  
  if (DepletionReference == 'B0') {
    val <- sum(NumberAtAge * WeightAtAge)
    ssq <- ((val/RefVal - DepletionInitial)^2)
  }  
  
  if (DepletionReference=='SB0') {
    val <-  sum(NumberAtAge * WeightAtAge * MaturityAtAge)
    ssq <- ((val/RefVal- DepletionInitial)^2)
  } 
  ssq
} 
