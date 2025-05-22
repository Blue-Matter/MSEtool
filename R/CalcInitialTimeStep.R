
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

CalcInitialTimeStep <- function(Hist, silent=FALSE) {
  if (inherits(Hist, 'om')) {
    Hist <- Hist(Hist, silent)
  } 
  if (is.null(Hist@Unfished@Equilibrium@Number))
    Hist@Unfished@Equilibrium <- CalcEquilibriumUnfished(OM)
  
  UnfishedDist <- Hist@OM@Stock@Spatial@UnfishedDist
  dd <- dim(UnfishedDist)
  nSim <- dd[1]
  nStock <- dd[2]
  nAge <- dd[3]
  nArea <- dd[5]
  
  NumberAtAgeAreaInitial <- array(0, c(nSim, nStock, nAge, nArea))
  
  # Dynamic Initial Number at Age Area
  for (st in 1:nStock) {
    EquilNumber <- abind::adrop(Hist@Unfished@Equilibrium@Number[[st]][,,1, drop=FALSE], 3) |>
      ExpandSims(nSim)
    DynNumber <- EquilNumber
    
    RecDevInit <- Hist@OM@Stock@SRR@RecDevInit[[st]] |>
      ExpandSims(nSim)
    RecDevHist <- abind::adrop(Hist@OM@Stock@SRR@RecDevHist[,st,, drop=FALSE], 2)
    RecDevHist1 <- RecDevHist[,1, drop=FALSE] |>
      ExpandSims(nSim)
    names(dimnames(RecDevHist1))[2] <- 'Age'
    dimnames(RecDevHist1)[[2]]
    
    ages <- as.numeric(dimnames(RecDevInit)[['Age']])
    InitAgeClassRecDevs <- cbind(RecDevHist1, RecDevInit) 
    dimnames(InitAgeClassRecDevs) <- list(Sim=1:nSim,
                                          Age=c(ages[1]-1, ages))
    
    NatAge <- ArrayMultiply(InitAgeClassRecDevs, EquilNumber) |>
      AddDimension('Area') 
    
    UnfishedDist <- abind::adrop(Hist@OM@Stock@Spatial@UnfishedDist[,st,,1,,drop=FALSE], c(2,4)) 
    
    NatAge <- ArrayMultiply(NatAge, UnfishedDist)
    
    if (!is.null(Hist@OM@Stock@Depletion@Initial)) {
      DepletionInitial <- Hist@OM@Stock@Depletion@Initial[st]
      DepletionReference <- Hist@OM@Stock@Depletion@Reference[st]
      
      if (!DepletionReference %in% c('B0', 'SB0'))
        cli::cli_abort("Currently only accepts `Depletion@Reference = 'B0' or 'SB0'")
      
      if (DepletionReference == 'B0') {
        # currently using Unfished Equilibrium Biomass from first time step
        RefVal <- abind::adrop(Hist@Unfished@Equilibrium@Biomass[,,1, drop=FALSE], 3) |> ExpandSims(nSim) |>
          apply(c('Sim', 'Stock'), sum)
      } else {
        RefVal <- abind::adrop(Hist@Unfished@Equilibrium@SBiomass[,,1, drop=FALSE], 3) |> ExpandSims(nSim) |>
          apply(c('Sim', 'Stock'), sum) 
      }
      RefVal <- RefVal[,st, drop=FALSE] |> abind::adrop(2)
      if (!is.null(DepletionInitial)) {
        
        NumberAtAgeList <- Array2List(apply(NatAge, c('Sim', 'Age'), sum), 1)
        WeightAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock@Weight@MeanAtAge[,st,,1, drop=FALSE],c(2,4)), 1)
        MaturityAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock@Maturity@MeanAtAge[,st,,1, drop=FALSE],c(2,4)), 1)
        FecundityAtAgeList <- Array2List(abind::adrop(Hist@OM@Stock@Fecundity@MeanAtAge[,st,,1, drop=FALSE],c(2,4)), 1)
        
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
        adjust <- lapply(dopt, '[[', 'minimum') |> unlist()
        adjust <- replicate(nAge, adjust)
        adjust <- replicate(nArea, adjust)
        NatAge <- NatAge * adjust
      }
    }
    
    NumberAtAgeAreaInitial[,st,,] <- NatAge
  }
  
  NumberAtAgeAreaInitial
}

