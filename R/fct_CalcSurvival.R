CalcSurvival <- function(NaturalMortalityAtAge, # Sim, nAge, nTS (Sim optional)
                         FishingMortalityAtAge=NULL, # Sim, nAge, nTS (Sim optional)
                         PlusGroup=TRUE, # logical 
                         SpawnTimeFrac=NULL, # double (can be length Sim)  
                         Semelparous=FALSE) { # FALSE or array Sim, nAge, nTS  (Sim optional)
  
  
  TotalMortalityAtAge <- NaturalMortalityAtAge
  if (!is.null(FishingMortalityAtAge))
    TotalMortalityAtAge <- ArrayAdd(NaturalMortalityAtAge, FishingMortalityAtAge)
  
  BySim <- "Sim" %in% names(dimnames(NaturalMortalityAtAge))
  survival <- TotalMortalityAtAge
  survival[] <- 0
  
  AgeInd <- which(names(dimnames(NaturalMortalityAtAge)) == 'Age')
  nAge <- dim(NaturalMortalityAtAge)[AgeInd]

  if (is.null(SpawnTimeFrac)) 
    SpawnTimeFrac <- 0

  if (inherits(Semelparous, 'logical')) {
    Semelparous <- NaturalMortalityAtAge[,1]
    Semelparous[] <- 0
  }
  
  if (BySim) {
    nSim <- dim(TotalMortalityAtAge)[1]
    nTS <- dim(TotalMortalityAtAge)[3]
    if (length(SpawnTimeFrac) != nSim) {
      SpawnTimeFrac <- rep(SpawnTimeFrac, nSim)[1:nSim]
      SpawnTimeFrac <- matrix(SpawnTimeFrac, nSim, nTS, byrow=FALSE)
    }
      
    Semelparous <- Semelparous |> ExpandSims(nSim)

    for (a in 1:nAge) {
      ZthisAge <- TotalMortalityAtAge[,a,]
      if (a==1) {
        survival[,a,] <- exp(-ZthisAge*SpawnTimeFrac)
      } else {
        ZlastAge <- TotalMortalityAtAge[,a-1,]
        PostSpawnMortalityLastAge <- Semelparous[,a-1,]
        survival[,a,] <- survival[,a-1,]*
          exp(-(ZlastAge*(1-SpawnTimeFrac)+ZthisAge*SpawnTimeFrac)) *
          (1-PostSpawnMortalityLastAge)
      }
    }
    
    if (PlusGroup)
      survival[,nAge,] <- survival[,nAge,]/(1-exp(-TotalMortalityAtAge[,nAge,]))
    
    return(survival)
  } 
  
  
  for (a in 1:nAge) {
    ZthisAge <- TotalMortalityAtAge[a,]
    if (a==1) {
      survival[a,] <- exp(-ZthisAge*SpawnTimeFrac)
    } else {
      ZlastAge <- TotalMortalityAtAge[a-1,]
      PostSpawnMortalityLastAge <- Semelparous[a-1,]
      survival[a,] <- survival[a-1,]*
        exp(-(ZlastAge*(1-SpawnTimeFrac)+ZthisAge*SpawnTimeFrac)) *
        (1-PostSpawnMortalityLastAge)
    }
  }
  if (PlusGroup)
    survival[nAge,] <- survival[nAge,]/(1-exp(-TotalMortalityAtAge[nAge,]))
  
  survival 
}


# # ---- CalcUnfishedSurvival -----
CalcUnfishedSurvival <- function(OM, SP=FALSE, TimeSteps=NULL, silent=FALSE) {
  
  if (inherits(OM,'stock'))
    return(CalcUnfishedSurvivalStock(OM, SP, TimeSteps))
  
  if (inherits(OM,'list'))
    return(CalcUnfishedSurvivalStockList(OM, SP, TimeSteps))
  
  OM <- PopulateOM(OM, silent)
  if (is.null(TimeSteps))
    TimeSteps <- OM@TimeSteps
  
  CalcUnfishedSurvivalStockList(OM@Stock, SP, TimeSteps)
  
}

CalcUnfishedSurvivalStockList <- function(StockList, SP=FALSE, TimeSteps=NULL) {
  purrr::map(StockList, \(Stock) CalcUnfishedSurvivalStock(Stock, SP, TimeSteps))
}

CalcUnfishedSurvivalStock <- function(Stock, SP=FALSE, TimeSteps=NULL) {
  nAges <- Stock@Ages@Classes |> length()
  NaturalMortalityAtAge <- Stock@NaturalMortality@MeanAtAge |>
    ArrayExpand(Stock@nSim, nAges, TimeSteps) |>
    ArraySubsetTimeStep(TimeSteps)
  
  PlusGroup <- Stock@Ages@PlusGroup
  SpawnTimeFrac <- ifelse(SP, Stock@SRR@SpawnTimeFrac, 0)
  Semelparous <- Stock@Maturity@Semelparous |> ArrayExpand(Stock@nSim, nAges, TimeSteps)
  
  IsIdenticalTime <- all(IdenticalTimeSteps(NaturalMortalityAtAge) & IdenticalTimeSteps(Semelparous))
  IsIdenticalSim <- all(IdenticalSims(NaturalMortalityAtAge) & IdenticalSims(Semelparous))
  
  BySim <- 'Sim' %in% names(dimnames(NaturalMortalityAtAge))
  

  if (!BySim) {
    if (IsIdenticalTime) {
      NaturalMortalityAtAge <- NaturalMortalityAtAge[,1, drop=FALSE]
      Semelparous <- Semelparous[,1, drop=FALSE]
      Survival <- CalcSurvival(NaturalMortalityAtAge,
                               PlusGroup=PlusGroup,
                               SpawnTimeFrac=SpawnTimeFrac,
                               Semelparous=Semelparous)
      dnames <- dimnames(Survival)
      nTS <- length(TimeSteps)
      
      SurvivalList <- replicate(nTS, Survival, simplify = FALSE)
      
      Survival <- abind::abind(SurvivalList, along=2)
      dimnames(Survival) <- list(Age=dnames[[1]],
                                 TimeStep=TimeSteps)
      
      return(Survival)
      
    } else {
      Survival <- CalcSurvival(NaturalMortalityAtAge,
                               PlusGroup=PlusGroup,
                               SpawnTimeFrac=SpawnTimeFrac,
                               Semelparous=Semelparous)
      return(Survival)
    }
    
  }
  
  if (IsIdenticalSim & IsIdenticalTime) {
    NaturalMortalityAtAge <- abind::adrop(NaturalMortalityAtAge[1,,1, drop=FALSE], 1)
    Semelparous <- abind::adrop(Semelparous[1,,1, drop=FALSE], 1)
    Survival <- CalcSurvival(NaturalMortalityAtAge,
                             PlusGroup=PlusGroup,
                             SpawnTimeFrac=SpawnTimeFrac,
                             Semelparous=Semelparous
    )
    Survival <- replicate(1, Survival) |>
      AddDimNames(c('Age', 'TimeStep', 'Sim'), TimeSteps) |>
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  } else if (IsIdenticalSim & !IsIdenticalTime) {
    NaturalMortalityAtAge <- abind::adrop(NaturalMortalityAtAge[1,,, drop=FALSE], 1)
    Semelparous <- abind::adrop(Semelparous[1,,, drop=FALSE], 1)
    Survival <- CalcSurvival(NaturalMortalityAtAge,
                             PlusGroup=PlusGroup,
                             SpawnTimeFrac=SpawnTimeFrac,
                             Semelparous=Semelparous
    )
    Survival <- replicate(1, Survival) |>
      AddDimNames(c('Age', 'TimeStep', 'Sim'), TimeSteps) |>
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  } else if (!IsIdenticalSim & IsIdenticalTime) {
    NaturalMortalityAtAgeList <- Array2List(NaturalMortalityAtAge[,,1, drop=FALSE], 1)
    SemelparousList <- Array2List(Semelparous[,,1, drop=FALSE], 1)
    Survival <- purrr::pmap(list(
      NaturalMortalityAtAge=NaturalMortalityAtAgeList,
      PlusGroup=PlusGroup,
      SpawnTimeFrac=SpawnTimeFrac,
      Semelparous=SemelparousList),
      CalcSurvival
    ) |> List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  } else {
    NaturalMortalityAtAgeList <- Array2List(NaturalMortalityAtAge, 1)
    SemelparousList <- Array2List(Semelparous, 1)
    Survival <- purrr::pmap(list(
      NaturalMortalityAtAge=NaturalMortalityAtAgeList,
      PlusGroup=PlusGroup,
      Semelparous=SemelparousList),
      CalcSurvival, SpawnTimeFrac=SpawnTimeFrac) |>
      List2Array('Sim') |>
      aperm(c('Sim', 'Age', 'TimeStep'))
    
  }
  Survival |> ArrayExpand(Stock@nSim, nAges, TimeSteps)
  
}


  

