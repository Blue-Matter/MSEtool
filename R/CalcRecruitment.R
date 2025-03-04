CalcRecruitment <- function(Hist, TimeStep=NULL) {
  
  SpawnProduction <- GetSProductionAtAge(Hist, TimeSteps=TimeStep)
  Recruits <- vector('list', nStock(Hist))
  names(Recruits) <- StockNames(Hist)
  
  for (st in 1:nStock(Hist)) {
    fun <- Hist@Stock[[st]]@SRR@Model
    
    SRRPars <- purrr::map(Hist@Stock[[st]]@SRR@Pars,
                          ArraySubsetTimeStep, 
                          TimeSteps=TimeStep)
    
    
    S0 <- Hist@Unfished@Equilibrium@SProduction[[st]] |>
      apply(c('Sim', 'Time Step'), sum) |>
      ArraySubsetTimeStep(TimeSteps=TimeStep)
    
    R0 <- GetR0(Hist@Stock[[st]], TimeSteps=TimeStep)
    
    S <- apply(SpawnProduction[[st]], 
               c('Sim', 'Time Step'),
               sum)
    
    Arglist <- c(list(S=S,
                      S0=S0, 
                      R0=R0),
                 SRRPars)
    
    RecruitEq <- RunSRRfunction(fun, Arglist)
    RecDev <- GetRecDevHist(Hist@Stock[[st]], TimeStep)
    Recruit <- ArrayMultiply(RecruitEq, RecDev) |> 
      AddDimension('Age', val=0) |>
      AddDimension('Area') |> 
      aperm(c(1,3,2,4))
    
    R0Dist <- GetUnfishedDist(Hist@Stock[[st]], TimeSteps = TimeStep)
    
    if (!is.null(R0Dist)) {
      # distributr R0
      nArea <- nArea(Hist@Stock[[st]])
      R0Dist <- R0Dist[,,1,1, drop=FALSE] |>
        aperm(c(1,3,4,2))
      Recruit <- ArrayMultiply(Recruit, R0Dist) 
    }
    
    
    ArrayFill(Hist@Number[[st]]) <- Recruit
  }
  Hist
}

#' @export
RunSRRfunction <- function(fun, Arglist) {
  dnames <- lapply(Arglist, dimnames)
  SimsList <- lapply(dnames, '[[', 'Sim') 
  TSList <- lapply(dnames, '[[', 'Time Step')
  Sims <- lapply(SimsList, as.numeric) |> unlist() |> unique() |> sort()
  MaxSims <- length(Sims)
  TSs <- lapply(TSList, as.numeric) |> unlist() |> unique() |> sort()
  MaxTS <- length(TSs)
  
  Recruit <- array(NA, dim=c(MaxSims, MaxTS),
                   dimnames = list(Sim=Sims,
                                   `Time Step`= TSs)
  )
  
  for (sim in 1:MaxSims) {
    for (ts in 1:MaxTS) {
      Sim <- Sims[sim]
      TS <- TSs[ts]
      Arglist2 <- Arglist
      for (arg in seq_along(Arglist2)) {
        Arglist2[[arg]] <- Arglist2[[arg]] |>
          ArraySubsetSim(Sim) |>
          ArraySubsetTimeStep(TS)
      }
      Recruit[sim, ts] <- do.call(fun, Arglist2)
      
    }
  }
  Recruit
}