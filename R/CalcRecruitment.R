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
    
    RunSRRfunction <- function(fun, Arglist) {
      dnames <- lapply(Arglist, dimnames)
      SimsList <- lapply(dnames, '[[', 'Sim') 
      MaxSimsList <- lapply(SimsList, as.numeric) |>  lapply(max)
      sims <-  SimsList |> unlist() |> as.numeric() |> unique()
      TSList <- lapply(dnames, '[[', 'Time Step')
      ts <-  TSList |> unlist() |> as.numeric() |> unique()
      
      Recruit <- array(NA, dim=c(length(sims), length(ts)),
                   dimnames = list(Sim=sims,
                                   `Time Step`= ts)
      )
      for (i in seq_along(sims)) {
        for (j in seq_along(ts)) {
          Arglist$S[GetIndex(i, )]
          i
          j
          
        }
      }
    
    }
    
    RecruitEq <- do.call(fun, l)
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
