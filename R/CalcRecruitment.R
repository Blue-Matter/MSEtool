CalcRecruitment <- function(Hist, TimeStep=NULL) {

  SpawnProduction <- Hist@SProduction |> 
    ArraySubsetTimeStep(TimeSteps=TimeStep)
  
  Recruits <- vector('list', nStock(Hist@OM))
  names(Recruits) <- StockNames(Hist@OM)

  for (st in 1:nStock(Hist)) {
    fun <- Hist@OM@Stock[[st]]@SRR@Model

    SRRPars <- purrr::map(Hist@OM@Stock[[st]]@SRR@Pars,
                          ArraySubsetTimeStep,
                          TimeSteps=TimeStep)


    S0 <- Hist@Unfished@Equilibrium@SProduction[,st,,drop=FALSE] |>
      apply(c('Sim', 'TimeStep'), sum) |>
      ArraySubsetTimeStep(TimeSteps=TimeStep)

    R0 <- Hist@OM@Stock[[st]]@SRR@R0 |>
      ArraySubsetTimeStep(TimeSteps=TimeStep)

    S <- apply(SpawnProduction[,st,, drop=FALSE],
               c('Sim', 'TimeStep'),
               sum)

    Arglist <- c(list(S=S,
                      S0=S0,
                      R0=R0),
                 SRRPars)

    RecruitEq <- RunSRRfunction(fun, Arglist)
    RecDev <- Hist@OM@Stock[[st]]@SRR@RecDevHist |> 
      ArraySubsetTimeStep(TimeSteps=TimeStep)
    
    Recruit <- ArrayMultiply(RecruitEq, RecDev) |>
      AddDimension('Age', val=0) |>
      AddDimension('Area') |>
      aperm(c(1,3,2,4))

    R0Dist <- Hist@OM@Stock[[st]]@Spatial@UnfishedDist |>
      ArraySubsetTimeStep(TimeSteps=TimeStep)

    if (!is.null(R0Dist)) {
      # distributr R0
      nArea <- nArea(Hist@OM@Stock[[st]])
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
  TSList <- lapply(dnames, '[[', 'TimeStep')
  Sims <- lapply(SimsList, as.numeric) |> unlist() |> unique() |> sort()
  MaxSims <- length(Sims)
  TSs <- lapply(TSList, as.numeric) |> unlist() |> unique() |> sort()
  MaxTS <- length(TSs)
  
  Recruit <- array(NA, dim=c(MaxSims, MaxTS),
                   dimnames = list(Sim=Sims,
                                   TimeStep= TSs)
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