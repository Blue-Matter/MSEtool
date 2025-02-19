OptimInitDepletion <- function(Hist) {
  # TODO
  # adjust initial rec devs to match initial depletion in first time step
  
  InitialDepletion <- purrr::map(Hist@Stock, slot, 'Depletion') |>
    purrr::map(slot, 'Initial')
  
  lens <- lapply(InitialDepletion, length) |> unlist()
  
  if (any(lens)>0)
    cli::cli_abort('Initial depletion not done', .internal=TRUE)
  Hist
}

SubsetSim <- function(object, Sim=1) {
  # this is a piece of black magic that subsets all arrays with a Sim
  # dimension
  # Sim is a numeric vector, can be length > 1
  # If Sim includes values greater then the length of the `Sim` dimension in
  # an array, the last value in that dimension will be returned
  # All Sim dimension should be length 1 or length nsim, so that shouldn't be 
  # a problem, but you've been warned!
  if (isS4(object)) {
    slots <- slotNames(object)
    for (i in seq_along(slots)) {
      obj <- slot(object, slots[i])
      slot(object, slots[i]) <- Recall(obj, Sim)
    }
    if ('nSim' %in% slotNames(object)) 
      object@nSim <- length(Sim)

    return(object)
  }
  
  if (inherits(object, c('StockList',  'StockFleetList', 'FleetList', 'list'))) {
    outlist <- object 
    for (j in seq_along(object)) {
      if (is.null(object[[j]])) {
        outlist[[j]] <- object[[j]]
      } else {
        outlist[[j]] <- Recall(object[[j]], Sim)
      }
      
    }
    if (length(outlist)<1) 
      outlist <- object
    return(outlist)
  }
  
  if (inherits(object, 'array')) {
    if ("Sim" %in% names(dimnames(object))) {
      object <- ArraySubsetSim(object, Sim)
    }
    return(object)
  }
  
  object
}

OptimCatchability <- function(Hist) {
  
  Depletion <- purrr::map(Hist@Stock, slot, 'Depletion') 
  DepletionTarget <- purrr::map(Depletion, slot, 'Final')

  if (length(DepletionTarget[[1]])<1)
    return(Hist)
  
  if (nStock(Hist)>1)
    cli::cli_abort('Optimizing catchability not working for multiple stocks', .internal=TRUE)
  
  if (nFleet(Hist)>1)
    cli::cli_abort('Optimizing catchability not working for multiple fleets', .internal=TRUE)
  
  nsims <- dim(Hist@Number[[1]][,,1,1, drop=FALSE])[1]
  bounds <-  c(1e-03, 15)
  
  Qvals <- rep(NA, nsims)
 
  for (sim in 1:nsims) {
    tictoc::tic()
    SubHist <- SubsetSim(Hist, sim)
  
    doOpt <- optimize(OptCatchability,
                      log(bounds), 
                      Hist=SubHist, 
                      sim=sim,
                      tol=1e-3)
    
    Qvals[sim] <- exp(doOpt$minimum)
    tictoc::toc()
  }
 
  Hist@Fleet[[1]][[1]]@Effort@Catchability <- array(Qvals,
                                                    dim=c(nsims,1),
                                                    dimnames = list(Sim=1:nsims,
                                                                    `Time Step`=TimeSteps(Hist, 'Historical')[1]))
  Hist
}

OptCatchability <- function(logQ, Hist, DepletionTarget, sim=1) {
  
  TimeSteps <- TimeSteps(Hist, 'Historical')
  DepletionTarget <- Hist@Stock[[1]]@Depletion@Final[sim]
  
  Hist@Fleet[[1]][[1]]@Effort@Catchability <- array(exp(logQ),
                                                    dim=c(1,1),
                                                    dimnames = list(Sim=sim,
                                                                    `Time Step`=TimeSteps[1])
  )
  postHist <- CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist,'Historical'), silent=TRUE)
  CurrDepletion <- GetDepletion(postHist)
  ssq <- sum((CurrDepletion[[1]][1,1] - DepletionTarget)^2)
  ssq
}


GetDepletion <- function(Hist, TimeSteps=NULL, Reference=NULL) {
  if (is.null(TimeSteps))
    TimeSteps <- TimeSteps(Hist, 'Historical') |> tail(1)
  
  Biomass <- GetBiomassAtAge(Hist, TimeSteps) |>
    purrr::map(\(x) apply(x, c('Sim', 'Time Step'), sum))
  
  Unfished <- purrr::map(Hist@Unfished@Equilibrium@Biomass, \(x) {
    ArraySubsetTimeStep(x,TimeSteps)
  }) |>
    purrr::map(\(x) apply(x, c('Sim', 'Time Step'), sum))
    
  purrr::map2(Biomass, Unfished, ArrayDivide)

}
