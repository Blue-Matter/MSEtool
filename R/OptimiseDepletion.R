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


OptimCatchability <- function(Hist) {
  
  Depletion <- purrr::map(Hist@Stock, slot, 'Depletion') 
  FinalDepletion <- purrr::map(Depletion, slot, 'Final')
  DepletionTarget = FinalDepletion
  
  if (nStock(Hist)>1)
    cli::cli_abort('Optimizing catchability not working for multiple stocks', .internal=TRUE)
  
  if (nFleet(Hist)>1)
    cli::cli_abort('Optimizing catchability not working for multiple fleets', .internal=TRUE)
  
  logQ <- log(rep(0.1, nsim(Hist)))
  bounds <-  c(1e-05, 15)
  doOpt <- optimize(OptCatchability, log(bounds), Hist=Hist, DepletionTarget=DepletionTarget,
                    tol=1e-2)
  logQ <- doOpt$minimum
 
}

OptCatchability <- function(logQ, Hist, DepletionTarget) {
  TimeSteps <- TimeSteps(Hist, 'Historical')
  Hist@Fleet[[1]][[1]]@Effort@Catchability <- array(exp(logQ),
                                                    dim=c(nSim(Hist),1),
                                                    dimnames = list(Sim=1:nSim(Hist),
                                                                    `Time Step`=TimeSteps[1])
  )
  
  
  
  profvis::profvis(
    postHist <- MSEtool:::CalcPopDynamics(Hist, TimeSteps=TimeSteps, silent=T)
  )
  
  # profvis
  # Sub Hist to 1 sim
  # speed up PopDynamics
  
  
 
  
  CurrDepletion <- GetDepletion(postHist)
  ssq <- sum((CurrDepletion[[1]][,1] - DepletionTarget[[1]])^2)
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
