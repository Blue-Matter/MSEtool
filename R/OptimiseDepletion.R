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
  
  lens <- lapply(FinalDepletion, length) |> unlist()
  
  if (any(lens)>0)
    cli::cli_abort('optimizing catchablity for depletion not done', .internal=TRUE)
  Hist
}


Hist@Fleet[[1]][[1]]@Effort@Catchability <- array(rep(0.1, nSim(Hist)),
                                                  dim=c(nSim(Hist),1),
                                                  dimnames = list(Sim=1:nSim(Hist),
                                                                  `Time Step`=TimeSteps[1])
)



t <- CalcPopDynamics(Hist, TimeSteps=TimeSteps(Hist, 'Historical'))
