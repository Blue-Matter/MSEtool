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
  
  FinalDepletion <- purrr::map(Hist@Stock, slot, 'Depletion') |>
    purrr::map(slot, 'Final')
  
  lens <- lapply(FinalDepletion, length) |> unlist()
  
  if (any(lens)>0)
    cli::cli_abort('optimizing catchablity for depletion not done', .internal=TRUE)
  Hist
  
}