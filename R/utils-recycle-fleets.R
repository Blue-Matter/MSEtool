.RecycleToFleets <- function(x, nFleet, arg_name) {
  if (length(x) == 1L)      return(rep(x, nFleet))
  if (length(x) == nFleet)  return(x)
  cli::cli_abort(
    "{.arg {arg_name}} must be length 1 or {nFleet}, not {length(x)}."
  )
}
