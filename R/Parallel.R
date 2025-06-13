define.lapply <- function(silent=FALSE) {
  if (requireNamespace("pbapply", quietly = TRUE) && !silent) {
    .lapply <- pbapply::pblapply   
    # Argument to pass parallel cluster (if running)
    formals(.lapply)$cl <- if (snowfall::sfIsRunning()) snowfall::sfGetCluster() else NULL
    return(.lapply)
  } else if (snowfall::sfIsRunning()) {
    return(snowfall::sfLapply)
  } 
  .lapply <- base::lapply
}


define.sapply <- function(silent=FALSE) {
  if (requireNamespace("pbapply", quietly = TRUE) && !silent) {
    .sapply <- return(pbapply::pbsapply)
    # Argument to pass parallel cluster (if running)
    formals(.sapply)$cl <- if (snowfall::sfIsRunning()) snowfall::sfGetCluster() else NULL
    return(.sapply)
  } else if (snowfall::sfIsRunning()) {
    .lapply <- snowfall::sfLapply
    return(snowfall::sfSapply)
  } 
  base::sapply
}

# define.mapply <- function(silent=FALSE) {
#   if (requireNamespace("pbapply", quietly = TRUE) && !silent) {
#     .mapply <- return(pbapply::.mapply)
#     # Argument to pass parallel cluster (if running)
#     formals(.mapply)$cl <- if (snowfall::sfIsRunning()) snowfall::sfGetCluster() else NULL
#     return(.mapply)
#   } else if (snowfall::sfIsRunning()) {
#     .lapply <- snowfall::sfLapply
#     return(snowfall::sfSapply)
#   } 
#   base::sapply
# }