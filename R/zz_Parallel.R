#' Is a snowfall (legacy engine) cluster running?
#'
#' Safe wrapper around `snowfall::sfIsRunning()` for use in the legacy engine,
#' now that `snowfall` is an optional (`Suggests`) dependency: returns `FALSE`
#' if `snowfall` isn't installed rather than erroring, so sequential legacy
#' runs work without it.
#' @keywords internal
.sfIsRunning <- function() {
  requireNamespace('snowfall', quietly = TRUE) && snowfall::sfIsRunning()
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
