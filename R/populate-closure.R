#' Populate a Closure Object
#'
#' Populates a `Closure` array for a fleet, ensuring proper dimensions across
#' simulations, years, and spatial areas.
#'
#' @param Closure An array representing fleet-specific area closures, or `NULL.`
#' @param nArea Integer. Number of spatial areas.
#' @param nSim Integer. Number of simulation replicates.
#' @param Years Numeric vector of model years.
#' @param silent Logical; if `TRUE`, suppresses informational messages.
#'
#' @details
#' `PopulateClosure()` ensures the Closure array has dimensions:
#' sim × year × area. If the array is empty, it defaults to 1 (open) for all
#' areas. 
#' 
#' Existing arrays are checked for compatibility with `nArea`. 
#' Dimension names for `Sim`, `Year`, and `Area`
#' are automatically added if missing.
#'
#' @return
#' An array representing fleet area closures with proper dimensions and dimnames.
#'
#' @examples
#' \dontrun{
#' Closure <- NULL
#' Closure_pop <- PopulateClosure(Closure, nArea = 5, nSim = 3, Years = 2000:2025)
#' }
#'
#' @export
PopulateClosure <- function(Closure, nArea, nSim = 5, Years, silent = FALSE) {
  
  nSim  <- Get_nSim(Closure, nSim)
  
  if (EmptyObject(Closure)) {
    Closure <- array(1,
                     dim = c(1, 1, nArea),
                     dimnames = list(
                       Sim = 1,
                       Year = Years[1],
                       Area = 1:nArea
                     ))
  } else {
    dd <- dim(Closure)
    if (dd[3] != nArea) {
      if (dd[1] == 1 && dd[2] == 1) {
        Closure <- array(1,
                         dim = c(1, 1, nArea),
                         dimnames = list(
                           Sim = 1,
                           Year = Years[1],
                           Area = 1:nArea
                         ))
      } else {
        cli::cli_abort("Error in {.val Fleet@Closure}", .internal = TRUE)
      }
    }
    
    # Add dimnames if missing, but preserve existing Year names
    dn <- dimnames(Closure)
    if (is.null(dn)) dn <- list(NULL, NULL, NULL)
    if (is.null(dn[[1]])) dn[[1]] <- 1:dd[1]
    if (is.null(dn[[2]])) dn[[2]] <- Years[1:dd[2]]
    if (is.null(dn[[3]])) dn[[3]] <- 1:dd[3]
    dimnames(Closure) <- dn
  }
  
  Closure
}
