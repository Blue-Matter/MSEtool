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
#' Closure <- NULL
#' Closure_pop <- PopulateClosure(Closure, nArea = 5, nSim = 3, Years = 2000:2025)
#'
#' @export
PopulateClosure <- function(Closure, nArea, nSim = 5, Years, silent = FALSE) {
  
  nSim  <- .GetNSim(Closure, nSim)
  
  if (EmptyObject(Closure)) {
    Closure <- array(1,
                     dim = c(1, 1, nArea),
                     dimnames = list(
                       Sim = 1,
                       Year = Years[1],
                       Area = 1:nArea
                     ))
  } else {
    Closure <- .NameSimYearArea(Closure, Years, nArea, 'Closure', 'fleet')
    Problem <- .CheckNum(Closure, 'Closure', 0, 1)
    NotFlag <- !Closure %in% c(0, 1)
    if (is.null(Problem) && any(NotFlag))
      Problem <- paste0('`Closure` must contain only 0 or 1; found ', .FormatVals(Closure[NotFlag]), '.')
    if (!is.null(Problem))
      cli::cli_abort(c("x" = .EscapeBraces(Problem),
                       "i" = "{.code Fleet@Closure} flags each area as open (1) or closed (0). See {.help MSEtool::Fleet}."),
                     call = NULL)
    if (length(dim(Closure)) != 3)
      cli::cli_abort(c("x" = "{.code Fleet@Closure} must be a 3-dimensional array ({.code Sim x Year x Area}).",
                       "i" = "Currently {if (is.null(dim(Closure))) paste('a vector of length', length(Closure)) else paste(length(dim(Closure)), 'dimensions')}. See {.help MSEtool::Fleet}."),
                     call = NULL)
    dd <- dim(Closure)
    if (dd[3] != nArea) {
      if (dd[1] == 1 && dd[2] == 1 && all(Closure == 1)) {
        Closure <- array(1,
                         dim = c(1, 1, nArea),
                         dimnames = list(
                           Sim = 1,
                           Year = Years[1],
                           Area = 1:nArea
                         ))
      } else {
        cli::cli_abort(c("x" = "{.code Fleet@Closure} has {dd[3]} area{?s} but the stock has {nArea} area{?s}.",
                         "i" = "The third ({.code Area}) dimension of {.code Fleet@Closure} must have length {nArea}."),
                       call = NULL)
      }
    }
    dd <- dim(Closure)
    
    # Add dimnames if missing, but preserve existing Year names
    dn <- dimnames(Closure)
    if (is.null(dn)) dn <- list(NULL, NULL, NULL)
    if (is.null(dn[[1]])) dn[[1]] <- 1:dd[1]
    if (is.null(dn[[2]])) dn[[2]] <- Years[1:dd[2]]
    if (is.null(dn[[3]])) dn[[3]] <- 1:dd[3]
    names(dn) <- c('Sim', 'Year', 'Area')
    dimnames(Closure) <- dn
  }
  
  Closure
}
