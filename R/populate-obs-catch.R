#' Populate Catch Observation Error
#'
#' Populate a [catchobs-class] object by expanding CV, generating stochastic
#' error arrays, applying multiplicative bias, and generating a stochastic
#' reference level, all across simulation replicates and years.
#'
#' @param Catch A [catchobs-class] object. Used for both landed catch
#'   (`Obs@Landings`) and discarded catch (`Obs@Discards`).
#' @param nSim Integer. Number of simulation replicates.
#' @param HistYears Integer vector. Calendar years of the historical period.
#' @param ProjYears Integer vector. Calendar years of the projection period.
#'
#' @details
#' If [EmptyObject()] returns `TRUE` for `Catch`, the object is returned
#' unchanged and no population is performed.
#'
#' @return A populated [catchobs-class] object.
#'
#' @seealso
#' [PopulateObs()], [catchobs-class]
#'
#' @export
PopulateCatchObs <- function(Catch, nSim, HistYears, ProjYears) {
  .CheckClass(Catch, "catchobs", "Catch")

  if (EmptyObject(Catch))
    return(Catch)

  Catch@CV    <- .PopulateObsCV(Catch@CV, nSim)
  Catch@Error <- .PopulateObsError(Catch, nSim, Years = c(HistYears, ProjYears))
  Catch@Bias  <- .PopulateObsBias(Catch, nSim)
  Catch@Ref   <- .PopulateObsRef(Catch@Ref, nSim)
  Catch
}
