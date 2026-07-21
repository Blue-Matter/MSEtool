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
#' Otherwise the following slots are populated in order:
#'
#' * `CV`: expanded to a named `[nSim]` array via [PopulateObsCV()].
#' * `Error`: generated as a lognormal draw from `CV` across all historical
#'   and projection years, producing a named `[nSim x nYear]` array via
#'   [PopulateObsError()]. If `Error` is already a fully-formed array it is
#'   validated and dimension names are applied.
#' * `Bias`: expanded to a named `[nSim]` multiplicative bias array via
#'   [PopulateObsBias()]. Defaults to 1 (no bias) if unspecified.
#' * `Ref`: generated as a lognormal draw from a CV, producing a named
#'   `[nSim]` reference level error multiplier array via [PopulateObsRef()].
#'
#' @return A populated [catchobs-class] object.
#'
#' @seealso
#' [PopulateObs()], [catchobs-class], [PopulateObsCV()],
#' [PopulateObsError()], [PopulateObsBias()], [PopulateObsRef()]
#'
#' @export
PopulateCatchObs <- function(Catch, nSim, HistYears, ProjYears) {
  .CheckClass(Catch, "catchobs", "Catch")
  
  if (EmptyObject(Catch))
    return(Catch)
  
  Catch@CV    <- PopulateObsCV(Catch@CV, nSim)
  Catch@Error <- PopulateObsError(Catch, nSim, Years = c(HistYears, ProjYears))
  Catch@Bias  <- PopulateObsBias(Catch, nSim)
  Catch@Ref   <- PopulateObsRef(Catch@Ref, nSim)
  Catch
}
