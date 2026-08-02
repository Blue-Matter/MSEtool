#' Populate Effort Observation Error
#'
#' Populate an [effortobs-class] object by expanding CV, generating
#' stochastic error arrays, and generating multiplicative bias across simulations.
#'
#' @param Effort An [effortobs-class] object.
#' @param nSim Integer. Number of simulation replicates.
#' @param HistYears Integer vector. Calendar years of the historical period.
#' @param ProjYears Integer vector. Calendar years of the projection period.
#'
#' @details
#' If [EmptyObject()] returns `TRUE` for `Effort`, the object is returned
#' unchanged and no population is performed.
#'
#' Otherwise the following slots are populated in order:
#'
#' * `CV`: expanded to a named `[nSim]` array.
#' * `Error`: generated as a lognormal draw from `CV` across all historical
#'   and projection years, producing a named `[nSim x nYear]` array. If `Error` 
#'   is already a fully-formed array it is
#'   validated and dimension names are applied.
#' * `Bias`: expanded to a named `[nSim]` multiplicative bias array.
#'  Defaults to 1 (no bias) if unspecified.
#'
#' @return A populated [effortobs-class] object.
#'
#' @seealso
#' [PopulateObs()], [effortobs-class]
#'
#' @export
PopulateEffortObs <- function(Effort, nSim, HistYears, ProjYears) {
  .CheckClass(Effort, "effortobs", "Effort")

  if (EmptyObject(Effort))
    return(Effort)

  Effort@CV    <- .PopulateObsCV(Effort@CV, nSim)
  Effort@Error <- .PopulateObsError(Effort, nSim, c(HistYears, ProjYears))
  Effort@Bias  <- .PopulateObsBias(Effort, nSim)
  Effort
}
