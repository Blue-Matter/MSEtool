#' Populate Index Observation Error
#'
#' Populate an [indicesobs-class] object by expanding CV, generating
#' stochastic error arrays, applying lag-1 autocorrelation, applying
#' multiplicative bias, and populating index years, across simulation
#' replicates and years.
#'
#' @param Index An [indicesobs-class] object. Used for both CPUE indices
#'   (`Obs@CPUE`) and fishery-independent survey indices (`Obs@Survey`).
#' @param nSim Integer. Number of simulation replicates.
#' @param HistYears Integer vector. Calendar years of the historical period.
#' @param ProjYears Integer vector. Calendar years of the projection period.
#'
#' @details
#' If [EmptyObject()] returns `TRUE` for `Index`, the object is returned
#' unchanged and no population is performed.
#'
#' Otherwise the following slots are populated in order:
#'
#' * `CV` — expanded to a named `[nSim]` array via [PopulateObsCV()].
#' * `Error` — generated as a lognormal draw from `CV` across all historical
#'   and projection years, producing a named `[nSim x nYear]` array via
#'   [PopulateObsError()]. If `Error` is already a fully-formed array it is
#'   validated and dimension names are applied. If `AC` is non-zero, the
#'   error series is subsequently transformed to an AR(1) process (see below).
#' * `AC` — expanded to a named `[nSim]` array via [PopulateObsAC()].
#'   Accepted inputs: scalar applied to all simulations, length-2 vector
#'   `c(lower, upper)` for a per-simulation Uniform draw, or a vector of
#'   length `nSim` used directly. Defaults to 0 (no autocorrelation).
#' * `Ref` — generated as a lognormal draw from a CV, producing a named
#'   `[nSim]` reference level error array via [PopulateObsRef()].
#' * `Years` — if empty, defaults to all historical years (`HistYears`).
#'
#' The following slots are used internally during the simulation and are not
#' populated here: `Beta`, `Selectivity`, `Type`, `Efficiency`.
#'
#' ## Autocorrelated index error
#'
#' When `AC` is non-zero, the independently-drawn lognormal error series is
#' transformed into a variance-preserving AR(1) process on the log scale.
#' Let \eqn{\varepsilon_t = \log(\mathrm{Error}_t)} be the raw log-errors
#' drawn from \eqn{N(\mu, \sigma^2)} where \eqn{\mu} and \eqn{\sigma} are
#' derived from `CV`. The AR(1) series is constructed as:
#'
#' \deqn{\varepsilon_t^* = \rho \, \varepsilon_{t-1}^* +
#'   \sqrt{1 - \rho^2} \, \varepsilon_t}
#'
#' where \eqn{\rho} is the per-simulation lag-1 autocorrelation coefficient
#' (`AC`). This formulation preserves the marginal variance
#' \eqn{\mathrm{Var}(\varepsilon_t^*) = \sigma^2} for all values of
#' \eqn{\rho}. The AR(1) series is then exponentiated to return to the
#' natural scale: \eqn{\mathrm{Error}_t^* = \exp(\varepsilon_t^*)}.
#'
#' @return A populated [indicesobs-class] object.
#'
#' @seealso
#' [PopulateObs()], [indicesobs-class], [PopulateObsCV()],
#' [PopulateObsError()], [PopulateObsAC()], 
#' [PopulateObsRef()]
#'
#' @export
PopulateIndexObs <- function(Index, nSim, HistYears, ProjYears) {
  .CheckClass(Index, "indicesobs", "Index")
  
  if (EmptyObject(Index))
    return(Index)
  
  Years <- c(HistYears, ProjYears)
  
  Index@CV    <- PopulateObsCV(Index@CV, nSim)
  Index@Error <- PopulateObsError(Index, nSim, Years)
  Index@AC    <- PopulateObsAC(Index@AC, nSim)
  Index@Error <- ApplyObsAC(Index@Error, Index@AC)
  Index@Ref   <- PopulateObsRef(Index@Ref, nSim)
  
  if (length(Index@Years) < 1)
    Index@Years <- HistYears
  
  # TODO: Beta, Selectivity, Type, Efficiency — used internally, not populated here
  Index
}
