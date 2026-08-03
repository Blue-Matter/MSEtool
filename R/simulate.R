#' Simulate Operating Model Dynamics
#'
#' Simulate historical fishery dynamics and calculate reference points for an
#' [om-class] or [OM-legacy-class] object. Returns a populated [hist-class]
#' or [Hist-class] object containing historical population dynamics, fishery
#' data, and reference points.
#'
#' @param OM An [om-class] or [OM-legacy-class] object. If `NULL` (default),
#'   `MSEtool::SingleStockOM` is used.
#' @param parallel Logical. Use parallel processing (across simulations) for
#'   MSY reference point calculation (see [CalcMSY()]), final-depletion
#'   catchability calibration, reference yield calculation, and historical
#'   data generation? Requires a `future` plan established by
#'   [SetupParallel()] -- errors if `TRUE` and no plan is active. Only used
#'   for [om-class] objects. Default `FALSE`.
#' @param silent Logical. Suppress progress messages if `TRUE`. Default
#'   `FALSE`.
#' @param nSim Integer. Number of simulation replicates. If `NULL` (default),
#'   the value in `OM` is used. Only used for [om-class] objects; use `nsim`
#'   for [OM-legacy-class] objects.
#' @param nsim Integer. Synonym for `nSim` for [OM-legacy-class] objects. If
#'   `NULL` (default), `nSim` is used if provided.
#' @param control A [SimControl()] object controlling which optional
#'   calculations are performed. Only used for [om-class] objects. Default
#'   `SimControl()`.
#' @param Reduce Logical. Reduce object size after simulation for memory
#'   efficiency? Default `TRUE`. See [ReduceDims()].
#' @param refpointsMSY A pre-calculated [refpointsMSY-class] object (e.g.
#'   from a previous [Simulate()] or [CalcMSY()] call), or `NULL` (default).
#'   If supplied, it is used directly as `Hist@Reference@MSY` and the
#'   (expensive) MSY reference point calculation is skipped -- `control`'s
#'   `MSYRefs` setting is ignored in this case. Only valid if it was
#'   calculated for an `OM` with the same `nSim`, stock/complex names, and
#'   reference year as the `OM` supplied here; an incompatible object throws
#'   an error rather than being silently ignored. Only used for [om-class]
#'   objects.
#' @param ... Additional arguments passed to sub-functions. Not currently
#'   used
#'
#' @details
#' `Simulate()` is a dispatcher that calls internal functions `.SimulateOM()` (for
#' [om-class] objects) or `SimulateOM()` (for legacy [OM-legacy-class]
#' objects) depending on the class of `OM`.
#'
#' ## `OM@Control`
#'
#' Unlike `control` (`SimControl()`, above), which only toggles which
#' optional quantities are computed, `OM@Control` holds settings that affect
#' simulated dynamics themselves:
#' - `MSYType`: `'Removals'` (default) or `'Landings'`. Passed as `type` to
#'   [CalcMSY()] and [CalcRefPoints()]. Whether MSY-based and other
#'   fishing-mortality reference points are defined in terms of total
#'   removals (landings + dead discards) or landings only.
#' - `RefYears`: Integer vector or `NULL` (default). Passed as `Years` to
#'   [CalcMSY()] and [CalcRefPoints()]. Which year(s) of biological/fishery
#'   parameters reference points are evaluated at. `NULL` uses the final
#'   historical year.
#' - `CorrelatedRecDevs`: `logical(1)`, default `TRUE`. For multi-stock
#'   `OM`s, whether projection recruitment deviations are correlated across
#'   stocks based on historical covariance; see [GenMultiStockRecDevs()].
#'
#' @return
#' - If `OM` is an [om-class] object: a [hist-class] object.
#' - If `OM` is an [OM-legacy-class] object: a [Hist-legacy-class] object.
#'
#' @seealso [OM()], [runMSE()]
#'
#' @examples
#' \dontrun{
#' # Simulate using the default example OM
#' hist <- Simulate()
#'
#' # Simulate with specific options
#' hist <- Simulate(
#'   OM      = MyOM,
#'   control = SimControl(RefLandings = TRUE, GenerateData = FALSE),
#'   silent  = TRUE
#' )
#' }
#'
#' @export
Simulate <- function(OM           = NULL,
                     parallel     = FALSE,
                     silent       = FALSE,
                     nSim         = NULL,
                     nsim         = NULL,
                     control      = SimControl(),
                     Reduce       = TRUE,
                     refpointsMSY = NULL,
                     ...) {

  if (is.null(OM))
    OM <- MSEtool::SingleStockOM

  if (inherits(OM, 'om'))
    return(
      .SimulateOM(OM           = OM,
                  parallel     = parallel,
                  silent       = silent,
                  nSim         = nSim,
                  control      = control,
                  Reduce       = Reduce,
                  refpointsMSY = refpointsMSY,
                  ...)
    )

  if (!is.null(nSim) && is.null(nsim))
    nsim <- nSim

  SimulateOM(OM       = OM,
             parallel = parallel,
             silent   = silent,
             nsim     = nsim)
}
