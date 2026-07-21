#' Import a WHAM Model into an Operating Model
#'
#' @description
#' `.ImportWHAM()` is not yet implemented. This is a placeholder documenting
#' why, and what a real implementation needs, so the gap is discoverable
#' rather than silent (see [ImportiSCAM()] and [ImportBAM()] for the
#' equivalent, implemented importers).
#'
#' @details
#' Unlike iSCAM ([ImportiSCAM()], which reuses the legacy [iSCAM2OM()]'s
#' file parser `load.iscam.files()` and a well-documented field mapping),
#' WHAM's own legacy importer ([WHAM2OM()]) has real gaps that make it
#' unsafe to port under a new name without further work:
#'
#' - It requires an in-memory **fitted WHAM/TMB model object** (`wham::fit_wham()`'s
#'   return value), not raw output files -- there is no file-based reader to
#'   reuse, and no `wham`/`TMB`/`mvtnorm` dependency is declared in this
#'   package's `DESCRIPTION`.
#' - It hard-codes fleet index 1 (`output[[x]]$FAA[,1,]`) -- multi-fleet
#'   models are not actually supported despite fleet selectivity being
#'   sampled.
#' - Recruitment/steepness handling only covers `recruit_model %in% c(1,2)`;
#'   other WHAM recruitment models hit an explicit, unimplemented `stop()`.
#' - There is no `WHAM2Data()` counterpart at all -- no observed-data import
#'   path to model a new `ImportWHAMData()` on.
#'
#' Building `.ImportWHAM()` responsibly needs either a real fitted WHAM
#' object to test against, or someone with WHAM/TMB domain knowledge to
#' validate the field mapping -- neither was available while this file was
#' written. Treat this as a scoped-out follow-up, not a deferred detail.
#'
#' @param obj A fitted WHAM/TMB model object (`wham::fit_wham()`'s return
#'   value). Not currently supported.
#' @param ... Not currently used.
#'
#' @return Errors; see Details.
#' @seealso [ImportiSCAM()], [ImportBAM()], [WHAM2OM()] (legacy, single-fleet
#'   only)
#' @keywords internal
.ImportWHAM <- function(obj, ...) {
  cli::cli_abort(c(
    "x" = "`.ImportWHAM()` is not yet implemented.",
    "i" = "See {.help MSEtool::.ImportWHAM} for why, and what's needed to build it.",
    "i" = "The legacy {.fn WHAM2OM} (single-fleet only, limited recruitment models) may work for your case."
  ), call = NULL)
}
