#' Import a WHAM Model into an Operating Model
#'
#' @description
#' `.ImportWHAM()` is not yet implemented. This is a placeholder documenting
#' why, and what a real implementation needs, so the gap is discoverable
#' rather than silent (see [ImportiSCAM()] and [ImportBAM()] for the
#' equivalent, implemented importers).
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
