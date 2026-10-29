#' Import a WHAM Model into an Operating Model
#'
#' `ImportWHAM()` is not yet implemented.
#' 
#' @param obj A fitted WHAM/TMB model object (`wham::fit_wham()`'s return
#'   value). Not currently supported.
#' @param ... Not currently used.
#'
#' @return Errors; see Details.
#' @seealso [ImportSS()], [ImportBAM()], [WHAM2OM()] (legacy, single-fleet
#'   only)
#' @export
ImportWHAM <- function(obj, ...) {
  cli::cli_abort(c(
    "x" = "`.ImportWHAM()` is not yet implemented.",
    "i" = "The legacy {.fn WHAM2OM} (single-fleet only, limited recruitment models) may work for your case."
  ), call = NULL)
}
