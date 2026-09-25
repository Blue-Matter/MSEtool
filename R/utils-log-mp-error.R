#' Check and log errors from a Management Procedure
#'
#' Internal helper function to standardize error handling for Management Procedures (MPs).
#' If the MP returns a valid `Advice` object, it is returned unchanged. If the MP returns a
#' different type or a `try-error`, a short error message is returned for the log.
#'
#' @param Advice The result returned by the MP. Should be of class `advice`.
#' @param MPName Character. Name of the Management Procedure.
#' @param Data An object containing the simulation data (typically an OM or Data object).
#' @param Sim Integer. Simulation number.
#' @param Year Integer. Current simulation year.
#'
#' @return
#' `Advice` if it is a valid advice object; otherwise a character error message.
#'
#' @keywords internal
.LogMPError <- function(Advice, MPName, Data, Sim, Year) {
  if (inherits(Advice, 'advice')) return(Advice)

  # MP, sim, and year are recorded as log entry tags, not in the message
  if (!inherits(Advice, 'try-error'))
    return("MP did not return an `Advice()` object")

  cond <- attr(Advice, 'condition')
  msg  <- if (inherits(cond, 'condition')) conditionMessage(cond) else trimws(as.character(Advice))
  paste0("MP error: ", msg)
}
