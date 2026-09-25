#' The `tunemp` S4 Class
#'
#' The result of [TuneMP()].
#'
#' @slot MP The tuned management procedure: `MP` with the chosen
#'   configuration and tuning argument set (see [SetMPArgs()]), and a
#'   `Tuning` attribute summarising the tuning.
#' @slot MPName Character. Name of the MP that was tuned.
#' @slot Args Named list of the argument values set in `MP`.
#' @slot Status Character. How the tuning ended: `'boundary'` (at the
#'   constraint boundary), `'interior'` (at a peak of the objective within the
#'   constraints), `'at_bound'` (the objective was still increasing at the
#'   end of the searched range), or `'infeasible'` (no value met the
#'   constraints; the value closest to meeting them is returned).
#' @slot Objective Numeric. Objective value of the tuned MP (`NA` without an
#'   objective).
#' @slot Constraints `data.frame` of each metric's value, thresholds, slack,
#'   and whether the constraint is binding, for the tuned MP.
#' @slot Configs `data.frame` ranking the configurations of the first stage
#'   (empty without `Configs`).
#' @slot History `data.frame` of every evaluated candidate.
#' @slot Validation `data.frame` of the tuned MP's metrics for
#'   `ValidationHist` (empty without it).
#' @slot MSE List of the [mse-class] objects of every evaluation, with
#'   `KeepMSE = TRUE` in [TuneControl()].
#' @slot Settings List of the metrics, [TuneControl()] settings,
#'   `HistWeights`, and `TuneArg`.
#'
#' @seealso [TuneMP()], [TuneTable()]
#' @name tunemp-class
#' @export
setClass('tunemp',
         slots = c(MP          = 'ANY',
                   MPName      = 'character',
                   Args        = 'list',
                   Status      = 'character',
                   Objective   = 'numeric',
                   Constraints = 'data.frame',
                   Configs     = 'data.frame',
                   History     = 'data.frame',
                   Validation  = 'data.frame',
                   MSE         = 'list',
                   Settings    = 'list'))

#' @rdname tunemp-class
#' @param object A `tunemp` object.
#' @export
setMethod('show', 'tunemp', function(object) {
  Arg <- object@Settings$TuneArg
  cli::cli_h3("Tuned MP: {object@MPName}")
  cli::cli_text("{.arg {Arg}} = {signif(object@Args[[Arg]], 5)} ({object@Status})")
  Other <- object@Args[setdiff(names(object@Args), Arg)]
  if (length(Other)) {
    Label <- .TuneConfigLabel(Other)
    cli::cli_text("Configuration: {Label}")
  }
  Tab       <- object@Constraints
  Tab$Value <- signif(Tab$Value, 4)
  Tab$Slack <- signif(Tab$Slack, 3)
  print(Tab, row.names = FALSE)
  if (object@Status == 'infeasible')
    cli::cli_alert_warning("No value of {.arg {Arg}} met every constraint.")
  if (object@Status == 'at_bound')
    cli::cli_alert_warning("The objective was still increasing at the end of the searched range; widen {.arg Interval} in {.fn TuneControl}.")
  invisible(object)
})

#' Tables from a Tuned MP
#'
#' @param x A [tunemp-class] object.
#' @param What Character. `'Constraints'` (default; the tuned MP's metric
#'   values), `'Configs'` (the first-stage ranking of configurations),
#'   `'History'` (every evaluated candidate), or `'Validation'`.
#'
#' @return A `data.frame`.
#' @seealso [TuneMP()]
#' @export
TuneTable <- function(x, What = c('Constraints', 'Configs', 'History', 'Validation')) {
  .CheckClass(x, 'tunemp', 'x')
  What <- match.arg(What)
  slot(x, What)
}
