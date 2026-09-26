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
#'   `ValidationHist` (empty without it). See [TuneTable()] for the columns
#'   of these four tables.
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
#' Returns one of the tables stored in a [tunemp-class] object: the
#' performance of the tuned MP, the ranking of candidate configurations, the
#' record of every evaluated candidate, or the performance of the tuned MP on
#' the validation operating models.
#'
#' Metric values are combined across stocks and `Hist` objects as specified
#' in [TuneObjective()] and [TuneConstraint()]. With `'worst'`, the reported
#' value is that of the stock or `Hist` with the smallest slack. The slack of
#' a constraint is `(Value - Min) / |Min|` and/or `(Max - Value) / |Max|` (the
#' smaller of the two); a negative slack means the constraint is not met.
#'
#' ## `'Constraints'`
#'
#' One row per metric (the objective first, then each constraint), for the
#' tuned MP. Also printed by `show()`.
#' - `Name`: name of the metric (`Name` in [TuneObjective()] or
#'   [TuneConstraint()]).
#' - `Type`: `'objective'` or `'constraint'`.
#' - `Value`: value of the metric.
#' - `Min`, `Max`: thresholds of the constraint (`NA` when not set).
#' - `Slack`: slack of the constraint (`NA` for the objective).
#' - `Binding`: `TRUE` for constraints with an absolute slack no greater than
#'   `max(TolPM, 0.01)` (see [TuneControl()]), i.e. the constraints that limit
#'   the tuned value.
#'
#' ## `'Configs'`
#'
#' One row per configuration in `Configs` (an empty data frame without
#' `Configs`, or with a single configuration). `Value` to `Slack` are from
#' the first stage, the `Tuned` columns from the second stage.
#' - `Config`: index of the configuration. Matches `Config` in `'History'`.
#' - `Label`: the configuration's argument values, e.g.
#'   `'Smooth = TRUE, RecentYears = 2'`.
#' - `Value`: approximately tuned value of the tuning argument. When the
#'   constraint boundary lies between two grid values, `Value` is
#'   interpolated between them.
#' - `Objective`: objective at `Value` (interpolated in the same way; `NA`
#'   when infeasible).
#' - `Feasible`: did any grid value meet every constraint?
#' - `Status`: `'boundary'`, `'interior'`, `'at_bound'`, or `'infeasible'`
#'   (see the `Status` slot of [tunemp-class]).
#' - `Slack`: largest slack across the grid values (`Inf` without
#'   constraints).
#' - `TunedValue`, `TunedObjective`, `TunedStatus`: the tuned value,
#'   objective, and status of the `nRefine` best configurations that were
#'   tuned fully (`NA` for the others).
#'
#' ## `'History'`
#'
#' One row per evaluated combination of configuration and tuning-argument
#' value, in the order they were evaluated.
#' - `Config`: index of the configuration (`1` without `Configs`).
#' - `x`: value of the tuning argument.
#' - `Objective`: value of the objective (`NA` without an objective, or when
#'   the candidate failed).
#' - `Slack`: smallest slack across the constraints (`Inf` without
#'   constraints, `-Inf` when the candidate failed).
#' - `Feasible`: does the candidate meet every constraint (within `TolPM`)?
#' - `FailRate`: largest fraction, across the `Hist` objects, of
#'   simulation-management years in which the MP returned an error (`1` when
#'   the projection failed). Candidates with a `FailRate` greater than
#'   `MaxFailRate` (see [TuneControl()]) fail.
#' - One column per metric, named by the metric, with its value.
#' - `Stage`: `'Configure'` (first stage, on the first `nSimConfig`
#'   simulations when set) or `'Tune'` (second stage, all simulations).
#'
#' ## `'Validation'`
#'
#' One row per validation `Hist` object and metric (an empty data frame
#' without `ValidationHist` in [TuneMP()]). Metrics are calculated for each
#' `Hist` separately and combined across stocks only.
#' - `Hist`: name of the validation `Hist` object (`'Validation1'`, ...
#'   when unnamed).
#' - `Name`, `Type`, `Value`, `Min`, `Max`: as for `'Constraints'`.
#' - `Met`: is the constraint met (without tolerance)? `NA` for the
#'   objective.
#'
#' @param x A [tunemp-class] object.
#' @param What Character. `'Constraints'` (default; the tuned MP's metric
#'   values), `'Configs'` (the ranking of configurations), `'History'` (every
#'   evaluated candidate), or `'Validation'` (the tuned MP's metric values for
#'   `ValidationHist`). See Details.
#'
#' @return A `data.frame`; see Details for the columns.
#'
#' @examples
#' \dontrun{
#' Tuned <- TuneMP(Hist, IndexTarget,
#'                 Constraints = TuneConstraint(PM_SBSBMSY, Min = 0.6),
#'                 Configs     = list(RecentYears = 1:3))
#' TuneTable(Tuned)
#'
#' # configurations, best first
#' Cfg <- TuneTable(Tuned, 'Configs')
#' Cfg[order(-Cfg$Objective), ]
#'
#' # objective against the tuning argument for the chosen configuration
#' Evals <- TuneTable(Tuned, 'History')
#' Evals <- Evals[Evals$Stage == 'Tune', ]
#' plot(Objective ~ x, data = Evals, log = 'x', col = ifelse(Evals$Feasible, 1, 2))
#' }
#'
#' @seealso [TuneMP()], [tunemp-class]
#' @export
TuneTable <- function(x, What = c('Constraints', 'Configs', 'History', 'Validation')) {
  .CheckClass(x, 'tunemp', 'x')
  What <- match.arg(What)
  slot(x, What)
}
