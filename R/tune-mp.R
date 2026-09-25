#' Tune a Management Procedure to Meet Management Objectives
#'
#' Finds the value of a management procedure's tuning argument (`tunepar` by
#' default) that maximises an objective performance metric (e.g. mean
#' landings) while meeting minimum or maximum thresholds on other performance
#' metrics (e.g. `P(SB > SBMSY) >= 0.6`), evaluated over one or more
#' [hist-class] objects. Optionally, first chooses the best values of other
#' MP arguments from a set of candidate configurations.
#'
#' ## Objective and constraints
#'
#' The objective and constraints are performance metric functions (see [PM],
#' [NewPM()]), specified with [TuneObjective()] and [TuneConstraint()]. When
#' the objective is still increasing at the boundary set by the constraints
#' (e.g. yield increases with `tunepar` until the biomass constraint is
#' breached), the result is the feasible value closest to that boundary. When
#' the objective peaks inside the feasible range, the result is that peak.
#' 
#' With `Objective = NULL`, the tuning argument itself is maximised (or, with
#' `Direction = 'decreasing'` in [TuneControl()], minimised) subject to the
#' constraints: the classic tuning of an MP to a target.
#'
#' ## Search
#'
#' The tuning argument is first evaluated on a grid of `nGrid` log-spaced
#' values over `Interval` (see [TuneControl()]). All grid values are projected
#' in one [Project()] call per `Hist` object, so they run in parallel when
#' `parallel = TRUE`. The range is extended by `ExpandFactor` (up to
#' `MaxExpand` times) when no grid value meets the constraints, or when the
#' objective is still increasing at the end of the range. The best grid value
#' is then refined towards the constraint boundary, evaluating `nPerRound` values
#' per round, until the bracket is narrower than `TolTune` (relative), the slack 
#' is within `TolPM`, the objective changes by less than `TolObj` (relative), 
#' or after `MaxIter` rounds.
#'
#' The slack of a constraint is `(Value - Min) / |Min|` and/or
#' `(Max - Value) / |Max|`. A value is feasible when every slack is at
#' least `-TolPM`.
#'
#' Every candidate is evaluated with the same process and observation errors
#' (stored in each `Hist`), and any random numbers drawn by the MP are seeded
#' from `OM@@Seed` (see [Project()]), so tuned performance is reproduced
#' exactly by `Project()`.
#'
#' ## Configurations (optional first stage)
#'
#' `Configs` gives candidate values of other MP arguments. It can be a named
#' list of candidate values (all combinations are evaluated; wrap
#' vector-valued candidates in `list()`, e.g. `IndexWeight = list(c(1, 1),
#' c(2, 1))`), a list of named lists (each a configuration), or a data frame
#' (one configuration per row; list-columns allowed). Configurations are
#' compared at their own approximately tuned value: each is evaluated on a
#' coarse grid of `nGridConfig` values of the tuning argument (optionally on
#' the first `nSimConfig` simulations), its constrained optimum is
#' interpolated, and the configurations are ranked by the objective there.
#' The `nRefine` best are then tuned fully, and the one with the highest
#' objective is returned. Selecting configurations on the same operating
#' models used to tune them is optimistic; `ValidationHist` reports the
#' performance of the final MP on other operating models.
#'
#' @param Hist A [hist-class] object, or a (named) list of them (e.g. a
#'   reference set of operating models).
#' @param MP An MP function (or its name) with a `TuneArg` argument.
#' @param Objective A [TuneObjective()] object, or `NULL` to maximise the
#'   tuning argument subject to the constraints. Default
#'   `TuneObjective(PM_Landings)`.
#' @param Constraints A [TuneConstraint()] object or a list of them.
#' @param Configs `NULL` (default), or candidate configurations of other MP
#'   arguments; see Details.
#' @param HistWeights `NULL` (default, equal weights) or a positive numeric
#'   vector, one per `Hist`, used by metrics with `HistSummary = 'weighted'`.
#' @param TuneArg Character. Name of the numeric MP argument to tune. Default
#'   `'tunepar'`.
#' @param Control A [TuneControl()] list.
#' @param ValidationHist `NULL` (default), or a [hist-class] object or list
#'   of them, used only to report the final MP's performance.
#' @param parallel Logical. Project candidates in parallel (see
#'   [SetupParallel()])? Default `FALSE`.
#' @param silent Logical. Suppress progress messages? Default `FALSE`.
#'
#' @return A [tunemp-class] object. The tuned MP is `@@MP`.
#'
#' @examples
#' \dontrun{
#' Hist <- Simulate(SingleStockOM)
#' Tuned <- TuneMP(Hist, IndexTarget,
#'                 Objective   = TuneObjective(PM_Landings),
#'                 Constraints = list(TuneConstraint(PM_SBSBMSY, Min = 0.6),
#'                                    TuneConstraint(PM_Safety, Lim = 0.4, Min = 0.9)),
#'                 Configs     = list(Smooth = c(TRUE, FALSE), RecentYears = 1:2))
#' Tuned
#' MSE <- Project(Hist, MPs = list(IT_Tuned = Tuned@@MP))
#' }
#'
#' @seealso [TuneObjective()], [TuneConstraint()], [TuneControl()],
#'   [TuneTable()], [SetMPArgs()], [PM]
#' @export
TuneMP <- function(Hist,
                   MP,
                   Objective      = TuneObjective(PM_Landings),
                   Constraints    = list(),
                   Configs        = NULL,
                   HistWeights    = NULL,
                   TuneArg        = 'tunepar',
                   Control        = TuneControl(),
                   ValidationHist = NULL,
                   parallel       = FALSE,
                   silent         = FALSE) {

  MPName  <- if (is.character(MP)) MP else deparse(substitute(MP))[1]
  OldSeed <- .SaveRNG()
  on.exit(.RestoreRNG(OldSeed), add = TRUE)
  if (is.character(MP)) MP <- get(MP, mode = 'function')
  .CheckClass(MP, c('mp', 'mmp'), 'MP')
  if (!TuneArg %in% names(formals(MP)))
    cli::cli_abort("{.arg MP} has no {.arg {TuneArg}} argument to tune.")
  if (!inherits(Control, 'tunecontrol'))
    cli::cli_abort("{.arg Control} must be made with {.fn TuneControl}.")

  HistList <- .TuneHistList(Hist, 'Hist')
  nHist    <- length(HistList)
  if (is.null(HistWeights)) HistWeights <- rep(1, nHist)
  if (length(HistWeights) != nHist || any(!is.finite(HistWeights)) || any(HistWeights < 0) ||
      !any(HistWeights > 0))
    cli::cli_abort("{.arg HistWeights} must be {nHist} non-negative number{?s}, at least one positive.")
  names(HistWeights) <- names(HistList)

  if (inherits(Constraints, 'tunemetric')) Constraints <- list(Constraints)
  if (!is.list(Constraints) || !all(vapply(Constraints, inherits, logical(1), 'tunemetric')))
    cli::cli_abort("{.arg Constraints} must be a {.fn TuneConstraint} or a list of them.")
  if (!all(vapply(Constraints, \(m) m$Type == 'constraint', logical(1))))
    cli::cli_abort("{.arg Constraints} must be made with {.fn TuneConstraint}.")
  if (!is.null(Objective) && !(inherits(Objective, 'tunemetric') && Objective$Type == 'objective'))
    cli::cli_abort("{.arg Objective} must be made with {.fn TuneObjective}, or {.code NULL}.")
  if (is.null(Objective) && !length(Constraints))
    cli::cli_abort("Supply an {.arg Objective}, {.arg Constraints}, or both.")

  Metrics <- c(if (!is.null(Objective)) list(Objective), Constraints)
  Metrics <- .TuneResolveMetrics(Metrics, names(HistList))

  ConfigList <- .TuneConfigList(Configs, MP, TuneArg, Control)
  if (length(ConfigList) > 1 && is.null(Objective))
    cli::cli_abort("Comparing {.arg Configs} needs an {.arg Objective}.")

  nPerRound <- Control$nPerRound %||%
    if (parallel && requireNamespace('future', quietly = TRUE)) max(1, future::nbrOfWorkers()) else 1

  State              <- new.env()
  State$HistList     <- HistList
  State$HistWeights  <- HistWeights
  State$MP           <- MP
  State$MPName       <- MPName
  State$TuneArg      <- TuneArg
  State$Metrics      <- Metrics
  State$Configs      <- ConfigList
  State$Control      <- Control
  State$parallel     <- parallel
  State$silent       <- silent
  State$Cache        <- list()
  State$History      <- list()
  State$MSE          <- list()
  State$HasObjective <- !is.null(Objective)
  State$nPerRound    <- nPerRound

  if (Control$ValidateConfigs)
    .TuneValidateConfigs(State)

  if (Control$DryRun)
    return(.TuneDryRun(State))

  Ranking    <- data.frame()
  Candidates <- 1L
  if (length(ConfigList) > 1) {
    if (!silent) cli::cli_inform("Stage 1: comparing {length(ConfigList)} configurations.")
    Ranking    <- .TuneConfigure(State)
    Ok         <- Ranking$Feasible
    Order      <- Ranking$Config[order(!Ok, -Ranking$Objective, -Ranking$Slack)]
    Candidates <- Order[seq_len(min(Control$nRefine, length(Order)))]
  }

  Results <- lapply(Candidates, \(cfg) {
    if (!silent)
      cli::cli_inform("Stage 2: tuning {.arg {TuneArg}}{if (length(ConfigList) > 1) paste0(' for configuration ', cfg) else ''}.")
    Eval <- function(x) .TuneEvalPoints(State, cfg, x, nSim = NULL, Stage = 'Tune')
    Res        <- .TuneSearch(Eval, Control, State$HasObjective, nPerRound)
    Res$Config <- cfg
    Res
  })

  Best  <- .TunePickBest(Results, State$HasObjective, Control$Direction)
  Args  <- c(ConfigList[[Best$Config]], stats::setNames(list(Best$x), TuneArg))
  Point <- Best$Points[which.min(abs(log(Best$Points$x) - log(Best$x))), ]

  Tuned      <- do.call(SetMPArgs, c(list(MP), Args))
  Constraint <- .TuneConstraintTable(State, Point)
  attr(Tuned, 'Tuning') <- list(TuneArg = TuneArg, Value = Best$x, Args = Args,
                                Status = Best$Status, Objective = Point$Objective,
                                Constraints = Constraint)

  if (nrow(Ranking)) {
    for (r in Results) {
      i <- match(r$Config, Ranking$Config)
      P <- r$Points[which.min(abs(log(r$Points$x) - log(r$x))), ]
      Ranking$TunedValue[i] <- r$x
      Ranking$TunedObjective[i] <- P$Objective
      Ranking$TunedStatus[i] <- r$Status
    }
  }

  Validation <- data.frame()
  if (!is.null(ValidationHist))
    Validation <- .TuneValidate(State, Tuned, .TuneHistList(ValidationHist, 'Validation'))

  History <- do.call(rbind, State$History)
  rownames(History) <- NULL

  Out <- methods::new('tunemp',
                      MP          = Tuned,
                      MPName      = MPName,
                      Args        = Args,
                      Status      = Best$Status,
                      Objective   = if (State$HasObjective) Point$Objective else NA_real_,
                      Constraints = Constraint,
                      Configs     = Ranking,
                      History     = History,
                      Validation  = Validation,
                      MSE         = State$MSE,
                      Settings    = list(Metrics = Metrics, Control = Control,
                                         HistWeights = HistWeights, TuneArg = TuneArg))
  if (!silent) methods::show(Out)
  invisible(Out)
}

#' Specify the Objective and Constraints for Tuning an MP
#'
#' `TuneObjective()` specifies the performance metric maximised by
#' [TuneMP()]; `TuneConstraint()` a performance metric that must be at least
#' `Min` and/or at most `Max`.
#'
#' A metric is calculated for each `Hist` object and stock (or complex, see
#' [PM]) as `Value` of the [pm-class] object returned by `PM`, then combined
#' across stocks (`StockSummary`) and across `Hist` objects (`HistSummary`).
#' `'worst'` requires the constraint to hold for every stock (or `Hist`)
#' separately; `'weighted'` averages with `HistWeights`; `'pooled'` averages
#' with weights proportional to the number of simulations (equivalent to
#' pooling the simulations of all `Hist` objects).
#'
#' @param PM A performance metric function (class `pm`, see [PM] and
#'   [NewPM()]), or its name.
#' @param ... Arguments passed to `PM`, e.g. `Ref`, `Years`, `Lim`.
#' @param Min,Max `NULL` or a number: minimum and maximum allowed values.
#'   At least one is needed.
#' @param Value `'Mean'` (default; `pm@@Mean`), `'Stat'` (mean over
#'   simulations of `pm@@Stat`), or a function of the `pm` object returning a
#'   Stock x MP matrix.
#' @param Stocks `NULL` (default, all) or the names of the stocks (complexes)
#'   to use.
#' @param StockSummary Character. How the metric is combined across stocks.
#' @param HistSummary Character. How the metric is combined across `Hist`
#'   objects.
#' @param Hists `NULL` (default, all) or the names or positions of the `Hist`
#'   objects the metric applies to.
#' @param Name Character. Label of the metric. Default is the name of `PM`.
#'
#' @return A list of class `tunemetric`.
#'
#' @examples
#' TuneObjective(PM_Landings, Years = 2031:2050)
#' TuneConstraint(PM_SBSBMSY, Min = 0.6)
#' TuneConstraint(PM_AAVY, Value = 'Mean', Max = 0.2)
#'
#' @seealso [TuneMP()], [PM]
#' @export
TuneObjective <- function(PM, ..., Value = 'Mean', Stocks = NULL,
                          StockSummary = c('sum', 'mean', 'min', 'max'),
                          HistSummary = c('weighted', 'pooled', 'min', 'max'),
                          Hists = NULL, Name = NULL) {
  Name <- Name %||% if (is.character(PM)) PM else deparse(substitute(PM))[1]
  .NewTuneMetric('objective', PM, list(...), NULL, NULL, Value, Stocks,
                 match.arg(StockSummary, c('sum', 'mean', 'min', 'max')),
                 match.arg(HistSummary, c('weighted', 'pooled', 'min', 'max')), Hists, Name)
}

#' @rdname TuneObjective
#' @export
TuneConstraint <- function(PM, ..., Min = NULL, Max = NULL, Value = 'Mean', Stocks = NULL,
                           StockSummary = c('worst', 'mean', 'sum', 'min', 'max'),
                           HistSummary = c('weighted', 'pooled', 'worst', 'min', 'max'),
                           Hists = NULL, Name = NULL) {
  Name <- Name %||% if (is.character(PM)) PM else deparse(substitute(PM))[1]
  if (is.null(Min) && is.null(Max))
    cli::cli_abort("{.fn TuneConstraint} needs {.arg Min}, {.arg Max}, or both.")
  for (x in list(Min, Max))
    if (!is.null(x) && (!is.numeric(x) || length(x) != 1 || !is.finite(x)))
      cli::cli_abort("{.arg Min} and {.arg Max} must be single numbers.")
  if (!is.null(Min) && !is.null(Max) && Min > Max)
    cli::cli_abort("{.arg Min} must not be greater than {.arg Max}.")
  .NewTuneMetric('constraint', PM, list(...), Min, Max, Value, Stocks,
                 match.arg(StockSummary, c('worst', 'mean', 'sum', 'min', 'max')),
                 match.arg(HistSummary, c('weighted', 'pooled', 'worst', 'min', 'max')), Hists, Name)
}

.NewTuneMetric <- function(Type, PM, Args, Min, Max, Value, Stocks, StockSummary, HistSummary,
                           Hists, Name) {
  if (is.character(PM)) PM <- get(PM, mode = 'function')
  if (!is.function(PM))
    cli::cli_abort("{.arg PM} must be a performance metric function or its name.")
  if (!is.function(Value) && !(is.character(Value) && Value %in% c('Mean', 'Stat')))
    cli::cli_abort("{.arg Value} must be {.val Mean}, {.val Stat}, or a function.")
  structure(list(Type = Type, PM = PM, Args = Args, Min = Min, Max = Max, Value = Value,
                 Stocks = Stocks, StockSummary = StockSummary, HistSummary = HistSummary,
                 Hists = Hists, Name = Name),
            class = 'tunemetric')
}

#' @export
print.tunemetric <- function(x, ...) {
  Bounds <- c(if (!is.null(x$Min)) paste('>=', x$Min), if (!is.null(x$Max)) paste('<=', x$Max))
  cli::cli_text("{.strong {x$Name}} ({x$Type}{if (length(Bounds)) paste0(', ', paste(Bounds, collapse = ' and ')) else ''}): ",
                "{x$Value} across stocks: {x$StockSummary}; across Hists: {x$HistSummary}")
  invisible(x)
}

#' Settings for Tuning an MP
#'
#' Controls for [TuneMP()]. See the Details of [TuneMP()] for how they are
#' used.
#'
#' @param Interval Numeric vector, length 2. Initial range of the tuning
#'   argument. Default `c(0.1, 10)`.
#' @param nGrid Positive integer. Number of log-spaced values in the initial
#'   grid. Default `7`.
#' @param Expand Logical. Extend the range when needed? Default `TRUE`.
#' @param ExpandFactor Number greater than 1. Factor by which the range is
#'   extended. Default `3`.
#' @param MaxExpand Non-negative integer. Maximum number of extensions.
#'   Default `2`.
#' @param nPerRound `NULL` (default: the number of parallel workers when
#'   `parallel = TRUE`, else `1`) or the number of values evaluated per
#'   refinement round.
#' @param MaxIter Positive integer. Maximum number of refinement rounds.
#'   Default `10`.
#' @param TolTune Positive number. Relative width of the tuning-argument
#'   bracket at which refinement stops. Default `0.01`.
#' @param TolPM Non-negative number. Constraint slack tolerance. Default
#'   `0.005`.
#' @param TolObj Positive number. Relative change in the objective at which
#'   refinement of an interior optimum stops. Default `0.001`.
#' @param Direction Character. `'increasing'` (default) if larger values of
#'   the tuning argument give higher catches (as for `tunepar`), otherwise
#'   `'decreasing'`. Used when `Objective = NULL`.
#' @param nGridConfig Positive integer. Number of values of the tuning
#'   argument evaluated for each configuration in the first stage. Default
#'   `5`.
#' @param nSimConfig `NULL` (default, all) or the number of simulations used
#'   in the first stage.
#' @param nRefine Positive integer. Number of best configurations tuned fully.
#'   Default `1`.
#' @param MaxConfigs Positive integer. Maximum number of configurations.
#'   Default `200`.
#' @param ValidateConfigs Logical. Apply the MP (with each configuration) to
#'   the historical data of the first `Hist` before projecting, to catch
#'   invalid arguments? Default `TRUE`.
#' @param MaxFailRate Number in `[0, 1]`. Candidates whose MP fails in a
#'   larger fraction of simulation-management years are infeasible. Default
#'   `0.05`.
#' @param KeepMSE Logical. Keep the [mse-class] objects of every evaluation
#'   (`@@MSE` of the result)? Default `FALSE`.
#' @param DryRun Logical. Only report the number of projections and an
#'   estimate of the run time? Default `FALSE`.
#'
#' @return A list of class `tunecontrol`.
#' @seealso [TuneMP()]
#' @export
TuneControl <- function(Interval        = c(0.1, 10),
                        nGrid           = 7,
                        Expand          = TRUE,
                        ExpandFactor    = 3,
                        MaxExpand       = 2,
                        nPerRound       = NULL,
                        MaxIter         = 10,
                        TolTune         = 0.01,
                        TolPM           = 0.005,
                        TolObj          = 0.001,
                        Direction       = c('increasing', 'decreasing'),
                        nGridConfig     = 5,
                        nSimConfig      = NULL,
                        nRefine         = 1,
                        MaxConfigs      = 200,
                        ValidateConfigs = TRUE,
                        MaxFailRate     = 0.05,
                        KeepMSE         = FALSE,
                        DryRun          = FALSE) {
  Direction <- match.arg(Direction, c('increasing', 'decreasing'))
  if (length(Interval) != 2 || any(!(Interval > 0)) || Interval[1] >= Interval[2])
    cli::cli_abort("{.arg Interval} must be two increasing positive numbers.")
  for (nm in c('nGrid', 'nGridConfig'))
    if (get(nm) < 2) cli::cli_abort("{.arg {nm}} must be at least 2.")
  if (!(ExpandFactor > 1))
    cli::cli_abort("{.arg ExpandFactor} must be greater than 1.")
  structure(
    list(Interval = Interval, nGrid = as.integer(nGrid), Expand = Expand,
         ExpandFactor = ExpandFactor, MaxExpand = as.integer(MaxExpand), nPerRound = nPerRound,
         MaxIter = as.integer(MaxIter), TolTune = TolTune, TolPM = TolPM, TolObj = TolObj,
         Direction = Direction, nGridConfig = as.integer(nGridConfig), nSimConfig = nSimConfig,
         nRefine = as.integer(nRefine), MaxConfigs = MaxConfigs,
         ValidateConfigs = ValidateConfigs, MaxFailRate = MaxFailRate,
         KeepMSE = KeepMSE, DryRun = DryRun),
    class = 'tunecontrol'
  )
}

.TuneHistList <- function(Hist, Label) {
  if (inherits(Hist, 'hist')) Hist <- list(Hist)
  if (!is.list(Hist) || !length(Hist) || !all(vapply(Hist, inherits, logical(1), 'hist')))
    cli::cli_abort("{.arg {Label}} must be a {.cls hist} object or a list of them.")
  nms <- names(Hist)
  if (is.null(nms)) nms <- rep('', length(Hist))
  nms[!nzchar(nms)] <- paste0(Label, seq_along(Hist))[!nzchar(nms)]
  names(Hist) <- make.unique(nms)
  Hist
}

.TuneResolveMetrics <- function(Metrics, HistNames) {
  Names <- make.unique(vapply(Metrics, `[[`, character(1), 'Name'))
  for (i in seq_along(Metrics)) {
    Metrics[[i]]$Name <- Names[i]
    H <- Metrics[[i]]$Hists
    if (is.null(H)) {
      H <- HistNames
    } else if (is.numeric(H)) {
      if (!all(H %in% seq_along(HistNames)))
        cli::cli_abort("{.arg Hists} of {.val {Names[i]}} must be within 1 to {length(HistNames)}.")
      H <- HistNames[H]
    } else if (!all(H %in% HistNames)) {
      cli::cli_abort("{.arg Hists} of {.val {Names[i]}} not found: {.val {setdiff(H, HistNames)}}.")
    }
    Metrics[[i]]$Hists <- H
  }
  Metrics
}

.TuneConfigList <- function(Configs, MP, TuneArg, Control) {
  if (is.null(Configs) || !length(Configs))
    return(list(list()))
  if (is.data.frame(Configs)) {
    List <- lapply(seq_len(nrow(Configs)), \(i) lapply(Configs, \(col) {
      v <- col[i]
      if (is.list(v)) v[[1]] else v
    }))
  } else if (is.list(Configs) && is.null(names(Configs)) &&
             all(vapply(Configs, \(x) is.list(x) && !is.null(names(x)), logical(1)))) {
    List <- Configs
  } else if (is.list(Configs) && !is.null(names(Configs)) && all(nzchar(names(Configs)))) {
    Candidates <- lapply(Configs, \(x) if (is.list(x)) x else as.list(x))
    Grid       <- expand.grid(lapply(Candidates, seq_along), KEEP.OUT.ATTRS = FALSE)
    List       <- lapply(seq_len(nrow(Grid)), \(i) stats::setNames(
      lapply(names(Candidates), \(nm) Candidates[[nm]][[Grid[[nm]][i]]]), names(Candidates)))
  } else {
    cli::cli_abort("{.arg Configs} must be a named list of candidate values, a list of named lists, or a data frame.")
  }
  if (length(List) > Control$MaxConfigs)
    cli::cli_abort("{.arg Configs} has {length(List)} configurations; {.arg MaxConfigs} is {Control$MaxConfigs}.")
  ArgNames <- unique(unlist(lapply(List, names)))
  if (TuneArg %in% ArgNames)
    cli::cli_abort("{.arg Configs} cannot include the tuning argument {.arg {TuneArg}}.")
  if (!'...' %in% names(formals(MP))) {
    Bad <- setdiff(ArgNames, names(formals(MP)))
    if (length(Bad))
      cli::cli_abort("{.arg Configs} argument{?s} {.val {Bad}} {?is/are} not in {.arg MP}.")
  }
  List
}

.TuneConfigLabel <- function(Config) {
  if (!length(Config)) return('(default)')
  paste(vapply(names(Config), \(nm) {
    v <- Config[[nm]]
    paste0(nm, ' = ', if (is.null(v)) 'NULL' else paste(deparse(v, width.cutoff = 500L), collapse = ''))
  }, character(1)), collapse = ', ')
}

.TuneValidateConfigs <- function(State) {
  Data   <- State$HistList[[1]]@Data[[1]][[1]]
  Errors <- character()
  for (i in seq_along(State$Configs)) {
    Fn  <- do.call(SetMPArgs, c(list(State$MP), State$Configs[[i]]))
    Res <- try(Fn(Data), silent = TRUE)
    if (inherits(Res, 'try-error'))
      Errors <- c(Errors, sprintf('%d (%s): %s', i, .TuneConfigLabel(State$Configs[[i]]),
                                  trimws(conditionMessage(attr(Res, 'condition')))))
  }
  if (length(Errors))
    cli::cli_abort(c("{length(Errors)} configuration{?s} failed when applied to the historical data:",
                     stats::setNames(Errors, rep('x', length(Errors)))))
  invisible(TRUE)
}

.TuneKey <- function(Config, x, nSim) paste(Config, format(x, digits = 15), nSim %||% 'all')

.TuneVariantName <- function(Config, x) sprintf('C%d_T%.10g', Config, x)

.TuneEvalPoints <- function(State, Config, x, nSim, Stage) {
  Pairs <- data.frame(Config = rep_len(Config, length(x)), x = x)
  .TuneEvalPairs(State, Pairs, nSim, Stage)
}

.TuneEvalPairs <- function(State, Pairs, nSim, Stage) {
  Keys <- .TuneKey(Pairs$Config, Pairs$x, nSim)
  New  <- !Keys %in% names(State$Cache) & !duplicated(Keys)
  if (any(New)) {
    NewPairs <- Pairs[New, , drop = FALSE]
    Variants <- lapply(seq_len(nrow(NewPairs)), \(i) {
      Args <- c(State$Configs[[NewPairs$Config[i]]],
                stats::setNames(list(NewPairs$x[i]), State$TuneArg))
      do.call(SetMPArgs, c(list(State$MP), Args))
    })
    VNames <- .TuneVariantName(NewPairs$Config, NewPairs$x)
    names(Variants) <- VNames
    Raw <- .TuneProject(State, Variants, nSim)
    for (i in seq_along(VNames))
      State$Cache[[Keys[New][i]]] <- lapply(Raw, \(r) list(Values = lapply(r$Values, \(v) v[, VNames[i]]),
                                                         FailRate = r$FailRate[[VNames[i]]],
                                                         nSim = r$nSim))
  }
  Out <- do.call(rbind, lapply(seq_len(nrow(Pairs)), \(i) {
    .TuneSummarise(State, State$Cache[[Keys[i]]], Pairs$Config[i], Pairs$x[i])
  }))
  Out$Stage <- Stage
  State$History[[length(State$History) + 1]] <- Out[New, , drop = FALSE]
  Out
}

.TuneProject <- function(State, Variants, nSim) {
  HistNames <- unique(unlist(lapply(State$Metrics, `[[`, 'Hists')))
  Out       <- list()
  for (h in HistNames) {
    MSE <- tryCatch(Project(State$HistList[[h]], MPs = Variants, parallel = State$parallel,
                            silent = TRUE, nSim = nSim),
                    error = function(e) e)
    Values   <- list()
    FailRate <- stats::setNames(rep(1, length(Variants)), names(Variants))
    nSimUsed <- nSim %||% State$HistList[[h]]@OM@nSim
    if (!inherits(MSE, 'error')) {
      if (State$Control$KeepMSE)
        State$MSE[[length(State$MSE) + 1]] <- list(Hist = h, Variants = names(Variants), MSE = MSE)
      for (m in State$Metrics) {
        if (!h %in% m$Hists) next
        Values[[m$Name]] <- .TuneMetricMatrix(m, MSE, names(Variants))
      }
      FailRate <- .TuneFailRate(MSE, Variants)
      nSimUsed <- MSE@OM@nSim
    } else {
      for (m in State$Metrics) {
        if (!h %in% m$Hists) next
        Values[[m$Name]] <- matrix(NA_real_, 1, length(Variants), dimnames = list(NULL, names(Variants)))
      }
    }
    Out[[h]] <- list(Values = Values, FailRate = FailRate, nSim = nSimUsed)
  }
  Out
}

.TuneMetricMatrix <- function(Metric, MSE, VNames) {
  Res <- tryCatch({
    pm  <- do.call(Metric$PM, c(list(MSE), Metric$Args))
    Mat <- if (is.function(Metric$Value)) {
      Metric$Value(pm)
    } else if (Metric$Value == 'Mean') {
      pm@Mean
    } else {
      apply(pm@Stat, c(2, 3), mean, na.rm = TRUE)
    }
    Mat <- as.matrix(Mat)
    if (!is.null(Metric$Stocks)) {
      Missing <- setdiff(Metric$Stocks, rownames(Mat))
      if (length(Missing))
        cli::cli_abort("{.arg Stocks} {.val {Missing}} not found for metric {.val {Metric$Name}}.")
      Mat <- Mat[Metric$Stocks, , drop = FALSE]
    }
    Out    <- matrix(NA_real_, nrow(Mat), length(VNames), dimnames = list(rownames(Mat), VNames))
    Common <- intersect(VNames, colnames(Mat))
    Out[, Common] <- Mat[, Common]
    Out
  }, error = function(e) {
    if (grepl('not found for metric', conditionMessage(e))) stop(e)
    matrix(NA_real_, 1, length(VNames), dimnames = list(NULL, VNames))
  })
  Res
}

.TuneFailRate <- function(MSE, Variants) {
  nSim      <- MSE@OM@nSim
  YearsProj <- Years(MSE@OM, 'Projection')
  YearsProj <- YearsProj[!YearsProj %in% .InterimTimesteps(MSE@OM)]
  Errors    <- MSE@Log$error
  ErrMP     <- vapply(Errors, \(e) if (is.list(e) && !is.null(e$mp)) as.character(e$mp) else NA_character_,
                  character(1))
  vapply(names(Variants), \(v) {
    Interval <- .ResolveInterval(MSE@OM@Interval, v, Variants[[v]], MSE@OM@Seasons)
    nMgmt    <- if (length(YearsProj)) length(.CalcManagementYears(YearsProj, Interval, MSE@OM@Seasons)) else 1
    Failed   <- !v %in% names(MSE@MPs) || !v %in% dimnames(MSE@SBiomass)[['MP']]
    if (Failed) return(1)
    min(1, sum(ErrMP == v, na.rm = TRUE) / (nSim * nMgmt))
  }, numeric(1))
}

.TuneSlack <- function(Value, Min, Max) {
  Scale <- function(t) if (abs(t) > 0) abs(t) else 1
  S <- Inf
  if (!is.null(Min)) S <- pmin(S, (Value - Min) / Scale(Min))
  if (!is.null(Max)) S <- pmin(S, (Max - Value) / Scale(Max))
  S
}

.TuneReduce <- function(x, How, Weights = NULL) {
  if (anyNA(x)) return(NA_real_)
  switch(How,
         sum      = sum(x),
         mean     = mean(x),
         min      = min(x),
         max      = max(x),
         weighted = ,
         pooled   = sum(x * Weights) / sum(Weights))
}

.TuneAggregate <- function(Metric, PerHist, HistWeights, nSims) {
  Hists  <- Metric$Hists
  Mats   <- lapply(Hists, \(h) PerHist[[h]]$Values[[Metric$Name]])
  nStock <- max(vapply(Mats, length, integer(1)))
  Arr    <- do.call(rbind, lapply(Mats, \(v) if (length(v) == nStock) unname(v) else rep(NA_real_, nStock)))
  if (is.null(dim(Arr))) Arr <- matrix(Arr, nrow = length(Hists))

  if (Metric$StockSummary != 'worst')
    Arr <- matrix(apply(Arr, 1, .TuneReduce, How = Metric$StockSummary), ncol = 1)
  if (Metric$HistSummary != 'worst') {
    W   <- if (Metric$HistSummary == 'pooled') nSims[Hists] else HistWeights[Hists]
    Arr <- matrix(apply(Arr, 2, .TuneReduce, How = Metric$HistSummary, Weights = W), nrow = 1)
  }
  if (anyNA(Arr)) return(c(Value = NA_real_, Slack = -Inf))
  if (Metric$Type == 'objective') return(c(Value = Arr[1], Slack = NA_real_))
  Slack <- .TuneSlack(Arr, Metric$Min, Metric$Max)
  k     <- which.min(Slack)
  c(Value = Arr[k], Slack = Slack[k])
}

.TuneSummarise <- function(State, Cached, Config, x) {
  nSims    <- vapply(Cached, `[[`, numeric(1), 'nSim')
  FailRate <- max(vapply(Cached, \(r) r$FailRate %||% 1, numeric(1)))
  Row      <- data.frame(Config = Config, x = x, Objective = NA_real_, Slack = Inf,
                    Feasible = TRUE, FailRate = FailRate)
  for (m in State$Metrics) {
    Agg <- .TuneAggregate(m, Cached, State$HistWeights, nSims)
    Row[[m$Name]] <- Agg[['Value']]
    if (m$Type == 'objective') {
      Row$Objective <- Agg[['Value']]
    } else {
      Row$Slack <- min(Row$Slack, Agg[['Slack']])
    }
  }
  Failed <- FailRate > State$Control$MaxFailRate ||
    (State$HasObjective && !is.finite(Row$Objective))
  if (Failed) {
    Row$Slack     <- -Inf
    Row$Objective <- NA_real_
  }
  Row$Feasible <- is.finite(Row$Slack) && Row$Slack >= -State$Control$TolPM || Row$Slack == Inf
  if (Failed) Row$Feasible <- FALSE
  Row
}

.TuneScore <- function(Points, HasObjective, Direction) {
  if (HasObjective) return(Points$Objective)
  (if (Direction == 'increasing') 1 else -1) * log(Points$x)
}

.TuneSearch <- function(Eval, Control, HasObjective, nPerRound = 1) {
  Search <- new.env(parent = emptyenv())
  Search$Eval         <- Eval
  Search$Control      <- Control
  Search$HasObjective <- HasObjective
  Search$Points       <- NULL
  Search$nExpand      <- 0

  Lo <- log(Control$Interval[1])
  Hi <- log(Control$Interval[2])
  .TuneAdd(Search, exp(seq(Lo, Hi, length.out = Control$nGrid)))

  repeat {
    Points <- Search$Points
    S      <- .TuneFeasibleScore(Search)
    n      <- nrow(Points)
    if (!any(is.finite(S))) {
      if (Control$Expand && Search$nExpand < Control$MaxExpand) {
        Sl <- replace(Points$Slack, !is.finite(Points$Slack), -Inf)
        .TuneExpand(Search, if (Sl[n] > Sl[1]) 1 else -1)
        next
      }
      return(.TuneResult(Search, 'infeasible'))
    }
    k      <- which.max(S)
    R      <- .TuneScore(Points, HasObjective, Control$Direction)
    Better <- function(j) j >= 1 && j <= n && !Points$Feasible[j] && is.finite(R[j]) && R[j] > S[k]
    Cand   <- c(k - 1, k + 1)[c(Better(k - 1), Better(k + 1))]
    if (length(Cand)) {
      j <- Cand[which.max(R[Cand])]
      return(.TuneBoundary(Search, k, j, nPerRound))
    }
    Rising <- (k == n && (n == 1 || S[k] > S[k - 1])) || (k == 1 && n > 1 && S[k] > S[k + 1])
    if (Rising && (k == n || k == 1)) {
      if (Control$Expand && Search$nExpand < Control$MaxExpand) {
        .TuneExpand(Search, if (k == n) 1 else -1)
        next
      }
      return(.TuneResult(Search, 'at_bound'))
    }
    return(.TuneInterior(Search, k, nPerRound))
  }
}

.TuneAdd <- function(Search, x) {
  Done <- log(Search$Points$x %||% numeric(0))
  x    <- x[!vapply(x, \(v) any(abs(log(v) - Done) < 1e-10), logical(1))]
  if (length(x))
    Search$Points <- rbind(Search$Points, Search$Eval(x))
  Search$Points <- Search$Points[order(Search$Points$x), , drop = FALSE]
  invisible(Search)
}

.TuneFeasibleScore <- function(Search) {
  Points <- Search$Points
  S <- .TuneScore(Points, Search$HasObjective, Search$Control$Direction)
  S[!Points$Feasible | is.na(S)] <- -Inf
  S
}

.TuneResult <- function(Search, Status) {
  Points <- Search$Points
  S      <- .TuneFeasibleScore(Search)
  k      <- if (any(is.finite(S))) which.max(S) else which.max(replace(Points$Slack, is.na(Points$Slack), -Inf))
  list(x = Points$x[k], Status = Status, Points = Points)
}

.TuneExpand <- function(Search, Side) {
  Control <- Search$Control
  Step    <- log(Control$ExpandFactor)
  nNew    <- max(2, ceiling(Control$nGrid / 2))
  u       <- log(Search$Points$x)
  New     <- if (Side > 0) max(u) + Step * seq_len(nNew) / nNew else min(u) - Step * seq_len(nNew) / nNew
  .TuneAdd(Search, exp(New))
  Search$nExpand <- Search$nExpand + 1
  invisible(Search)
}

.TuneBoundary <- function(Search, k, j, nPerRound) {
  Control <- Search$Control
  Points  <- Search$Points
  a       <- log(Points$x[k])
  b       <- log(Points$x[j])
  sa      <- Points$Slack[k]
  sb      <- Points$Slack[j]
  Side    <- 0
  for (it in seq_len(Control$MaxIter)) {
    if (abs(b - a) < log1p(Control$TolTune) || abs(sa) < Control$TolPM) break
    if (nPerRound > 1) {
      New <- a + (b - a) * seq_len(nPerRound) / (nPerRound + 1)
    } else {
      u   <- if (is.finite(sb) && is.finite(sa) && sa != sb) a - sa * (b - a) / (sb - sa) else (a + b) / 2
      Lim <- sort(c(a, b))
      Pad <- 0.05 * (Lim[2] - Lim[1])
      New <- min(max(u, Lim[1] + Pad), Lim[2] - Pad)
    }
    .TuneAdd(Search, exp(New))
    P      <- Search$Points
    u      <- log(P$x)
    Inside <- which(u > min(a, b) - 1e-12 & u < max(a, b) + 1e-12)
    Inside <- Inside[order(abs(u[Inside] - a))]
    First  <- Inside[!P$Feasible[Inside]][1]
    Last   <- max(which(P$Feasible[Inside] & seq_along(Inside) < match(First, Inside)))
    NewA   <- u[Inside[Last]]
    NewB   <- u[First]
    if (nPerRound == 1) {
      if (NewA != a) {
        if (Side == 1) sb <- sb / 2
        Side <- 1
      } else {
        if (Side == -1) sa <- sa / 2
        Side <- -1
      }
    }
    if (NewA != a) sa <- P$Slack[Inside[Last]]
    if (NewB != b) sb <- P$Slack[First]
    a <- NewA
    b <- NewB
  }
  Best   <- which.max(.TuneFeasibleScore(Search))
  Status <- if (abs(log(Search$Points$x[Best]) - a) < 1e-10) 'boundary' else 'interior'
  .TuneResult(Search, Status)
}

.TuneInterior <- function(Search, k, nPerRound) {
  Control <- Search$Control
  u       <- log(Search$Points$x)
  L       <- u[max(1, k - 1)]
  R       <- u[min(length(u), k + 1)]
  Prev    <- max(.TuneFeasibleScore(Search))
  Phi     <- (sqrt(5) - 1) / 2
  for (it in seq_len(Control$MaxIter)) {
    if (R - L < log1p(Control$TolTune)) break
    New <- if (nPerRound > 1) L + (R - L) * seq_len(nPerRound) / (nPerRound + 1) else {
      Best <- log(Search$Points$x[which.max(.TuneFeasibleScore(Search))])
      if (Best - L > R - Best) Best - (1 - Phi) * (Best - L) else Best + (1 - Phi) * (R - Best)
    }
    .TuneAdd(Search, exp(New))
    S   <- .TuneFeasibleScore(Search)
    uu  <- log(Search$Points$x)
    kb  <- which.max(S)
    InB <- which(uu >= L - 1e-12 & uu <= R + 1e-12)
    Pos <- match(kb, InB)
    if (!is.na(Pos)) {
      L <- uu[InB[max(1, Pos - 1)]]
      R <- uu[InB[min(length(InB), Pos + 1)]]
    }
    Now  <- max(S)
    Gain <- Now - Prev
    if (Gain > 0 && Gain <= Control$TolObj * max(abs(Now), 1e-12)) break
    Prev <- Now
  }
  .TuneResult(Search, 'interior')
}

.TunePickBest <- function(Results, HasObjective, Direction) {
  Scores <- vapply(Results, \(r) {
    P <- r$Points[which.min(abs(log(r$Points$x) - log(r$x))), ]
    if (!P$Feasible) return(-Inf)
    .TuneScore(P, HasObjective, Direction)
  }, numeric(1))
  Results[[if (any(is.finite(Scores))) which.max(Scores) else 1]]
}

.TuneApprox <- function(Points, HasObjective, Direction) {
  Points <- Points[order(Points$x), , drop = FALSE]
  S      <- .TuneScore(Points, HasObjective, Direction)
  R      <- S
  S[!Points$Feasible | is.na(S)] <- -Inf
  if (!any(is.finite(S)))
    return(list(x = Points$x[which.max(replace(Points$Slack, !is.finite(Points$Slack), -Inf))],
                Objective = NA_real_, Feasible = FALSE, Status = 'infeasible'))
  k    <- which.max(S)
  n    <- nrow(Points)
  Cand <- c(k - 1, k + 1)
  Cand <- Cand[Cand >= 1 & Cand <= n]
  Cand <- Cand[!Points$Feasible[Cand] & is.finite(R[Cand]) & R[Cand] > S[k]]
  if (length(Cand)) {
    j  <- Cand[which.max(R[Cand])]
    ua <- log(Points$x[k])
    ub <- log(Points$x[j])
    sa <- Points$Slack[k]
    sb <- Points$Slack[j]
    w  <- if (is.finite(sb) && sa != sb) sa / (sa - sb) else 0
    w  <- min(max(w, 0), 1)
    return(list(x = exp(ua + w * (ub - ua)), Objective = S[k] + w * (R[j] - S[k]),
                Feasible = TRUE, Status = 'boundary'))
  }
  Status <- if ((k == n || k == 1) && n > 1) 'at_bound' else 'interior'
  list(x = Points$x[k], Objective = S[k], Feasible = TRUE, Status = Status)
}

.TuneConfigure <- function(State) {
  Control <- State$Control
  Grid    <- exp(seq(log(Control$Interval[1]), log(Control$Interval[2]), length.out = Control$nGridConfig))
  Pairs   <- expand.grid(x = Grid, Config = seq_along(State$Configs))[, c('Config', 'x')]
  Points  <- .TuneEvalPairs(State, Pairs, Control$nSimConfig, 'Configure')
  do.call(rbind, lapply(seq_along(State$Configs), \(cfg) {
    P <- Points[Points$Config == cfg, , drop = FALSE]
    A <- .TuneApprox(P, State$HasObjective, Control$Direction)
    data.frame(Config = cfg, Label = .TuneConfigLabel(State$Configs[[cfg]]),
               Value = A$x, Objective = A$Objective, Feasible = A$Feasible, Status = A$Status,
               Slack = max(replace(P$Slack, !is.finite(P$Slack), -Inf)),
               TunedValue = NA_real_, TunedObjective = NA_real_, TunedStatus = NA_character_)
  }))
}

.TuneConstraintTable <- function(State, Point) {
  Rows <- lapply(State$Metrics, \(m) {
    Value <- Point[[m$Name]]
    data.frame(Name = m$Name, Type = m$Type, Value = Value,
               Min = m$Min %||% NA_real_, Max = m$Max %||% NA_real_,
               Slack = if (m$Type == 'constraint') .TuneSlack(Value, m$Min, m$Max) else NA_real_)
  })
  Out         <- do.call(rbind, Rows)
  Out$Binding <- Out$Type == 'constraint' & !is.na(Out$Slack) &
    abs(Out$Slack) <= max(State$Control$TolPM, 0.01)
  Out
}

.TuneValidate <- function(State, Tuned, ValidationList) {
  Rows <- list()
  for (h in names(ValidationList)) {
    MSE <- Project(ValidationList[[h]], MPs = list(Tuned = Tuned), silent = TRUE)
    for (m in State$Metrics) {
      V     <- .TuneMetricMatrix(m, MSE, 'Tuned')[, 'Tuned']
      Value <- if (m$StockSummary == 'worst') {
        V[which.min(.TuneSlack(V, m$Min, m$Max))]
      } else {
        .TuneReduce(V, m$StockSummary)
      }
      Rows[[length(Rows) + 1]] <- data.frame(Hist = h, Name = m$Name, Type = m$Type, Value = Value,
                                             Min = m$Min %||% NA_real_, Max = m$Max %||% NA_real_)
    }
  }
  Out     <- do.call(rbind, Rows)
  Out$Met <- ifelse(Out$Type == 'constraint',
                    (is.na(Out$Min) | Out$Value >= Out$Min) & (is.na(Out$Max) | Out$Value <= Out$Max),
                    NA)
  Out
}

.TuneDryRun <- function(State) {
  Control <- State$Control
  nHist   <- length(unique(unlist(lapply(State$Metrics, `[[`, 'Hists'))))
  nConfig <- length(State$Configs)
  Stage1  <- if (nConfig > 1) nConfig * Control$nGridConfig else 0
  PerTune <- Control$nGrid + Control$MaxExpand * max(2, ceiling(Control$nGrid / 2)) +
    Control$MaxIter * State$nPerRound
  Stage2 <- min(Control$nRefine, nConfig) * PerTune
  Pilot  <- do.call(SetMPArgs, c(list(State$MP), State$Configs[[1]],
                                stats::setNames(list(1), State$TuneArg)))
  Hist1 <- State$HistList[[unique(unlist(lapply(State$Metrics, `[[`, 'Hists')))[1]]]
  Time  <- system.time(Project(Hist1, MPs = list(Pilot = Pilot), silent = TRUE))[['elapsed']]
  Out   <- list(nHist = nHist, nConfig = nConfig, Stage1Projections = Stage1 * nHist,
              Stage2Projections = Stage2 * nHist, SecondsPerProjection = Time,
              EstimatedSeconds = (Stage1 + Stage2) * nHist * Time)
  if (!State$silent)
    cli::cli_inform(c(
      "Dry run: {nConfig} configuration{?s} x {nHist} Hist object{?s}.",
      "*" = "Stage 1: {Out$Stage1Projections} MP projection{?s}; Stage 2: up to {Out$Stage2Projections}.",
      "*" = "About {round(Time, 1)} s per MP projection: up to {round(Out$EstimatedSeconds / 60, 1)} min (less when MPs are projected together or in parallel)."
    ))
  invisible(Out)
}
