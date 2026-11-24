#' Years Where Reference Point Inputs Change
#'
#' Finds the years in which the parameters that determine the equilibrium
#' unfished values or the MSY reference points change. Between these
#' change-point years the parameters, and so the reference points, are
#' constant.
#'
#' By default, [Simulate()] calculates the MSY reference points for the last
#' historical year only (see `RefYears` in [OM()] `Control`), and records an
#' assumption in the log if the parameters affecting them change over time.
#' Use the years returned by `RefChangeYears()` to calculate the MSY reference
#' points for every change-point year, either before simulating:
#'
#' ```
#' OM@Control$RefYears <- RefChangeYears(OM)
#' Hist <- Simulate(OM)
#' ```
#'
#' or for an existing [hist-class] object:
#'
#' ```
#' Hist@Reference@MSY <- CalcMSY(Hist, Years = RefChangeYears(Hist))
#' ```
#'
#' The equilibrium unfished values (`Hist@Unfished@Equilibrium`) are always
#' calculated for every change-point year (`Type = 'Unfished'`), historical
#' and projection.
#'
#' @param x An [om-class] or [hist-class] object. An `om` is populated first.
#' @param Type `'MSY'` (default) for the parameters affecting the MSY
#'   reference points: the stock parameters affecting the unfished values,
#'   the stock-recruitment parameters, the fleet selectivity, retention,
#'   discard mortality, and weight-at-age, and the relative fishing mortality
#'   among fleets (effort x catchability, unless `OM@RefEffortYears` is set).
#'   `'Unfished'` for the parameters affecting the equilibrium unfished values:
#'   natural mortality, weight, maturity, fecundity, semelparity, `R0`, the
#'   unfished spatial distribution, and sex change.
#' @param Period Years to search: `'All'` (default; historical and
#'   projection), `'Historical'`, or `'Projection'`.
#'
#' @return A numeric vector of calendar years, always including the first year
#'   of `Period`. Attribute `Parameters` is a named list (by year) of the
#'   parameters that change in each year after the first. The object has class
#'   `RefChangeYears` (attributes `Type` and `Period`), which prints a summary
#'   by parameter; use `as.numeric()` for a plain vector.
#'
#' @details
#' Only changes specified in the operating model are detected. Changes made
#' during the projection by a management procedure (e.g., to selectivity or
#' retention) are not.
#'
#' For seasonal models, a change-point year is a calendar year whose seasonal
#' values differ from those of the previous year.
#'
#' @seealso [CalcMSY()], [CalcUnfished_Equilibrium()]
#' @export
RefChangeYears <- function(x, Type = c('MSY', 'Unfished'),
                           Period = c('All', 'Historical', 'Projection')) {
  Type   <- match.arg(Type)
  Period <- match.arg(Period)
  OM <- if (inherits(x, 'hist')) x@OM else x
  .CheckClass(OM, 'om', 'x')
  if (!inherits(x, 'hist'))
    OM <- suppressMessages(Populate(OM, silent = TRUE))

  TimeSteps <- switch(Period,
                      All        = c(Years(OM, 'Historical'), Years(OM, 'Projection')),
                      Historical = Years(OM, 'Historical'),
                      Projection = Years(OM, 'Projection'))
  Out <- .ChangeYears(.RefParArrays(OM, Type, TimeSteps), TimeSteps)
  structure(Out, Type = Type, Period = Period,
            class = c('RefChangeYears', 'numeric'))
}

#' @describeIn RefChangeYears Print the change-point years and, for each
#'   parameter, the years in which it changes.
#' @param ... Not used.
#' @export
print.RefChangeYears <- function(x, ...) {
  Type   <- attr(x, 'Type') %||% 'MSY'
  Period <- attr(x, 'Period') %||% 'All'
  Pars   <- attr(x, 'Parameters') %||% list()
  Yrs    <- as.numeric(x)
  What   <- switch(Type, MSY = 'MSY reference point', Unfished = 'Equilibrium unfished')
  PeriodText <- switch(Period, All = 'historical and projection',
                       Historical = 'historical', Projection = 'projection')

  cli::cli_h3("{What} change-point years ({PeriodText})")
  if (length(Yrs) <= 1L) {
    cli::cli_text("No changes: the parameters are constant from {.val {Yrs}}.")
    return(invisible(x))
  }
  YearText <- .CollapseYearsText(Yrs, MaxRuns = Inf)
  cli::cli_text("{length(Yrs)} change-point years: {YearText}")

  # by parameter: the years in which it changes
  Labels <- unique(unlist(Pars, use.names = FALSE))
  ByPar  <- purrr::map(Labels, \(l) as.numeric(names(Pars)[vapply(Pars, \(p) l %in% p, logical(1))]))
  cli::cli_text("Parameters that change:")
  Items <- vapply(seq_along(Labels), \(i)
    paste0(Labels[i], ': ', .CollapseYearsText(ByPar[[i]], MaxRuns = Inf)), character(1))
  cli::cli_ul(Items)
  if ('relative fishing mortality among fleets' %in% Labels)
    cli::cli_alert_info("Set {.code OM@RefEffortYears} to fix the relative fishing mortality among fleets to the effort in given years.")
  invisible(x)
}

# arrays, by parameter label, of the inputs affecting the unfished values
# (`Type = 'Unfished'`) or the MSY reference points (`Type = 'MSY'`)
.RefParArrays <- function(OM, Type = c('MSY', 'Unfished'), TimeSteps = NULL) {
  Type <- match.arg(Type)
  Out <- list()
  Add <- function(label, x) {
    if (is.array(x) && 'Year' %in% names(dimnames(x)) && dim(x)[match('Year', names(dimnames(x)))] > 1)
      Out[[label]] <<- c(Out[[label]], list(x))
  }
  for (st in seq_along(OM@Stock)) {
    Stock <- OM@Stock[[st]]
    Add('natural mortality', Stock@NaturalMortality@MeanAtAge)
    Add('weight', Stock@Weight@MeanAtAge)
    Add('maturity', Stock@Maturity@MeanAtAge)
    Add('semelparity', Stock@Maturity@Semelparous)
    Add('fecundity', Stock@Fecundity@MeanAtAge)
    Add('R0', Stock@SRR@R0)
    if (Type == 'Unfished')
      Add('unfished spatial distribution', Stock@Spatial@UnfishedDist)
    if (Type == 'MSY') {
      for (p in Stock@SRR@Pars) Add('stock-recruitment parameters', p)
      Fleets <- OM@Fleet[[st]]
      for (Fleet in Fleets) {
        Add('selectivity', Fleet@Selectivity@MeanAtAge)
        Add('retention', Fleet@Retention@MeanAtAge)
        Add('discard mortality', Fleet@DiscardMortality@MeanAtAge)
        Add('fleet weight-at-age', Fleet@WeightFleetRetained)
        Add('fleet weight-at-age', Fleet@WeightFleetSelected)
      }
      if (length(Fleets) > 1 && is.null(OM@RefEffortYears))
        Add('relative fishing mortality among fleets', .FleetShareByYear(Fleets, OM@Seasons))
    }
  }
  if (length(OM@Herm) && !is.null(TimeSteps)) {
    Hazard <- tryCatch(.HermResolvePairs(OM, TimeSteps)$Hazard, error = function(e) list())
    for (h in Hazard) Add('sex change', h)
  }
  Out
}

# relative fishing mortality among fleets by time step (effort x catchability
# as a fraction of the total; seasonal: of the peak-season total within each
# calendar year) [Sim, Year, Fleet]
.FleetShareByYear <- function(Fleets, Seasons = 1) {
  Arr <- tryCatch(purrr::map(Fleets, \(Fleet) {
    ArrayMultiply(Fleet@Effort@Effort, Fleet@Catchability@Efficiency)
  }) |> List2Array('Fleet', pos = 3), error = function(e) NULL)
  if (is.null(Arr) || !'Year' %in% names(dimnames(Arr))) return(NULL)
  Arr <- .Aperm(Arr, c('Sim', 'Year', 'Fleet'))
  Total <- apply(Arr, c(1, 2), sum)
  if (Seasons > 1) {
    CalYear <- floor(as.numeric(colnames(Total)))
    for (i in seq_len(nrow(Total)))
      Total[i, ] <- stats::ave(Total[i, ], CalYear, FUN = max)
  }
  Total[Total <= 0] <- 1
  sweep(Arr, c(1, 2), Total, '/')
}

# calendar years (within `TimeSteps`) where any array in `ParArrays` changes;
# the first year is always included. Attribute `Parameters`: the labels that
# change in each later year.
.ChangeYears <- function(ParArrays, TimeSteps) {
  CalYears <- sort(unique(floor(as.numeric(TimeSteps))))
  Changed <- MakeNamedList(as.character(CalYears[-1]))
  for (label in names(ParArrays)) {
    for (x in ParArrays[[label]]) {
      yrs <- .ArrayChangeYears(x, TimeSteps)
      for (y in as.character(intersect(yrs, CalYears[-1])))
        Changed[[y]] <- union(Changed[[y]], label)
    }
  }
  Changed <- purrr::compact(Changed)
  Out <- sort(c(CalYears[1], as.numeric(names(Changed))))
  attr(Out, 'Parameters') <- Changed
  Out
}

# calendar years in which the values of `x` differ from the previous calendar
# year (seasonal: compared season by season)
.ArrayChangeYears <- function(x, TimeSteps, tol = 1e-6) {
  yd <- match('Year', names(dimnames(x)))
  if (is.na(yd) || dim(x)[yd] < 2) return(numeric(0))
  x <- .ArraySubsetYear(x, TimeSteps)
  Steps <- as.numeric(dimnames(x)[[yd]])
  CalYear <- floor(Steps)
  Cal <- unique(CalYear)
  if (length(Cal) < 2) return(numeric(0))
  m <- matrix(aperm(x, c(seq_along(dim(x))[-yd], yd)), ncol = dim(x)[yd])
  Block <- \(y) m[, CalYear == y, drop = FALSE]
  Diff <- vapply(seq_along(Cal)[-1], \(i) {
    a <- Block(Cal[i]); b <- Block(Cal[i - 1])
    if (!identical(dim(a), dim(b))) return(TRUE)
    d <- abs(a - b) > tol * pmax(abs(a), abs(b), 1e-12)
    isTRUE(any(d[!is.na(d)])) || !identical(is.na(a), is.na(b))
  }, logical(1))
  Cal[-1][Diff]
}

# years as consecutive ranges, e.g. "1990-1995, 2000, 2010-2020"; long lists
# keep the first and last ranges
.CollapseYearsText <- function(Years, MaxRuns = 6) {
  Years <- sort(unique(Years))
  if (!length(Years)) return('')
  Runs <- split(Years, cumsum(c(1, diff(Years) != 1)))
  Text <- vapply(Runs, \(r) if (length(r) == 1) as.character(r) else
    paste0(r[1], '-', r[length(r)]), character(1))
  if (length(Text) > MaxRuns)
    Text <- c(Text[seq_len(MaxRuns - 2)], '...', Text[length(Text)])
  paste(Text, collapse = ', ')
}

# assumption logged by Simulate() when the MSY reference points are calculated
# for the last historical year only, but their inputs change over time
.LogMSYChangeYears <- function(Hist, MSYYears) {
  ChangeYears <- tryCatch(RefChangeYears(Hist, 'MSY'), error = function(e) NULL)
  if (is.null(ChangeYears)) return(Hist)
  Other <- setdiff(ChangeYears, c(min(ChangeYears), MSYYears))
  if (!length(Other)) return(Hist)

  Pars <- attr(ChangeYears, 'Parameters')[as.character(Other)] |> unlist() |> unique()
  Hint <- if ('relative fishing mortality among fleets' %in% Pars)
    " The relative fishing mortality among fleets is from the effort in each year; set {.code OM@RefEffortYears} to fix it to the effort in given years." else ""
  nYr <- length(ChangeYears)
  ParText  <- paste(Pars, collapse = ', ')
  YearText <- .CollapseYearsText(ChangeYears)
  String <- cli::format_inline(
    "MSY reference points were calculated for {.val {MSYYears}} ",
    "(the last historical year) only, but the parameters affecting them ",
    "({ParText}) change over time: they have {nYr} ",
    "change-point years ({YearText}).", Hint,
    " To calculate the reference points for every change-point year, use ",
    "{.code OM@Control$RefYears <- RefChangeYears(OM)} before {.code Simulate(OM)}, ",
    "or {.code Hist@Reference@MSY <- CalcMSY(Hist, Years = RefChangeYears(Hist))} ",
    "for an existing {.cls hist} object."
  )
  .CaptureLog(Hist, String, name = 'MSYChangeYears', type = 'assumption')
}

# values of `x` in `Years` (arrays with a `Year` dimension; others unchanged):
# later years take the most recent earlier value and years before the first
# take the first value
.SubsetYearIfAny <- function(x, Years) {
  if (!is.array(x) || !'Year' %in% names(dimnames(x))) return(x)
  ExtendYears(x, Years, backfill = TRUE) |> .ArraySubsetYear(Years)
}

# time steps (historical and projection) of the calendar years in which the
# inputs to the equilibrium unfished values change
.UnfishedYears <- function(OM) {
  TimeSteps <- c(Years(OM, 'Historical'), Years(OM, 'Projection'))
  CalYears  <- .ChangeYears(.RefParArrays(OM, 'Unfished', TimeSteps), TimeSteps)
  TimeSteps[floor(TimeSteps) %in% CalYears]
}
