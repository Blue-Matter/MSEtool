#' Time Steps of an Operating Model
#'
#' Functions for the historical and projection time steps:
#'
#' - `CalcYears()` generates a numeric vector of time step values from
#'   explicit parameters.
#' - `Years()` extracts time step values from an [om-class], [hist-class], or
#'   [mse-class] object, or a list of such objects, by delegating to
#'   `CalcYears()`. For other objects with a `Years` slot (e.g. [data-class],
#'   [pm-class], and the observation components) it returns that slot.
#' - `nYear()`, `pYear()`, `CurrentYear()`, and `Seasons()` access and
#'   replace the number of historical and projection years, the final
#'   historical year, and the number of seasons per year.
#'
#' @details
#' The historical period spans `nYear` time steps ending at `CurrentYear`.
#' The projection period spans `pYear` time steps beginning after
#' `CurrentYear`. If `pYear = 0` or `Period = "H"`, only historical values
#' are returned.
#'
#' For sub-annual seasons (`Seasons > 1`), time steps are expressed as
#' decimal dates (e.g. `2020.5` for mid-2020) using
#' [lubridate::decimal_date()]. Supported resolutions are half-year, quarter,
#' month, week, and day, as determined by [CalcTSUnits()].
#'
#' When `CurrentYear < 1900`, a non-calendar index is used and years are
#' counted backwards from `CurrentYear` rather than mapped to real calendar
#' years.
#'
#' `Period` accepts partial matches to `"Historical"` or `"Projection"` (e.g.
#' `"H"`, `"Hist"`, `"Proj"`). Matching is case-insensitive. `NULL` (default)
#' returns all time steps.
#'
#' @param nYear Integer. Number of historical time steps.
#' @param pYear Integer. Number of projection time steps.
#' @param CurrentYear Integer. Final historical year (or index if
#'   `CurrentYear < 1900`).
#' @param Seasons Integer. Number of seasons per year. Default `1` (annual).
#' @param Period Character or `NULL`. Controls which time steps are returned.
#'   See Details.
#' @param x An [om-class], [hist-class], or [mse-class] object, or a list of
#'   such objects. `Years()`, `nYear()`, `CurrentYear()`, and `Seasons()` also
#'   accept a [data-class] object or a list of them.
#'
#' @return
#' - `CalcYears()` and `Years()` return a numeric vector of time step values,
#'   or for `Years()` applied to a list, a list of such vectors.
#' - `nYear()`, `pYear()`, `CurrentYear()`, and `Seasons()` return the value
#'   of the slot of an [om-class] object, or of the embedded `OM` of a
#'   [hist-class] or [mse-class] object.
#' - For a [data-class] object, `nYear()` returns the number of historical
#'   years up to `YearLH`, `CurrentYear()` returns `YearLH`, and `Seasons()`
#'   returns the `Seasons` slot. For a list of [data-class] objects they return
#'   a single value when it is the same for every object, and otherwise a list.
#'   `pYear()` gives an error for [data-class] objects; see [ProjectionYear()].
#' - Replacement functions return `x` with the slot updated. `nYear`, `pYear`,
#'   `CurrentYear`, and `Seasons` can only be replaced in [om-class] objects.
#'
#' @examples
#' # Annual time steps
#' CalcYears(nYear=20, pYear=10, CurrentYear=2026)
#'
#' # Semi-annual time steps
#' CalcYears(nYear=20, pYear=10, CurrentYear=2026, Seasons=2)
#'
#' # Historical period only
#' CalcYears(nYear=20, pYear=10, CurrentYear=2026, Period="H")
#'
#' @seealso [CalcTSUnits()], [om-class]
#' @name Years
#' @export
CalcYears <- function(nYear, pYear, CurrentYear, Seasons=1, Period=NULL) {

  key <- paste(nYear, pYear, CurrentYear, Seasons, Period %||NA% "NULL", sep = "|")
  cached <- .CalcYearsCache[[key]]
  if (!is.null(cached))
    return(cached)

  result <- .CalcYearsUncached(nYear, pYear, CurrentYear, Seasons, Period)
  .CalcYearsCache[[key]] <- result
  result
}

.CalcYearsCache <- new.env(parent = emptyenv())

.CalcYearsUncached <- function(nYear, pYear, CurrentYear, Seasons=1, Period=NULL) {

  period_resolved <- NULL
  if (!is.null(Period)) {
    valid   <- c("Historical", "Projection")
    matched <- valid[startsWith(tolower(valid), tolower(Period))]
    if (length(matched) == 0)
      cli::cli_abort(c(
        '{.val {Period}} does not partially match a valid `Period`.',
        'i' = 'Must partially match {.val {"Historical"}} or {.val {"Projection"}}, or be `NULL`.'
      ))
    if (length(matched) > 1)
      cli::cli_abort(c(
        '{.val {Period}} is ambiguous \u2014 matches {.val {matched}}.',
        'i' = 'Provide a longer string to disambiguate.'
      ))
    period_resolved <- matched
  }
  
  TimeUnits <- CalcTSUnits(Seasons)
  CalcProj  <- pYear > 0
  
  if (TimeUnits == 'year') {
    if (CurrentYear < 1900) {
      hist <- rev(seq(CurrentYear, by=-1, length.out=nYear))
      proj <- if (CalcProj) seq(CurrentYear + 1, by=1, length.out=pYear)
    } else {
      hist <- seq(CurrentYear - nYear + 1, CurrentYear)
      proj <- if (CalcProj) seq(CurrentYear + 1, CurrentYear + pYear)
    }
  } else {
    by_map <- c(
      "half-year" = "6 months",
      "quarter"   = "3 months",
      "month"     = "1 month",
      "week"      = "1 week",
      "day"       = "1 day"
    )
    to_decimal <- function(start_yr, end_yr) {
      end_yr <- round(end_yr)
      seq(
        lubridate::ymd(paste0(start_yr, '-01-01')),
        lubridate::ymd(paste0(end_yr,   '-12-31')),
        by = by_map[[TimeUnits]]
      ) |> lubridate::decimal_date() |> round(4)
    }
    hist <- to_decimal(CurrentYear - nYear + 1, CurrentYear)
    proj <- if (CalcProj) to_decimal(start_yr = CurrentYear + 1, end_yr = CurrentYear + pYear)
    proj <- proj[seq_len(pYear * Seasons)]
  }
  
  switch(
    period_resolved %||NA% "All",
    "All"        = if (CalcProj) c(hist, proj) else hist,
    "Historical" = hist,
    "Projection" = if (CalcProj) proj else numeric(0)
  )
}

#' @rdname Years
#' @export
Years <- function(x, Period=NULL) {
  if (isS4(x)) {
    if (!methods::.hasSlot(x, 'nYear') && methods::.hasSlot(x, 'Years'))
      return(x@Years)
    if (inherits(x, 'mse') || inherits(x, 'hist'))
      x <- x@OM
    return(CalcYears(x@nYear, x@pYear, x@CurrentYear, x@Seasons, Period))
  }
  if (is.list(x))
    return(purrr::map(x, Years, Period))
}

#' @param value The value to assign: a numeric vector of years for `Years<-`,
#'   or a single number for the other replacement functions.
#' @rdname Years
#' @export
`Years<-` <- function(x, value) {
  .AssignSlot(x, value, 'Years')
}

#' @rdname Years
#' @export
nYear <- function(x) {
  if (inherits(x, "data") || .IsDataList(x))
    return(.DataValue(x, \(d) {
      if (!length(d@Years))
        return(0L)
      YearLH <- d@YearLH %||% floor(max(d@Years, -Inf))
      length(unique(floor(d@Years[d@Years < YearLH + 1])))
    }))
  .IsHist(x, "nYear")
}

#' @rdname Years
#' @export
`nYear<-` <- function(x, value) .AssignOMSlot(x, value, "nYear", "struct")

#' @rdname Years
#' @export
pYear <- function(x) {
  if (inherits(x, "data") || .IsDataList(x))
    cli::cli_abort(c("{.code pYear} is not stored in {.cls data} objects",
                     "i" = "Use {.fn ProjectionYear} for the number of projection time steps in the data"),
                   call = NULL)
  .IsHist(x, "pYear")
}

#' @rdname Years
#' @export
`pYear<-` <- function(x, value) .AssignOMSlot(x, value, "pYear", "struct")

#' @rdname Years
#' @export
CurrentYear <- function(x) {
  if (inherits(x, "data") || .IsDataList(x))
    return(.DataValue(x, \(d) d@YearLH))
  .IsHist(x, "CurrentYear")
}

#' @rdname Years
#' @export
`CurrentYear<-` <- function(x, value) .AssignOMSlot(x, value, "CurrentYear", "struct")

#' @rdname Years
#' @export
Seasons <- function(x) {
  if (.IsDataList(x))
    return(.DataValue(x, \(d) d@Seasons))
  .IsHist(x, "Seasons")
}

#' @rdname Years
#' @export
`Seasons<-` <- function(x, value) .AssignOMSlot(x, value, "Seasons", "struct")
