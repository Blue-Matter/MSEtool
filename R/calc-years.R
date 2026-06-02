#' Calculate or Extract Time Step Values
#'
#' Two related functions for working with historical and projection time steps:
#'
#' - `CalcYears()` generates a numeric vector of time step values from
#'   explicit parameters.
#' - `Years()` extracts time step values from an [om-class], [hist-class], or
#'   [mse-class] object, or a list of such objects, by delegating to
#'   `CalcYears()`.
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
#'   such objects. (`Years()` only.)
#'
#' @return A numeric vector of time step values, or for `Years()` applied to
#'   a list, a list of such vectors.
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
    period_resolved %||% "All",
    "All"        = if (CalcProj) c(hist, proj) else hist,
    "Historical" = hist,
    "Projection" = if (CalcProj) proj else numeric(0)
  )
}

#' @rdname Years
#' @export
Years <- function(x, Period=NULL) {
  if (isS4(x)) {
    if (inherits(x, 'data'))
      return(x@Years)
    if (inherits(x, 'mse') || inherits(x, 'hist'))
      x <- x@OM
    return(CalcYears(x@nYear, x@pYear, x@CurrentYear, x@Seasons, Period))
  }
  if (is.list(x))
    return(purrr::map(x, Years, Period))
}
