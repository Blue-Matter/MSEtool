#' Calculate Historical and Projection Time Values
#'
#' Generates historical and projection time values based on the number of
#' years, projection years, current year, and seasonal structure.
#'
#' @details
#' 
#' * Historical period spans `nYear` years ending at `CurrentYear`
#' * Projection period spans `pYear` years beginning after `CurrentYear`
#' * If `pYear = 0`, only historical values are returned
#' * For sub-annual time units, values are returned as decimal dates
#'
#' @param nYear Integer. Number of historical years.
#' @param pYear Integer. Number of projection years.
#' @param CurrentYear Integer. Final historical year.
#' @param Seasons Integer. Number of seasons per year.
#' @param Period Optional character. One of:
#'   * `"Historical"` or `"H"` for historical only
#'   * `"Projection"` or `"P"` for projection only
#'   * `NULL` (default) for both
#'
#' @return Numeric vector of time values.
#' @examples
#' # Annual
#' CalcYears(nYear = 20,
#'           pYear = 10,
#'           CurrentYear = 2026)
#' 
#' # Sub-Annual
#' CalcYears(nYear = 20,
#'           pYear = 10,
#'           CurrentYear = 2026,
#'           Seasons =2)
#' 
#' @export
CalcYears <- function(nYear, pYear, CurrentYear, Seasons = 1, Period = NULL) {
  
  TimeUnits <- CalcTSUnits(Seasons)
  CalcProj <- pYear > 0
  
  if (TimeUnits == "year") {
    # Special case: non-calendar year indexing
    if (CurrentYear < 1900) {
      hist <- seq(CurrentYear, by = -1, length.out = nYear) |> rev()
      if (CalcProj) {
        proj <- seq(CurrentYear + 1, by = 1, length.out = pYear)
      }
    } else {
      hist <- seq(CurrentYear - nYear + 1, CurrentYear)
      if (CalcProj) {
        proj <- seq(CurrentYear + 1, CurrentYear + pYear)
      }
    }
  } else {
    # sub-annual time steps
    FirstHist <- lubridate::ymd(paste0(CurrentYear - nYear + 1, "-01-01"))
    LastHist  <- lubridate::ymd(paste0(CurrentYear, "-12-31"))
    
    FirstProj <- lubridate::ymd(paste0(CurrentYear + 1, "-01-01"))
    LastProj  <- lubridate::ymd(paste0(CurrentYear + pYear, "-12-31"))
    
    by_map <- list(
      "half-year" = "6 months",
      "quarter"   = "3 months",
      "month"     = "1 month",
      "week"      = "1 week",
      "day"       = "1 day"
    )
    
    hist <- seq(FirstHist, LastHist, by = by_map[[TimeUnits]]) |>
      lubridate::decimal_date() |>
      round(4)
    
    if (CalcProj) {
      proj <- seq(FirstProj, LastProj, by = by_map[[TimeUnits]]) |>
        lubridate::decimal_date() |>
        round(4)
    }
  }
  
  if (is.null(Period)) {
    if (CalcProj) {
      return(c(hist, proj))
    } else {
      return(hist)
    }
  }
  
  if (grepl("H", Period)) {
    return(hist)
  }
  
  if (grepl("P", Period)) {
    if (!CalcProj) return(numeric(0))
    return(proj)
  }
  
  invisible(NULL)
}
