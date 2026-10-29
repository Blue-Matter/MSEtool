
#' Calculate Age Classes
#'
#' Computes the numeric vector of age classes (in years) from an [Ages()]
#' object, respecting sub-annual time steps.
#'
#' @param Ages An [Ages()] object with slots `MinAge`, `MaxAge`, and `Units`
#'   set.
#'
#' @details
#' Age classes are always expressed in years, regardless of the time unit
#' used to define `MinAge` and `MaxAge`. `Units` controls the within-year
#' resolution via `CalcSeasons()`, which maps unit strings to a seasons
#' divisor:
#'
#' | `Units` | Seasons | Step size (yr) |
#' |---|---|---|
#' | `"year"` | 1 | 1 |
#' | `"half-year"` | 2 | 0.5 |
#' | `"quarter"` | 4 | 0.25 |
#' | `"month"` | 12 | 0.0833 |
#' | `"week"` | 52 | 0.0192 |
#'
#' The sequence runs from `MinAge / Seasons` to `MaxAge / Seasons` in steps
#' of `1 / Seasons`, rounded to three decimal places.
#'
#' Returns `NULL` if `Units`, `MinAge`, or `MaxAge` are not set.
#'
#' @return A numeric vector of age classes in years, or `NULL` if any of
#'   `Ages@Units`, `Ages@MinAge`, or `Ages@MaxAge` are empty.
#'
#' @examples
#' # Annual age classes 0–5
#' CalcAgeClasses(Ages(MinAge = 0, MaxAge = 5, Units = "year"))
#' #> [1] 0 1 2 3 4 5
#'
#' # Quarterly age classes 0–1
#' CalcAgeClasses(Ages(MinAge = 0, MaxAge = 4, Units = "quarter"))
#' #> [1] 0.00 0.25 0.50 0.75 1.00
#'
#' @seealso [Ages()], [ages-class], [PopulateStock()]
#' @export
CalcAgeClasses <- function(Ages) {
  if (!length(Ages@Units))
    return(NULL)
  # always in years 
  if (!length(Ages@MaxAge) || !length(Ages@MinAge))
    return(NULL)
  Seasons <- CalcSeasons(Ages@Units)
  seq(from=Ages@MinAge/Seasons, by=1/Seasons, to=Ages@MaxAge/Seasons) |>
    round(3)
}
