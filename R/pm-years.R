#' Years in Which the Management Procedures Are Active
#'
#' The calendar years of the projection period in which the management
#' procedures (MPs) are active (from `OM@MPStartYear`), or a window of them,
#' for the `Years` argument of the [PM] functions.
#'
#' The years are the complete calendar years of the projection period from
#' `OM@MPStartYear` (all projection years if `MPStartYear` is `NULL`). The
#' first `Skip` years are dropped, and `Window` selects:
#'
#' - `"all"`: all the years.
#' - `"first"`: the first `n` years.
#' - `"last"`: the last `n` years.
#' - `"middle"`: the years between the first `n` and the last `n` years.
#'
#' @param object An [om-class], [hist-class], or [mse-class] object, or a
#'   `list` of [mse-class] objects (the first is used).
#' @param Window Character. The window of years: `"all"` (default),
#'   `"first"`, `"last"`, or `"middle"`. See Details.
#' @param n Integer. The number of years of the `"first"`, `"last"`, and
#'   `"middle"` windows. Default `10`.
#' @param Skip Integer. The number of years to drop from the start before
#'   applying `Window`. Default `0`.
#'
#' @return A numeric vector of calendar years.
#'
#' @examples
#' \dontrun{
#' PMYears(MSE)                      # all years the MPs are active
#' PMYears(MSE, 'first', n = 10)     # the first 10
#' PMYears(MSE, 'middle', n = 10)    # between the first 10 and the last 10
#' PMYears(MSE, 'last', n = 1)       # the terminal year
#' PMYears(MSE, Skip = 5)            # all but the first 5
#' PM_Status(MSE, Years = PMYears(MSE, 'last', n = 15))
#' }
#'
#' @seealso [PM]
#' @export
PMYears <- function(object, Window = c('all', 'first', 'last', 'middle'), n = 10, Skip = 0) {
  Window <- match.arg(Window)
  if (is.list(object) && !isS4(object))
    object <- object[[1]]
  .CheckClass(object, c('om', 'hist', 'mse'), 'object')
  OM <- if (methods::is(object, 'om')) object else object@OM

  Yrs <- .CompleteCalendarYears(Years(OM, 'Projection'), OM@Seasons)
  if (!is.null(OM@MPStartYear))
    Yrs <- Yrs[Yrs >= OM@MPStartYear]
  if (Skip >= length(Yrs))
    cli::cli_abort("{.arg Skip} = {Skip} leaves no years: the MPs are active in {length(Yrs)} year{?s}.")
  if (Skip > 0)
    Yrs <- Yrs[-seq_len(Skip)]

  switch(Window,
         all    = Yrs,
         first  = utils::head(Yrs, n),
         last   = utils::tail(Yrs, n),
         middle = Yrs[!seq_along(Yrs) %in% c(utils::head(seq_along(Yrs), n),
                                             utils::tail(seq_along(Yrs), n))])
}

.CompleteCalendarYears <- function(Years, Seasons = 1) {
  cal <- .CalendarYear(Years)
  Yrs <- unique(cal)
  Yrs[tabulate(match(cal, Yrs), length(Yrs)) == max(1, Seasons)]
}
