#' Extract Recruitment Time Series
#'
#' Returns recruitment, the number (or biomass) of individuals in the youngest
#' age class, from a [hist-class] or [mse-class] object. Recruits are reported
#' in the time step in which they enter the population, so for stocks whose
#' youngest age class is older than 0 the values are recruits produced by
#' spawning in an earlier year.
#'
#' When applied to an [mse-class] object the historical and projection
#' periods are row-bound and labelled via the `Period` column; historical
#' rows carry `MP = "Historical"`.
#'
#' @param object A [hist-class] or [mse-class] object.
#' @param df Logical. If `TRUE` (default) a tidy `data.frame` is returned.
#'   If `FALSE` an array is returned.
#' @param Units Character. `"Number"` (default) returns the number of recruits.
#'   `"Biomass"` multiplies the number of recruits by the weight-at-age of the
#'   youngest age class.
#' @param byArea Logical. If `TRUE` the `Area` dimension is retained.
#'   Otherwise values are summed over areas.
#' @param Annual Logical. For seasonal models, if `TRUE` recruitment is summed
#'   over the time steps of each complete calendar year and `Year` is the
#'   calendar year. Ignored for models with one season per year. Default
#'   `FALSE`.
#' @param Reduce Logical. If `TRUE` (default) simulation dimensions are
#'   reduced using [ReduceDims()] before conversion to a data frame.
#' @param IncYear Logical. Passed to [ReduceDims()]; controls whether the
#'   year dimension is retained during reduction. Default `FALSE`.
#' @param Extend Logical. If `TRUE`, rows sharing one value across every
#'   simulation are broadcast to all `nSim` simulations via [ExtendSims()],
#'   rather than appearing once with `Sim = 1`. Default `FALSE`. Ignored when
#'   `df = FALSE`.
#' @param silent Logical. If `FALSE` and `Extend = FALSE`, emits a message
#'   when the returned data frame has rows sharing one value across every
#'   simulation, pointing at `Extend = TRUE`. Default `TRUE`. Ignored when
#'   `df = FALSE`.
#'
#' @return
#' * `df = FALSE` — a numeric array `Sim x Stock x Year` (plus `Area` when
#'   `byArea = TRUE`). For [mse-class] objects the array covers the projection
#'   period and has a trailing `MP` dimension.
#' * `df = TRUE` — a `data.frame` (subclass `"recruits.df"`) with columns
#'   `Sim`, `Stock`, `Year`, `Period`, `MP` (MSE only), and optionally `Area`,
#'   plus `Value`, `Variable`, and `Units`.
#'
#' @seealso [Number()], [Biomass()], [ExtendSims()]
#' @export
#' @examples
#' Hist <- Simulate(SingleStockOM)
#' Recruits(Hist)
#' Recruits(Hist, Units = 'Biomass')
#' Recruits(Hist, df = FALSE)
Recruits <- function(object,
                     df      = TRUE,
                     Units   = c('Number', 'Biomass'),
                     byArea  = FALSE,
                     Annual  = FALSE,
                     Reduce  = TRUE,
                     IncYear = FALSE,
                     Extend  = FALSE,
                     silent  = TRUE) {
  Units <- match.arg(Units)
  .CheckClass(object, c('hist', 'mse'), 'object')
  OM <- object@OM

  if (inherits(object, 'hist')) {
    arr <- .CalcRecruits(object@Number, OM, Years(OM, 'Historical'), Units, byArea, Annual)
    if (!df) return(arr)
    out <- .RecruitsArrayToDF(arr, OM, Units, 'Historical', Reduce, IncYear)
    out <- .FinalizeTimeseriesDF(out, OM@nSim, Extend, silent)
    class(out) <- c('recruits.df', class(out))
    return(out)
  }

  projArr <- .CalcRecruits(object@Number, OM, Years(OM, 'Projection'), Units, byArea, Annual)
  if (!df) return(projArr)

  histArr <- .CalcRecruits(object@Hist@Number, OM, Years(OM, 'Historical'), Units, byArea, Annual)
  hist <- .RecruitsArrayToDF(histArr, OM, Units, 'Historical', Reduce, IncYear) |>
    dplyr::mutate(MP = 'Historical')
  proj <- .RecruitsArrayToDF(projArr, OM, Units, 'Projection', Reduce, IncYear)

  out <- dplyr::bind_rows(hist, proj)
  out <- .FinalizeTimeseriesDF(out, OM@nSim, Extend, silent)
  class(out) <- c('recruits.df', class(out))
  out
}

.CalcRecruits <- function(number_list, OM, Years, Units = 'Number', byArea = FALSE, Annual = FALSE) {
  arrs <- purrr::imap(number_list, \(n, st) {
    n <- .SubsetYear(n, Years) |> .FirstAge()
    if (Units == 'Biomass') {
      w <- OM@Stock[[st]]@Weight@MeanAtAge |> .SubsetYear(Years) |> .FirstAge() |>
        AddDimension('Area', pos = 4)
      if ('MP' %in% names(dimnames(n)))
        w <- AddDimension(w, 'MP')
      n <- ArrayMultiply(n, w)
    }
    if (!byArea) n <- SumOverArea(n)
    DropDimension(n, 'Age')
  })

  out <- List2Array(arrs, 'Stock', pos = 2)
  if (Annual && .IsSeasonal(OM))
    out <- .SumWithinCalendarYear(out, OM@Seasons)
  out
}

# Youngest age class; keeps a collapsed (length-1) Age dimension as is.
.FirstAge <- function(arr) {
  if (dim(arr)[match('Age', names(dimnames(arr)))] == 1)
    return(arr)
  abind::asub(arr, 1, match('Age', names(dimnames(arr))), drop = FALSE)
}

.RecruitsArrayToDF <- function(arr, OM, Units, Period, Reduce = TRUE, IncYear = FALSE) {
  if (Reduce)
    arr <- ReduceDims(arr, IncYear = IncYear)

  units <- if (Units == 'Number') .GetNumberUnits(OM) else .GetUnits(OM, 'Biomass')

  Array2DF(arr) |>
    dplyr::mutate(Variable = 'Recruits',
                  Period = Period,
                  Stock = as.character(Stock)) |>
    dplyr::relocate('Sim', 'Stock', 'Year', 'Period') |>
    dplyr::left_join(units, by = 'Stock') |>
    dplyr::mutate(Stock = .MakeFactor(Stock))
}
