#' Total mortality at age for the requested time steps
#'
#' \eqn{Z_a = M_a + \sum_f F^{dead}_{a,f}}, resolved per area.
#'
#' @param Proj A `hist`/`Proj` object.
#' @param st Integer. Stock index.
#' @param x Integer. Simulation index.
#' @param TSIndex Integer vector of time-step indices.
#' @param areas Integer vector of area indices to include.
#'
#' @return `[Age x Area]` when `TSIndex` is length 1, otherwise
#'   `[Age x Year x Area]`. `NULL` when the inputs are unavailable.
#' @keywords internal
.TotalMortalityAtAge <- function(Proj, st, x, TSIndex, areas) {

  M  <- Proj@OM@Stock[[st]]@NaturalMortality@MeanAtAge
  Fd <- Proj@FDeadArea[[st]]
  if (is.null(M) || is.null(Fd)) return(NULL)

  # [Age x nY]
  Mv <- M[pmin(x, dim(M)[1]), , TSIndex, drop = FALSE] |> abind::adrop(1)

  # [Age x nY x Fleet x Area] -> [Age x nY x Area]
  Ft <- Fd[x, , TSIndex, , areas, drop = FALSE] |> abind::adrop(1)
  Ft <- apply(Ft, c(1, 2, 4), sum)

  # Mv recycles over the Area margin: Age and Year are the fastest-varying dims
  Z <- array(as.vector(Ft) + as.vector(Mv), dim = dim(Ft))

  if (length(TSIndex) == 1L) Z <- array(Z, dim = dim(Z)[c(1, 3)])
  Z
}

#' Decay numbers-at-age to an observation point within the time step
#'
#' An index observed part-way through a time step sees the population after the
#' mortality accumulated up to that point, so numbers are decayed by
#' \eqn{\exp(-Z_a \tau)}. `Timing` is a fraction of the *time step*, so in a
#' seasonal model it is within-season - the same convention as
#' `Stock@SpawnTimeFrac`.
#'
#' Applied identically when generating historical and projected indices, so the
#' two are on the same footing and catchability is not left absorbing a timing
#' offset across the projection boundary.
#'
#' @param Number List (per stock) of numbers, `[Age x Area]` for a single time
#'   step or `[Age x Year x Area]` for several.
#' @param Proj A `hist`/`Proj` object.
#' @param stocks Integer vector of stock indices matching `Number`.
#' @param x Integer. Simulation index.
#' @param TSIndex Integer vector of time-step indices, matching `Number`.
#' @param timing Numeric. Fraction of the time step, in `[0, 1)`.
#' @param areas Integer vector of area indices to include.
#'
#' @return `Number`, decayed. Returned unchanged when `timing` is `0`, `NA`, or
#'   the mortality needed to decay it is unavailable.
#' @keywords internal
.DecayNumbersToTiming <- function(Number, Proj, stocks, x, TSIndex, timing, areas) {

  if (length(timing) != 1L || !is.finite(timing) || timing <= 0) return(Number)

  purrr::map2(Number, stocks, function(num, st) {
    Z <- .TotalMortalityAtAge(Proj, st, x, TSIndex, areas)
    if (is.null(Z) || !all(is.finite(Z))) return(num)
    if (!all(dim(Z) == dim(num)))         return(num)
    num * exp(-Z * timing)
  })
}

#' Decay full `[Sim x Age x Year x Area]` numbers to an observation point
#'
#' The all-simulation counterpart of `.DecayNumbersToTiming()`, used when
#' conditioning observation error where every simulation is handled at once.
#' Mortality differs by simulation, so \eqn{Z} is built per simulation.
#'
#' @param Number List (per stock) of `[Sim x Age x Year x Area]` arrays.
#' @param Hist A `hist` object.
#' @param stocks Integer vector of stock indices matching `Number`.
#' @param timing Numeric. Fraction of the time step, in `[0, 1)`.
#'
#' @return `Number`, decayed, or unchanged when `timing` is `0`/`NA` or the
#'   mortality is unavailable.
#' @keywords internal
.DecayNumberListToTiming <- function(Number, Hist, stocks, timing) {

  if (length(timing) != 1L || !is.finite(timing) || timing <= 0) return(Number)

  purrr::map2(Number, stocks, function(num, st) {

    dd <- dim(num)
    if (length(dd) != 4L) return(num)
    ts    <- seq_len(dd[3])
    areas <- seq_len(dd[4])

    Z <- tryCatch(
      vapply(seq_len(dd[1]),
             function(s) .TotalMortalityAtAge(Hist, st, s, ts, areas),
             array(0, dim = dd[-1])),          # [Age x Year x Area x Sim]
      error = function(e) NULL)

    if (is.null(Z) || !all(is.finite(Z))) return(num)

    num * exp(-aperm(Z, c(4, 1, 2, 3)) * timing)
  })
}
