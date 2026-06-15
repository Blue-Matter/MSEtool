#' Set Seasonal R0 Using a Wrapped Normal Distribution
#'
#' Distributes annual unfished recruitment (`AnnualR0`) across seasons using a
#' wrapped normal distribution, producing an `nSim x nSeason` matrix of
#' per-season R0 values that sum to `AnnualR0` for each simulation.
#'
#' Seasonal recruitment is controlled entirely by `SRR@R0`: at unfished
#' equilibrium the stock-recruit relationship returns exactly `R0[m]` recruits
#' in season `m`. Setting `R0[m] = pi_m * AnnualR0` via this function is
#' therefore both necessary and sufficient to produce the target seasonal
#' recruitment pattern. Optionally call [AdjustSeasonalFecundity()] afterwards
#' to also concentrate reported seasonal spawning production in the same
#' seasons, reflecting the biological reality that fish only spawn during
#' certain months.
#'
#' @param AnnualR0 Numeric. Annual unfished recruitment. Accepted forms:
#'   a scalar (same value for all simulations), a numeric vector of length
#'   `nSim`, or an `nSim x 1` matrix. Per-season values sum to `AnnualR0`
#'   for each simulation.
#' @param Seasons Integer. Number of seasons per year (must match `OM@Seasons`).
#' @param PeakSeason Numeric. Season of peak recruitment. Doesn't need to be an
#'   integer; e.g. `3.5` places the peak midway between seasons 3 and 4.
#'   Passed as `mu` to [WrappedNormal()].
#' @param Sigma Numeric. Spread of recruitment around the peak, in units of
#'   seasons. Smaller values concentrate recruitment; larger values spread it
#'   across the year. Passed as `sigma` to [WrappedNormal()].
#' @param Years Optional numeric vector of year labels, e.g. from
#'   `Years(OM, 'Historical')`. If provided, the `Year` dimension of the
#'   returned matrix is named using the first `Seasons` elements
#'   (`Years[1:Seasons]`). If `NULL` (default), dimension names are omitted.
#'
#' @return An `nSim x nSeason` numeric matrix of per-season R0 values.
#'   If `Years` is supplied, `dimnames = list(Sim = 1:nSim, Year = Years[1:Seasons])`.
#'
#' @seealso [WrappedNormal()], [AdjustSeasonalFecundity()]
#'
#' @export
SetSeasonalR0 <- function(AnnualR0, Seasons, PeakSeason, Sigma, Years = NULL) {
  if (is.matrix(AnnualR0)) {
    r0_vec <- as.numeric(AnnualR0[, 1])
  } else {
    r0_vec <- as.numeric(AnnualR0)
  }
  nSim <- length(r0_vec)

  wn  <- WrappedNormal(n_seasons = Seasons, mu = PeakSeason, sigma = Sigma)
  out <- outer(r0_vec, wn)

  if (!is.null(Years)) {
    dimnames(out) <- list(Sim = seq_len(nSim), Year = Years[seq_len(Seasons)])
  }

  out
}
