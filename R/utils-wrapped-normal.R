#' Wrapped Normal Distribution
#'
#' Creates a wrapped normal distribution to represent a seasonal recruitment
#' patterns in models with `Seasons > 1`.
#'
#' @param n_seasons Integer. Number of seasons per year (e.g. `12` for
#'   monthly, `4` for quarterly).
#' @param mu Numeric. Peak season (need not be an integer; e.g. `3.5` for a
#'   peak between seasons 3 and 4).
#' @param sigma Numeric. Standard deviation in units of seasons. Smaller
#'   values concentrate recruitment around the peak; larger values spread it
#'   across the year.
#' @param n_wraps Integer. Number of periods summed on each side to approximate
#'   wrapping. Default `5` is sufficient for any reasonable `sigma`.
#'
#' @return A numeric vector of normalised density values of length
#'   `n_seasons`.
#'
#' @examples
#' # Monthly model: peak recruitment March/April, moderate spread
#' pi_m <- WrappedNormal(n_seasons = 12, mu = 3.5, sigma = 1.5)
#' names(pi_m) <- month.abb
#'
#' # Narrow spread — recruitment concentrated in 1-2 seasons
#' pi_m <- WrappedNormal(n_seasons = 12, mu = 3.5, sigma = 0.5)
#'
#' # Wide spread — recruitment across most of the year
#' pi_m <- WrappedNormal(n_seasons = 12, mu = 3.5, sigma = 3)
#'
#' # Quarterly model: peak in Q1
#' pi_m <- WrappedNormal(n_seasons = 4, mu = 1.5, sigma = 0.8)
#'
#' @export
WrappedNormal <- function(n_seasons, mu, sigma, n_wraps = 5) {
  k <- seq(-n_wraps, n_wraps)
  out <- sapply(seq_len(n_seasons), function(xi) {
    sum(dnorm(xi, mean = mu + k * n_seasons, sd = sigma))
  })
  out/sum(out)
}
