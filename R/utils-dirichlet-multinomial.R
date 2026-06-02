#' Dirichlet-Multinomial Sampler
#'
#' Draw a single composition sample from the Dirichlet-Multinomial
#' distribution. The Dirichlet-Multinomial arises as a multinomial draw whose
#' probability vector is itself drawn from a Dirichlet distribution, producing
#' compositions that are overdispersed relative to a standard multinomial.
#'
#' @param n Integer. Total count (sample size) for the multinomial draw.
#' @param alpha Numeric vector of positive Dirichlet concentration parameters,
#'   length `nBin`. The relative magnitudes determine the expected composition.
#'
#' @details
#' ## Model
#'
#' Given concentration vector \eqn{\boldsymbol{\alpha}} and total count
#' \eqn{n}:
#'
#' \deqn{\mathbf{p} \sim \mathrm{Dirichlet}(\boldsymbol{\alpha})}
#' \deqn{\mathbf{y} \sim \mathrm{Multinomial}(n,\, \mathbf{p})}
#'
#' The marginal distribution of \eqn{\mathbf{y}} is Dirichlet-Multinomial
#' with mean \eqn{n \cdot \boldsymbol{\alpha} / \sum \boldsymbol{\alpha}} and
#' variance exceeding the multinomial by a factor of
#' \eqn{(n + \sum \boldsymbol{\alpha}) / (1 + \sum \boldsymbol{\alpha})}.
#'
#' The Dirichlet draw is obtained via the standard gamma representation:
#' independent \eqn{\mathrm{Gamma}(\alpha_b, 1)} draws normalised to sum
#' to 1.
#'
#' ## Relationship to `ESS` and `Theta`
#'
#' In the composition observation model (see [CompObs()]), the concentration
#' vector is constructed as:
#'
#' \deqn{\alpha_b = \mathrm{ESS} \cdot \Theta \cdot q_b \cdot \exp(\mathrm{Shift}_b)}
#'
#' where \eqn{\mathbf{q}} is the vector of OM-predicted proportions. Passing
#' this as `alpha` and `SampleSize` as `n` recovers the full composition
#' observation model.
#'
#' - **`Theta = 1`, no `Shift`**: recovers a near-multinomial draw (Dirichlet
#'   variance shrinks as `ESS` grows).
#' - **`Theta < 1`**: shrinks `alpha`, increasing overdispersion.
#' - **`Shift` non-`NULL`**: tilts the expected composition bin-by-bin on the
#'   log-concentration scale before drawing.
#'
#' @return Integer vector of length `nBin` summing to `n`.
#'
#' @seealso [CompObs()], [GenHistData_AgeComp()], [GenHistData_SizeComp()]
#' @importFrom stats rgamma
#' @examples
#' # Near-multinomial draw (high ESS, Theta = 1, no shift)
#' q <- c(0.1, 0.3, 0.4, 0.2)
#' alpha <- 200 * 1 * q
#' rDirichletMultinomial(n = 500, alpha = alpha)
#'
#' # Overdispersed draw (low Theta)
#' alpha_od <- 200 * 0.3 * q
#' rDirichletMultinomial(n = 500, alpha = alpha_od)
#'
#' # With shift — tilts expected composition toward older ages
#' shift <- c(-0.5, -0.2, 0.2, 0.5)
#' alpha_shifted <- 200 * 1 * q * exp(shift)
#' rDirichletMultinomial(n = 500, alpha = alpha_shifted)
#'
#' @export
rDirichletMultinomial <- function(n, alpha) {
  p <- stats::rgamma(length(alpha), shape = alpha, rate = 1)
  p <- p / sum(p)
  as.integer(rmultinom(1, size = n, prob = p))
}

