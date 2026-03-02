#' Convert Between Steepness and Compensation Ratio
#'
#' Convert between the Beverton-Holt stock-recruitment steepness (h) and
#' compensation ratio (CR) parameterizations.
#'
#' @param CR Numeric. Compensation ratio (see Details).
#' @param h Numeric. Steepness value (see Details). Must be in the range
#'   (0.2, 1), exclusive.
#'
#' @return Numeric. `CR2h()` returns steepness (h) bounded between 0.2 and 1;
#'   `h2CR()` returns the compensation ratio (CR).
#'
#' @details
#' **Steepness (h)** is the fraction of unfished recruitment obtained when the
#' spawning biomass is reduced to 20% of its unfished level. It is bounded
#' between 0.2 (highly depensatory) and 1.0 (density-independent recruitment).
#'
#' **Compensation ratio (CR)** is the ratio of recruits-per-spawner at the
#' origin of the stock-recruitment curve relative to recruits-per-spawner at
#' unfished equilibrium. Higher values indicate stronger density-dependent
#' compensation. CR is bounded between 1 (no compensation) and infinity.
#'
#' The relationships between the two parameters are:
#' \deqn{h = \frac{CR}{CR + 4} \qquad CR = \frac{4h}{1 - h}}
#'
#' @export
#' @examples
#' CR2h(5)
#' h2CR(0.7)
CR2h <- function(CR) {
  CR/(CR+4)
}


#' @rdname CR2h
#' @export
h2CR <- function(h) {
  (4*h)/(1-h) 
}